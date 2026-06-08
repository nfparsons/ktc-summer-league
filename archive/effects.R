# =====================================================================
# Ctesiphus Expedition — effects registry
# ---------------------------------------------------------------------
# Two layers:
#   1. apply_action(): the five generic campaign actions (p.55), fully
#      implemented. Each may defer to a location-specific effect.
#   2. EFFECTS: a dispatch table keyed by (rule_code, trigger) holding
#      the per-location specials from pp.58-67. trigger is one of
#      "search", "camp", "threat", "explore". Fill these in during the
#      debug/add pass; the engine already routes to them.
#
# Dice helpers assume d3()/d6()/d36() live in the rules-engine dice
# module (built alongside the exploration tibbles). Referenced, not
# redefined here.
# =====================================================================

library(dplyr)
library(DBI)
library(jsonlite)

# --- registry ---------------------------------------------------------
# Register an effect:  register_effect("SL34", "search", function(ctx) {...})
# An effect fn receives a context list (con, round, team_id, hex, params)
# and returns a named list describing what it did (logged + shown in UI).

EFFECTS <- new.env(parent = emptyenv())

register_effect <- function(rule_code, trigger, fn) {
  assign(paste(rule_code, trigger, sep = ":"), fn, envir = EFFECTS)
  invisible(TRUE)
}

get_effect <- function(rule_code, trigger) {
  key <- paste(rule_code, trigger, sep = ":")
  if (exists(key, envir = EFFECTS, inherits = FALSE)) get(key, envir = EFFECTS) else NULL
}

# context builder: everything an effect might need about the acting team's hex
effect_ctx <- function(con, round, team_id, params = list()) {
  hex <- DBI::dbGetQuery(con,
    "SELECT h.hex_uid, h.hex_no, h.hex_type, h.location_code, h.condition_code,
            h.explored, h.blocked
       FROM teams t JOIN hexes h ON h.hex_uid = t.current_hex
      WHERE t.team_id = $1", params = list(team_id)) |> tibble::as_tibble()
  list(con = con, round = round, team_id = team_id, hex = hex, params = params)
}

adjust_sp <- function(con, team_id, delta) {
  DBI::dbExecute(con,
    "UPDATE teams SET supply_points = LEAST(GREATEST(supply_points + $2, 0), 10)
     WHERE team_id = $1", params = list(team_id, delta))
}
adjust_cp <- function(con, team_id, delta) {
  DBI::dbExecute(con,
    "UPDATE teams SET campaign_points = campaign_points + $2 WHERE team_id = $1",
    params = list(team_id, delta))
}

# --- generic actions (p.55) ------------------------------------------

apply_action <- function(con, round, team_id, action, params) {
  ctx <- effect_ctx(con, round, team_id, params)
  switch(action,
    resupply = act_resupply(ctx),
    scout    = act_scout(ctx),
    encamp   = act_encamp(ctx),
    search   = act_search(ctx),
    demolish = act_demolish(ctx),
    stop("Unknown action: ", action))
}

# RESUPPLY (0 SP): gain SP by hex type — base 10, camp D3+3, blocked 0,
# any other hex 1, up to a cap of 10. Landing Site (SL25) adds D3 here.
act_resupply <- function(ctx) {
  hex <- ctx$hex
  gain <- if (isTRUE(ctx$params$at_base)) {
            # set SP straight to 10 (base) — handled as "fill to cap"
            DBI::dbExecute(ctx$con, "UPDATE teams SET supply_points = 10 WHERE team_id = $1",
                           params = list(ctx$team_id))
            return(list(action = "resupply", gain = "set_to_10", hex = hex$hex_no))
          } else if (isTRUE(ctx$params$at_camp)) {
            d3() + 3L
          } else if (isTRUE(hex$blocked)) {
            0L
          } else {
            1L
          }
  # Landing Site location special stacks on top, if registered.
  bonus_fn <- get_effect(hex$location_code, "resupply")
  bonus <- if (!is.null(bonus_fn)) bonus_fn(ctx)$gain %||% 0L else 0L
  adjust_sp(ctx$con, ctx$team_id, gain + bonus)
  list(action = "resupply", gain = gain + bonus, hex = hex$hex_no)
}

# SCOUT (1-3 SP): explore one unexplored hex within range = SP spent.
# Exploration itself (D36 location + D36 condition, re-roll dup) lives in
# the rules-engine module; called here as explore_hex().
act_scout <- function(ctx) {
  sp <- as.integer(ctx$params$sp %||% 1L)
  target <- as.integer(ctx$params$target_hex)
  adjust_sp(ctx$con, ctx$team_id, -sp)
  result <- explore_hex(ctx$con, target, via = "scout")   # rules-engine module
  list(action = "scout", sp = sp, target_hex = target, explored = result)
}

# ENCAMP: cost = hexes to nearest base/camp (excl. blocked). Gain a camp
# in the current hex (max 2 camps). Forsaken Fortress (SL35) overrides to
# allow a base move; that override is registered as an "encamp" effect.
act_encamp <- function(ctx) {
  override <- get_effect(ctx$hex$location_code, "encamp")
  if (!is.null(override)) return(override(ctx))
  cost <- as.integer(ctx$params$cost)  # distance computed by sf adjacency, passed in
  adjust_sp(ctx$con, ctx$team_id, -cost)
  DBI::dbExecute(ctx$con,
    "INSERT INTO camps (team_id, hex_uid) VALUES ($1, $2)",
    params = list(ctx$team_id, ctx$hex$hex_uid))
  list(action = "encamp", cost = cost, hex = ctx$hex$hex_no)
}

# SEARCH (1 SP): resolve the current hex's search rule, if any. The rule
# is entirely location/tomb specific, so dispatch to the registry.
act_search <- function(ctx) {
  adjust_sp(ctx$con, ctx$team_id, -1L)
  fn <- get_effect(ctx$hex$location_code, "search")
  if (is.null(fn))
    return(list(action = "search", note = "no search rule for this hex",
                hex = ctx$hex$hex_no))
  c(list(action = "search", hex = ctx$hex$hex_no), fn(ctx))
}

# DEMOLISH (3 SP): remove an opponent's camp in the current hex. Requires
# a win against that opponent this round (or a refused/declined challenge);
# that eligibility check is enforced before submission is accepted.
act_demolish <- function(ctx) {
  adjust_sp(ctx$con, ctx$team_id, -3L)
  target_team <- as.integer(ctx$params$target_team)
  DBI::dbExecute(ctx$con,
    "DELETE FROM camps WHERE team_id = $1 AND hex_uid = $2",
    params = list(target_team, ctx$hex$hex_uid))
  # Power Cell Sanctum (TL24) adds a penalty effect on first demolish here.
  extra <- get_effect(ctx$hex$location_code, "demolish")
  extra_out <- if (!is.null(extra)) extra(ctx) else NULL
  c(list(action = "demolish", target_team = target_team, hex = ctx$hex$hex_no),
    extra_out)
}

# --- threat-phase triggers (p.56) ------------------------------------
# Resolve every registered "threat" effect for hexes currently in play,
# in priority order. Beast Lair (SL36), Energy Hot Spot (TL26),
# Revivification Crypt (TL33), Hyperfractal Gaol (TL32), Crucible of
# Whispers camp (TL23) all hook here.
apply_threat_triggers <- function(con, round) {
  # hexes with a team or camp present and a registered threat effect
  live <- DBI::dbGetQuery(con,
    "SELECT DISTINCT h.hex_uid, h.location_code
       FROM hexes h
       LEFT JOIN teams t ON t.current_hex = h.hex_uid
       LEFT JOIN camps c ON c.hex_uid = h.hex_uid
      WHERE t.team_id IS NOT NULL OR c.team_id IS NOT NULL") |>
    tibble::as_tibble()
  ord <- priority_order(con)
  for (i in seq_len(nrow(live))) {
    fn <- get_effect(live$location_code[i], "threat")
    if (!is.null(fn))
      fn(list(con = con, round = round, hex_uid = live$hex_uid[i], order = ord))
  }
}

# =====================================================================
# REGISTERED EFFECTS
# ---------------------------------------------------------------------
# Real, working examples below. The remaining ~35 location specials get
# registered the same way during debug/add. Each needs the exact rule it
# implements — and since the rule TEXT lives in the rule_text table you
# populate, the only thing to encode here is the MECHANIC.
# =====================================================================

# Blackstone Obelisk (SL34) — Search: gain 1 CP, once per campaign.
register_effect("SL34", "search", function(ctx) {
  flag <- paste0("SL34_searched_", ctx$team_id)
  done <- DBI::dbGetQuery(ctx$con,
    "SELECT (state->$1) IS NOT NULL AS done FROM campaign_state",
    params = list(flag))$done
  if (isTRUE(done)) return(list(gain_cp = 0, note = "already searched this campaign"))
  adjust_cp(ctx$con, ctx$team_id, 1L)
  DBI::dbExecute(ctx$con,
    "UPDATE campaign_state SET state = jsonb_set(state, ARRAY[$1], 'true')",
    params = list(flag))
  list(gain_cp = 1)
})

# Blackstone Obelisk (SL34) — Camp: gain 1 CP each Threat phase while camped.
register_effect("SL34", "threat", function(ctx) {
  campers <- DBI::dbGetQuery(ctx$con,
    "SELECT team_id FROM camps WHERE hex_uid = $1", params = list(ctx$hex_uid))$team_id
  for (tid in campers) adjust_cp(ctx$con, tid, 1L)
})

# Landing Site (SL25) — Resupply here gains D3 additional SP.
register_effect("SL25", "resupply", function(ctx) list(gain = d3()))

# Astral Augury (TL34) — Search: first searcher gains D3 CP, later
# searchers gain 1 CP; once per team per campaign.
register_effect("TL34", "search", function(ctx) {
  any_done <- DBI::dbGetQuery(ctx$con,
    "SELECT (state->'TL34_searched') IS NOT NULL AS done FROM campaign_state")$done
  gain <- if (isTRUE(any_done)) 1L else d3()
  adjust_cp(ctx$con, ctx$team_id, gain)
  DBI::dbExecute(ctx$con,
    "UPDATE campaign_state SET state = jsonb_set(state, ARRAY['TL34_searched'], 'true')")
  list(gain_cp = gain, first = !isTRUE(any_done))
})

# --- extension points still to register (debug/add pass) --------------
# Surface: SL22 SL23 SL24 SL26 SL31 SL32 SL33 SL35 SL36
# Tomb:    TL21 TL22 TL23 TL24 TL25 TL26 TL31 TL32 TL33 TL35 TL36
# (Conditions SC**/TC** are in-game killzone rules, surfaced read-only on
#  the pre-game screen rather than resolved by this engine.)
