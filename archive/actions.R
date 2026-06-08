#' Action-phase engine for the Ctesiphus Expedition (rulebook p. 55)
#'
#' Depends, sourced first, on: dice.R, exploration_tables.R, explore.R, hexmap.R.
#'
#' The Action phase has five campaign actions. Each player performs one per
#' phase, in order: all Battle-phase winners (by priority), then drawers, then
#' losers; a bye counts as a draw. This file resolves a SINGLE action: its SP
#' cost, whether the player may legally take it, and the campaign-layer effect.
#'
#' STATE CONTRACT
#' --------------
#' `state` is a list:
#'   $map   tibble(hex_number, type, q, r, explored [lgl],
#'                 location_code [chr|NA], condition_code [chr|NA],
#'                 supply_remaining [int|NA], intel_remaining [int|NA])
#'   $team  list(hex = <hex_number>, supply = <0:10>, campaign = <int>,
#'               base = <hex_number>, camps = <int vec of hex_numbers>,
#'               flags = <named list of per-player once-flags / tokens>)
#'   $opp_sites tibble(hex_number, kind ["base"/"camp"], owner [chr])
#'   $round list(beaten = <chr opponents beaten this round>,
#'               walkover = <chr opponents you challenged who didn't play>)
#'
#' Every action returns a result list:
#'   list(action, ok [lgl], reason [chr], cost [int], state [updated],
#'        log [chr], needs_input [list|NULL])
#' When `needs_input` is non-NULL the action could not fully resolve in code and
#' the UI must collect a choice (e.g. which camp to remove, how much SP to
#' spend on a search); `ok` is still TRUE and `state`/`cost` reflect what is
#' settled so far.

library(dplyr)

# ---- catalogue --------------------------------------------------------------
campaign_actions <- tribble(
  ~action,    ~cost_type,         ~fixed_cost, ~needs_target, ~summary,
  "scout",    "chosen_1_3",       NA_integer_, TRUE,  "Spend 1-3 SP to explore one unexplored, non-blocked hex within that many hexes.",
  "resupply", "fixed",            0L,          FALSE, "Gain SP from your hex (base: 10; own camp: D3+3; blocked: 0; else: 1), capped at 10.",
  "search",   "fixed",            1L,          FALSE, "Pay 1 SP to resolve the current hex's search rule, if it has one.",
  "encamp",   "distance_to_site", NA_integer_, FALSE, "Pay the hex-distance to your nearest base/camp to found a camp here (max two camps).",
  "demolish", "fixed",            3L,          FALSE, "Pay 3 SP to remove an opponent's camp here (needs a win, or an unplayed challenge, vs them this round)."
)

# ---- small state helpers ----------------------------------------------------
.hex_row    <- function(state, hex = state$team$hex) state$map[state$map$hex_number == hex, ]
.loc_code   <- function(state, hex = state$team$hex) .hex_row(state, hex)$location_code
.opp_site_here <- function(state, kind = NULL) {
  s <- state$opp_sites[state$opp_sites$hex_number == state$team$hex, ]
  if (!is.null(kind)) s <- s[s$kind == kind, ]
  s
}

# ---- SCOUT ------------------------------------------------------------------
# Range is measured as graph distance over non-blocked hexes (see hex_distance).
# NOTE: the rules say "within a number of hexes" without specifying path vs.
# straight-line; graph distance is the consistent, conservative reading and the
# one decision worth confirming. Swap to hex_cube_distance if you prefer raw.

scout_targets <- function(state, sp_spent) {
  in_range <- hexes_within(state$map, state$team$hex, sp_spent)
  rows <- state$map[state$map$hex_number %in% in_range, ]
  rows <- rows[rows$type != "blocked" & !rows$explored, ]
  rows$hex_number
}

can_scout <- function(state, sp_spent) {
  if (!sp_spent %in% 1:3)            return(list(ok = FALSE, reason = "Scout costs 1-3 SP."))
  if (state$team$supply < sp_spent)  return(list(ok = FALSE, reason = "Not enough SP."))
  if (length(scout_targets(state, sp_spent)) == 0)
    return(list(ok = FALSE, reason = "No unexplored, non-blocked hex within range."))
  list(ok = TRUE, reason = "")
}

apply_scout <- function(state, sp_spent, target_hex) {
  chk <- can_scout(state, sp_spent)
  if (!chk$ok) return(list(action = "scout", ok = FALSE, reason = chk$reason,
                           cost = sp_spent, state = state, log = "", needs_input = NULL))
  if (!target_hex %in% scout_targets(state, sp_spent))
    return(list(action = "scout", ok = FALSE,
                reason = "Target is out of range, blocked, or already explored.",
                cost = sp_spent, state = state, log = "", needs_input = NULL))

  trow <- .hex_row(state, target_hex)
  used_loc  <- na.omit(state$map$location_code)
  used_cond <- na.omit(state$map$condition_code)
  res <- explore_hex(trow$type, used_loc, used_cond)   # from explore.R

  m <- state$map; i <- which(m$hex_number == target_hex)
  m$explored[i]       <- TRUE
  m$location_code[i]  <- res$location_code
  m$condition_code[i] <- res$condition_code
  if (isTRUE(res$becomes_blocked)) m$type[i] <- "blocked"
  if (!is.na(res$resource)) {
    if (res$resource == "supply") m$supply_remaining[i] <- res$resource_amount
    if (res$resource == "intel")  m$intel_remaining[i]  <- res$resource_amount
  }
  state$map <- m
  state$team$supply <- state$team$supply - sp_spent

  list(action = "scout", ok = TRUE, reason = "",
       cost = sp_spent, state = state,
       log = sprintf("Scouted hex %s -> %s / %s%s", target_hex,
                     res$location_name, res$condition_name,
                     if (!is.na(res$resource)) sprintf(" (%s %d)", res$resource, res$resource_amount) else ""),
       needs_input = NULL)
}

# ---- RESUPPLY ---------------------------------------------------------------
resupply_gain <- function(state) {
  team <- state$team; hex <- team$hex
  if (hex == team$base)        return(10L)             # tops the team up to 10
  if (hex %in% team$camps)     return(roll_d3() + 3L)
  if (.hex_row(state)$type == "blocked") return(0L)
  1L
}

can_resupply <- function(state) {
  if (identical(.loc_code(state), "SL24"))             # Asteroid Impact
    return(list(ok = FALSE, reason = "Cannot Resupply while in an Asteroid Impact hex."))
  list(ok = TRUE, reason = "")
}

apply_resupply <- function(state) {
  chk <- can_resupply(state)
  if (!chk$ok) return(list(action = "resupply", ok = FALSE, reason = chk$reason,
                           cost = 0L, state = state, log = "", needs_input = NULL))
  gain <- resupply_gain(state)
  if (identical(.loc_code(state), "SL25")) gain <- gain + roll_d3()   # Landing Site
  before <- state$team$supply
  state$team$supply <- min(10L, before + gain)
  list(action = "resupply", ok = TRUE, reason = "", cost = 0L, state = state,
       log = sprintf("Resupplied: %d -> %d SP", before, state$team$supply),
       needs_input = NULL)
}

# ---- SEARCH -----------------------------------------------------------------
# Which locations have a search rule, and how the campaign layer resolves it.
# kinds resolved in code: take_supply, take_intel, grant_supply, grant_cp_once,
# first_or_cp, gain_key. kinds returning needs_input (a player choice the UI
# collects): spend_choice, relocate, nominate, free_scout.
search_meta <- tribble(
  ~code,  ~kind,          ~dice,
  "SL22", "take_supply",  "d3",
  "SL32", "take_supply",  "d6",
  "SL31", "take_intel",   "d3",
  "SL35", "grant_supply", "d3",
  "SL34", "grant_cp_once", NA_character_,
  "TL34", "first_or_cp",  NA_character_,
  "TL36", "gain_key",     NA_character_,
  "SL23", "spend_choice", NA_character_,
  "TL23", "spend_choice", NA_character_,
  "TL31", "spend_choice", NA_character_,
  "SL33", "relocate",     NA_character_,
  "SL26", "free_scout",   NA_character_,
  "TL21", "nominate",     NA_character_,
  "TL25", "nominate",     NA_character_,
  "TL35", "nominate",     NA_character_
)

can_search <- function(state) {
  if (state$team$supply < 1L) return(list(ok = FALSE, reason = "Not enough SP."))
  if (!isTRUE(.hex_row(state)$explored))
    return(list(ok = FALSE, reason = "Hex is not explored."))
  if (!.loc_code(state) %in% search_meta$code)
    return(list(ok = FALSE, reason = "This hex has no search rule."))
  list(ok = TRUE, reason = "")
}

roll_dice_spec <- function(spec) switch(spec, "d3" = roll_d3(), "d6" = roll_d6(),
                                        stop("bad dice spec: ", spec))

resolve_search <- function(state) {
  code <- .loc_code(state)
  meta <- search_meta[search_meta$code == code, ]
  i <- which(state$map$hex_number == state$team$hex)

  switch(meta$kind,
    take_supply = {
      have <- state$map$supply_remaining[i]; have <- if (is.na(have)) 0L else have
      take <- min(roll_dice_spec(meta$dice), have)
      state$map$supply_remaining[i] <- have - take
      state$team$supply <- min(10L, state$team$supply + take)
      list(state = state, log = sprintf("Search %s: took %d SP (%d left).", code, take, have - take), needs_input = NULL)
    },
    take_intel = {
      have <- state$map$intel_remaining[i]; have <- if (is.na(have)) 0L else have
      take <- min(roll_d3(), have)
      state$map$intel_remaining[i] <- have - take
      # each intel grants a free 0-SP Scout that may target ANY unexplored
      # surface hex (handled by the UI as `take` queued free scouts)
      list(state = state, log = sprintf("Search %s: gained %d intel (%d left); %d free scout(s) queued.", code, take, have - take, take),
           needs_input = if (take > 0) list(requires = "free_scout", count = take, surface_only = TRUE, any_range = TRUE) else NULL)
    },
    grant_supply = {
      if (nrow(.opp_site_here(state)) > 0)
        return(list(state = state, log = "Search SL35 blocked: opponent base/camp here.", needs_input = NULL))
      g <- roll_d3(); state$team$supply <- min(10L, state$team$supply + g)
      list(state = state, log = sprintf("Search %s: gained %d SP.", code, g), needs_input = NULL)
    },
    grant_cp_once = {
      key <- paste0("searched_", code)
      if (isTRUE(state$team$flags[[key]]))
        return(list(state = state, log = sprintf("Search %s: already taken this campaign.", code), needs_input = NULL))
      state$team$flags[[key]] <- TRUE
      state$team$campaign <- state$team$campaign + 1L
      list(state = state, log = sprintf("Search %s: gained 1 CP.", code), needs_input = NULL)
    },
    first_or_cp = {
      key <- paste0("hex_searched_", state$team$hex)   # global flag in campaign_state
      first <- !isTRUE(state$team$flags[[key]])
      state$team$flags[[key]] <- TRUE
      gain <- if (first) roll_d3() else 1L
      state$team$campaign <- state$team$campaign + gain
      list(state = state, log = sprintf("Search %s: gained %d CP%s.", code, gain, if (first) " (first searcher)" else ""), needs_input = NULL)
    },
    gain_key = {
      state$team$flags$dimensional_key <- TRUE
      list(state = state, log = "Search TL36: gained the dimensional key.", needs_input = NULL)
    },
    # interactive kinds -> hand the choice back to the caller
    list(state = state, log = sprintf("Search %s needs a player decision (%s).", code, meta$kind),
         needs_input = list(requires = meta$kind, code = code))
  )
}

apply_search <- function(state) {
  chk <- can_search(state)
  if (!chk$ok) return(list(action = "search", ok = FALSE, reason = chk$reason,
                           cost = 1L, state = state, log = "", needs_input = NULL))
  state$team$supply <- state$team$supply - 1L
  out <- resolve_search(state)
  list(action = "search", ok = TRUE, reason = "", cost = 1L,
       state = out$state, log = out$log, needs_input = out$needs_input)
}

# ---- ENCAMP -----------------------------------------------------------------
encamp_cost <- function(state) {
  sites <- c(state$team$base, state$team$camps)
  as.integer(hex_distance(state$map, state$team$hex, to = sites))
}

can_encamp <- function(state) {
  if (.hex_row(state)$type == "blocked")
    return(list(ok = FALSE, reason = "Cannot encamp in a blocked hex."))
  if (nrow(.opp_site_here(state)) > 0)
    return(list(ok = FALSE, reason = "Hex contains an opponent's base or camp."))
  if (identical(.loc_code(state), "SL36") && !isTRUE(state$team$flags$beast_destroyed_SL36))
    return(list(ok = FALSE, reason = "Beast Lair: cannot encamp until the beast is destroyed."))
  cost <- encamp_cost(state)
  if (state$team$supply < cost)
    return(list(ok = FALSE, reason = sprintf("Not enough SP (need %d).", cost)))
  list(ok = TRUE, reason = "")
}

apply_encamp <- function(state, remove_camp = NULL) {
  chk <- can_encamp(state)
  if (!chk$ok) return(list(action = "encamp", ok = FALSE, reason = chk$reason,
                           cost = NA_integer_, state = state, log = "", needs_input = NULL))
  cost <- encamp_cost(state)
  camps <- state$team$camps
  if (length(camps) >= 2 && is.null(remove_camp))
    return(list(action = "encamp", ok = TRUE, reason = "", cost = cost, state = state,
                log = "Two camps already held - choose one to remove.",
                needs_input = list(requires = "remove_camp", options = camps)))
  if (!is.null(remove_camp)) camps <- setdiff(camps, remove_camp)
  state$team$camps  <- c(camps, state$team$hex)
  state$team$supply <- state$team$supply - cost
  list(action = "encamp", ok = TRUE, reason = "", cost = cost, state = state,
       log = sprintf("Encamped at hex %s for %d SP.", state$team$hex, cost),
       needs_input = NULL)
}

# ---- DEMOLISH ---------------------------------------------------------------
can_demolish <- function(state) {
  if (state$team$supply < 3L) return(list(ok = FALSE, reason = "Not enough SP (need 3)."))
  if (identical(.loc_code(state), "TL22"))
    return(list(ok = FALSE, reason = "Transeptum Maze camp rule forbids Demolish here."))
  camp <- .opp_site_here(state, kind = "camp")
  if (nrow(camp) == 0) return(list(ok = FALSE, reason = "No opponent camp in this hex."))
  owner <- camp$owner[1]
  if (!(owner %in% state$round$beaten || owner %in% state$round$walkover))
    return(list(ok = FALSE, reason = sprintf("Need a win or unplayed challenge vs %s this round.", owner)))
  list(ok = TRUE, reason = "")
}

apply_demolish <- function(state) {
  chk <- can_demolish(state)
  if (!chk$ok) return(list(action = "demolish", ok = FALSE, reason = chk$reason,
                           cost = 3L, state = state, log = "", needs_input = NULL))
  camp <- .opp_site_here(state, kind = "camp")
  state$opp_sites <- state$opp_sites[!(state$opp_sites$hex_number == state$team$hex &
                                       state$opp_sites$kind == "camp" &
                                       state$opp_sites$owner == camp$owner[1]), ]
  state$team$supply <- state$team$supply - 3L
  list(action = "demolish", ok = TRUE, reason = "", cost = 3L, state = state,
       log = sprintf("Demolished %s's camp at hex %s.", camp$owner[1], state$team$hex),
       needs_input = NULL)
}

# ---- dispatcher -------------------------------------------------------------
#' Perform one campaign action.
#' @param state see STATE CONTRACT.
#' @param action one of campaign_actions$action.
#' @param ... action-specific args: scout needs sp_spent and target_hex;
#'   encamp accepts remove_camp.
perform_action <- function(state, action, ...) {
  args <- list(...)
  switch(action,
    scout    = apply_scout(state, args$sp_spent, args$target_hex),
    resupply = apply_resupply(state),
    search   = apply_search(state),
    encamp   = apply_encamp(state, remove_camp = args$remove_camp),
    demolish = apply_demolish(state),
    stop("Unknown action: ", action)
  )
}
