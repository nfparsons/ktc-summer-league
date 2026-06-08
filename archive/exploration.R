# =====================================================================
# Ctesiphus Expedition — exploration
# ---------------------------------------------------------------------
# Source order: dice.R, round_engine.R, effects.R, exploration.R
# (this file uses d36(), log_event(), phase_cursor(), get_effect()).
#
# Encodes only the STRUCTURE of the exploration tables (pp.58-67): which
# D36 row maps to which location/condition code, its short label, and the
# mechanical flags the engine needs. The rules wording and flavour text
# live in the rule_text table the facilitator populates; nothing prose is
# duplicated here.
# =====================================================================

library(dplyr)
library(tibble)
library(DBI)

# --- D36 lookup tables ------------------------------------------------
# d36_lo/d36_hi give the inclusive row span (Ruin / Clear Conditions span
# 11-16; everything else is a single row). explore_roll names the
# on-exploration resource roll, if any, dispatched via the registry.

surface_locations <- tibble::tribble(
  ~code,  ~d36_lo, ~d36_hi, ~label,                  ~becomes_blocked, ~explore_roll,
  "SL11", 11L,     16L,     "Ruin",                  FALSE,            NA_character_,
  "SL21", 21L,     21L,     "Tectonic Fissure",      TRUE,             NA_character_,
  "SL22", 22L,     22L,     "Abandoned Camp",        FALSE,            "supply_d6",
  "SL23", 23L,     23L,     "Cryovolcanic Edifices", FALSE,            NA_character_,
  "SL24", 24L,     24L,     "Asteroid Impact",       FALSE,            NA_character_,
  "SL25", 25L,     25L,     "Landing Site",          FALSE,            NA_character_,
  "SL26", 26L,     26L,     "Observation Tower",     FALSE,            NA_character_,
  "SL31", 31L,     31L,     "Crashed Ship",          FALSE,            "intel_d6",
  "SL32", 32L,     32L,     "Resource Stockpile",    FALSE,            "supply_2d6",
  "SL33", 33L,     33L,     "Starsteles",            FALSE,            NA_character_,
  "SL34", 34L,     34L,     "Blackstone Obelisk",    FALSE,            NA_character_,
  "SL35", 35L,     35L,     "Forsaken Fortress",     FALSE,            NA_character_,
  "SL36", 36L,     36L,     "Beast Lair",            FALSE,            NA_character_
)

tomb_locations <- tibble::tribble(
  ~code,  ~d36_lo, ~d36_hi, ~label,                   ~becomes_blocked, ~explore_roll,
  "TL11", 11L,     16L,     "Ruin",                   FALSE,            NA_character_,
  "TL21", 21L,     21L,     "Transdimensional Portal",FALSE,            NA_character_,
  "TL22", 22L,     22L,     "Transeptum Maze",        FALSE,            NA_character_,
  "TL23", 23L,     23L,     "Crucible of Whispers",   FALSE,            "campaign_d3",
  "TL24", 24L,     24L,     "Power Cell Sanctum",     FALSE,            NA_character_,
  "TL25", 25L,     25L,     "Transtechnic Fulcrum",   FALSE,            NA_character_,
  "TL26", 26L,     26L,     "Energy Hot Spot",        FALSE,            NA_character_,
  "TL31", 31L,     31L,     "Vivitrophic Terminal",   FALSE,            NA_character_,
  "TL32", 32L,     32L,     "Hyperfractal Gaol",      FALSE,            NA_character_,
  "TL33", 33L,     33L,     "Revivification Crypt",   FALSE,            NA_character_,
  "TL34", 34L,     34L,     "Astral Augury",          FALSE,            NA_character_,
  "TL35", 35L,     35L,     "Doomsday Vault",         FALSE,            "searchcost_d3",
  "TL36", 36L,     36L,     "Dimension Matrix",       FALSE,            NA_character_
)

surface_conditions <- tibble::tribble(
  ~code,  ~d36_lo, ~d36_hi, ~label,
  "SC11", 11L,     16L,     "Clear Conditions",
  "SC21", 21L,     21L,     "Dust Storms",
  "SC22", 22L,     22L,     "Radiation Field",
  "SC23", 23L,     23L,     "Blighted Land",
  "SC24", 24L,     24L,     "Missile Strike",
  "SC25", 25L,     25L,     "Minefield",
  "SC26", 26L,     26L,     "Skull Mounds",
  "SC31", 31L,     31L,     "Subterranean Tremors",
  "SC32", 32L,     32L,     "Exotic Particle Field",
  "SC33", 33L,     33L,     "Metallic Infused Vegetation",
  "SC34", 34L,     34L,     "Gyromantic Shards",
  "SC35", 35L,     35L,     "Cryoflux Blizzard",
  "SC36", 36L,     36L,     "Gravitic Anomaly"
)

tomb_conditions <- tibble::tribble(
  ~code,  ~d36_lo, ~d36_hi, ~label,
  "TC11", 11L,     16L,     "Clear Conditions",
  "TC21", 21L,     21L,     "Darkness",
  "TC22", 22L,     22L,     "Scarab Swarm",
  "TC23", 23L,     23L,     "Tesla Rupture",
  "TC24", 24L,     24L,     "Automated System Error",
  "TC25", 25L,     25L,     "Weakened Structure",
  "TC26", 26L,     26L,     "Crypt Fatigue",
  "TC31", 31L,     31L,     "Temporal Diffusement",
  "TC32", 32L,     32L,     "Hyperspatial Breach",
  "TC33", 33L,     33L,     "Xenoviral Demise",
  "TC34", 34L,     34L,     "Collapsing Tomb",
  "TC35", 35L,     35L,     "Nanoweave Web Traps",
  "TC36", 36L,     36L,     "Neurotechnic Haunting"
)

all_locations <- dplyr::bind_rows(surface_locations, tomb_locations)

# Ruin / Clear Conditions are the only codes that may repeat on the map.
DUP_EXEMPT <- c("SL11", "TL11", "SC11", "TC11")

# --- the unique draw (p.58) ------------------------------------------
# Roll D36, map the row to a code. If that code already exists on the map
# and isn't dup-exempt, re-roll. `kind` selects the column to check.

draw_unique <- function(con, tbl, kind = c("location", "condition")) {
  kind <- match.arg(kind)
  col  <- paste0(kind, "_code")
  existing <- DBI::dbGetQuery(con,
    sprintf("SELECT DISTINCT %s AS code FROM hexes WHERE %s IS NOT NULL", col, col))$code
  repeat {
    r <- d36()
    code <- tbl$code[r >= tbl$d36_lo & r <= tbl$d36_hi]
    if (length(code) != 1L) next                 # full coverage; guard anyway
    if (code %in% DUP_EXEMPT) break              # Ruin / Clear: repeats allowed
    if (!(code %in% existing)) break             # new code: accept
    # otherwise a non-exempt duplicate -> re-roll
  }
  list(roll = r, code = code)
}

# --- explore a hex ----------------------------------------------------
# n_options = 1: draw and commit immediately (the standard case).
# n_options > 1: draw that many valid location candidates and return them
# WITHOUT committing, for the player to choose (Observation Tower SL26),
# then finalise with commit_exploration().

explore_hex <- function(con, hex_uid, via = c("move", "scout"),
                        n_options = 1L, round = NULL, team_id = NULL) {
  via <- match.arg(via)
  hx <- DBI::dbGetQuery(con,
    "SELECT h.hex_uid, h.hex_no, h.hex_type, h.explored, h.blocked,
            EXISTS (SELECT 1 FROM teams t WHERE t.base_hex = h.hex_uid) AS is_base
       FROM hexes h WHERE h.hex_uid = $1",
    params = list(hex_uid)) |> tibble::as_tibble()
  if (nrow(hx) == 0L) stop("No such hex_uid: ", hex_uid, call. = FALSE)
  if (isTRUE(hx$explored)) stop("Hex ", hx$hex_no, " is already explored.", call. = FALSE)
  if (isTRUE(hx$is_base))  stop("Base hexes cannot be explored.", call. = FALSE)
  if (isTRUE(hx$blocked))  stop("Blocked hexes cannot be explored.", call. = FALSE)

  loc_tbl  <- if (hx$hex_type == "tomb") tomb_locations  else surface_locations
  cond_tbl <- if (hx$hex_type == "tomb") tomb_conditions else surface_conditions

  if (n_options > 1L) {
    cands <- lapply(seq_len(n_options), function(i) draw_unique(con, loc_tbl, "location"))
    return(list(needs_choice = TRUE, hex_uid = hex_uid,
                candidates = cands, hex_type = hx$hex_type))
  }

  loc  <- draw_unique(con, loc_tbl,  "location")
  cond <- draw_unique(con, cond_tbl, "condition")
  # NB (solo/co-op, p.57): exploring a tomb hex other than via Scout rolls
  # a D6 to raise threat. That branch belongs to the solo engine; hook here
  # keyed on via == "move" && hx$hex_type == "tomb".
  commit_exploration(con, hex_uid, loc$code, cond$code, round = round, team_id = team_id)
}

commit_exploration <- function(con, hex_uid, location_code, condition_code,
                               round = NULL, team_id = NULL) {
  becomes_blocked <- isTRUE(all_locations$becomes_blocked[
    all_locations$code == location_code])

  DBI::dbExecute(con,
    "UPDATE hexes
        SET location_code = $2, condition_code = $3,
            explored = TRUE, blocked = $4
      WHERE hex_uid = $1",
    params = list(hex_uid, location_code, condition_code, becomes_blocked))

  # On-exploration resource roll, if this location has one.
  ex_fn  <- get_effect(location_code, "explore")
  ex_out <- if (!is.null(ex_fn)) ex_fn(list(con = con, hex_uid = hex_uid)) else NULL

  log_event(con, round %||% phase_cursor(con)$current_round, team_id, "hex_explored",
            list(hex_uid = hex_uid, location = location_code,
                 condition = condition_code, blocked = becomes_blocked,
                 on_explore = ex_out))
  invisible(list(needs_choice = FALSE, location = location_code,
                 condition = condition_code, blocked = becomes_blocked,
                 on_explore = ex_out))
}

# --- per-hex resources ------------------------------------------------
set_hex_resource <- function(con, hex_uid, resource_type, amount) {
  DBI::dbExecute(con,
    "INSERT INTO hex_resources (hex_uid, resource_type, amount)
     VALUES ($1, $2, $3)
     ON CONFLICT (hex_uid, resource_type) DO UPDATE SET amount = EXCLUDED.amount",
    params = list(hex_uid, resource_type, amount))
  invisible(amount)
}

# --- on-exploration resource rolls (registered into the effects table) -
register_effect("SL22", "explore", function(ctx) {            # Abandoned Camp
  amt <- d6(); set_hex_resource(ctx$con, ctx$hex_uid, "supply", amt)
  list(supply = amt)
})
register_effect("SL31", "explore", function(ctx) {            # Crashed Ship
  amt <- d6(); set_hex_resource(ctx$con, ctx$hex_uid, "intel", amt)
  list(intel = amt)
})
register_effect("SL32", "explore", function(ctx) {            # Resource Stockpile
  amt <- sum(d6(2L)); set_hex_resource(ctx$con, ctx$hex_uid, "supply", amt)
  list(supply = amt)
})
register_effect("TL23", "explore", function(ctx) {            # Crucible of Whispers
  amt <- d3(); set_hex_resource(ctx$con, ctx$hex_uid, "campaign", amt)
  list(campaign = amt)
})
register_effect("TL35", "explore", function(ctx) {            # Doomsday Vault
  amt <- d3(); set_hex_resource(ctx$con, ctx$hex_uid, "search_cost", amt)
  list(search_cost = amt)
})
