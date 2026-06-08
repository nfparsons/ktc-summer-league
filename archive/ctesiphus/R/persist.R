#' Persistence bridge: in-memory functional `board` <-> Postgres schema.
#'
#' Depends on: DBI, RPostgres, jsonlite, tibble, dplyr.
#'
#' The rules engine speaks `hex_number`; the schema keys hexes by `hex_uid`.
#' This file is the only place that translation lives. It also pivots
#' hex_resources into the map's *_remaining columns, and round-trips per-team
#' flags + campaign tokens through the campaign_state key/value table.
#'
#' NOT YET EXERCISED against a live Postgres. The two things to watch first are
#' the integer[] binding (camps/path) and jsonb round-trips (flags/tokens).

library(DBI); library(jsonlite); library(tibble); library(dplyr)

db_connect <- function() {
  DBI::dbConnect(RPostgres::Postgres(),
    dbname   = Sys.getenv("CTES_DB",   "ctesiphus"),
    host     = Sys.getenv("CTES_HOST", "localhost"),
    port     = as.integer(Sys.getenv("CTES_PORT", "5432")),
    user     = Sys.getenv("CTES_USER"),
    password = Sys.getenv("CTES_PASS"))
}

# ---- key maps ---------------------------------------------------------------
.hexmap_keys <- function(con, campaign_id) {
  k <- DBI::dbGetQuery(con,
    "SELECT hex_uid, hex_number FROM hexes WHERE campaign_id = $1",
    params = list(campaign_id))
  list(uid_of = setNames(k$hex_uid, as.character(k$hex_number)),
       num_of = setNames(k$hex_number, as.character(k$hex_uid)))
}

# ---- load -------------------------------------------------------------------
load_board <- function(con, campaign_id) {
  camp <- DBI::dbGetQuery(con,
    "SELECT current_threat, max_threat, current_round, current_phase, phase_status
       FROM campaign WHERE campaign_id = $1", params = list(campaign_id))
  keys <- .hexmap_keys(con, campaign_id)

  hx <- DBI::dbGetQuery(con,
    "SELECT hex_uid, hex_number, type, coord_q, coord_r, explored, blocked,
            location_code, condition_code FROM hexes WHERE campaign_id = $1",
    params = list(campaign_id)) |> tibble::as_tibble()
  rsc <- DBI::dbGetQuery(con,
    "SELECT hr.hex_uid, hr.resource_type, hr.remaining
       FROM hex_resources hr JOIN hexes h ON h.hex_uid = hr.hex_uid
      WHERE h.campaign_id = $1", params = list(campaign_id))

  pick <- function(uid, type) { v <- rsc$remaining[rsc$hex_uid == uid & rsc$resource_type == type]; if (length(v)) v[1] else NA_integer_ }
  map <- tibble::tibble(
    hex_number        = hx$hex_number,
    type              = ifelse(hx$blocked, "blocked", hx$type),   # engine treats blocked via type
    q = hx$coord_q, r = hx$coord_r,
    explored          = hx$explored,
    location_code     = hx$location_code,
    condition_code    = hx$condition_code,
    supply_remaining   = vapply(hx$hex_uid, pick, integer(1), type = "supply"),
    intel_remaining    = vapply(hx$hex_uid, pick, integer(1), type = "intel"),
    campaign_remaining = vapply(hx$hex_uid, pick, integer(1), type = "campaign"),
    search_cost        = vapply(hx$hex_uid, pick, integer(1), type = "search_cost"))

  kt <- DBI::dbGetQuery(con,
    "SELECT team_id, base_hex_uid, current_hex_uid, campaign_points, supply_points
       FROM kill_teams WHERE campaign_id = $1", params = list(campaign_id))
  camps <- DBI::dbGetQuery(con,
    "SELECT c.team_id, c.hex_uid FROM camps c JOIN kill_teams t ON t.team_id = c.team_id
      WHERE t.campaign_id = $1 AND c.kind = 'camp' AND c.active", params = list(campaign_id))

  get_state <- function(key, default) {
    r <- DBI::dbGetQuery(con, "SELECT value FROM campaign_state WHERE campaign_id = $1 AND key = $2",
                         params = list(campaign_id, key))
    if (nrow(r)) jsonlite::fromJSON(r$value) else default
  }

  teams <- list()
  for (i in seq_len(nrow(kt))) {
    tid <- as.character(kt$team_id[i])
    teams[[tid]] <- list(
      hex      = unname(keys$num_of[as.character(kt$current_hex_uid[i])]),
      supply   = kt$supply_points[i],
      campaign = kt$campaign_points[i],
      base     = unname(keys$num_of[as.character(kt$base_hex_uid[i])]),
      camps    = unname(keys$num_of[as.character(camps$hex_uid[camps$team_id == kt$team_id[i]])]),
      flags    = get_state(paste0("team_flags_", tid), list()))
  }

  list(map = map, teams = teams,
       threat = camp$current_threat, max_threat = camp$max_threat,
       tokens = get_state("tokens", list()),
       priority = NULL,
       cursor = list(round = camp$current_round, phase = camp$current_phase,
                     status = camp$phase_status))
}

# ---- save (idempotent full write) -------------------------------------------
save_board <- function(con, campaign_id, board, log = character(0),
                       phase = NA, round_id = NA) {
  keys <- .hexmap_keys(con, campaign_id)
  uid  <- function(num) unname(keys$uid_of[as.character(num)])

  DBI::dbWithTransaction(con, {
    # campaign cursor + threat
    DBI::dbExecute(con,
      "UPDATE campaign SET current_threat = $2 WHERE campaign_id = $1",
      params = list(campaign_id, board$threat))

    # hexes
    for (i in seq_len(nrow(board$map))) {
      m <- board$map[i, ]
      DBI::dbExecute(con,
        "UPDATE hexes SET explored = $3, blocked = $4, location_code = $5, condition_code = $6
          WHERE campaign_id = $1 AND hex_number = $2",
        params = list(campaign_id, m$hex_number, m$explored, m$type == "blocked",
                      m$location_code, m$condition_code))
      for (rt in c("supply", "intel", "campaign", "search_cost")) {
        col <- switch(rt, supply = "supply_remaining", intel = "intel_remaining",
                          campaign = "campaign_remaining", search_cost = "search_cost")
        val <- m[[col]]
        if (!is.na(val)) DBI::dbExecute(con,
          "INSERT INTO hex_resources (hex_uid, resource_type, remaining) VALUES ($1, $2, $3)
           ON CONFLICT (hex_uid, resource_type) DO UPDATE SET remaining = EXCLUDED.remaining",
          params = list(uid(m$hex_number), rt, as.integer(val)))
      }
    }

    # teams + flags
    for (tid in names(board$teams)) {
      t <- board$teams[[tid]]; id <- as.integer(tid)
      DBI::dbExecute(con,
        "UPDATE kill_teams SET supply_points = $2, campaign_points = $3,
                current_hex_uid = $4, base_hex_uid = $5 WHERE team_id = $1",
        params = list(id, t$supply, t$campaign, uid(t$hex), uid(t$base)))
      DBI::dbExecute(con,
        "INSERT INTO campaign_state (campaign_id, key, value) VALUES ($1, $2, $3)
         ON CONFLICT (campaign_id, key) DO UPDATE SET value = EXCLUDED.value",
        params = list(campaign_id, paste0("team_flags_", tid),
                      jsonlite::toJSON(t$flags, auto_unbox = TRUE)))
      # camp sync: deactivate any active camp not in the desired set, add the rest
      want <- vapply(t$camps, uid, integer(1))
      DBI::dbExecute(con,
        "UPDATE camps SET active = false WHERE team_id = $1 AND kind = 'camp' AND active
           AND NOT (hex_uid = ANY($2))",
        params = list(id, if (length(want)) paste0("{", paste(want, collapse = ","), "}") else "{}"))
      for (h in want) DBI::dbExecute(con,
        "INSERT INTO camps (team_id, hex_uid, kind, active)
         SELECT $1, $2, 'camp', true
          WHERE NOT EXISTS (SELECT 1 FROM camps WHERE team_id = $1 AND hex_uid = $2 AND kind = 'camp' AND active)",
        params = list(id, h))
    }

    # campaign tokens (prisoner, dimensional key, ...)
    DBI::dbExecute(con,
      "INSERT INTO campaign_state (campaign_id, key, value) VALUES ($1, 'tokens', $2)
       ON CONFLICT (campaign_id, key) DO UPDATE SET value = EXCLUDED.value",
      params = list(campaign_id, jsonlite::toJSON(board$tokens, auto_unbox = TRUE)))

    # append the phase log to event_log
    for (line in log) DBI::dbExecute(con,
      "INSERT INTO event_log (campaign_id, round_id, phase, event_type, payload)
       VALUES ($1, $2, $3, 'phase_log', $4)",
      params = list(campaign_id, round_id %||% NA, phase %||% NA,
                    jsonlite::toJSON(list(message = line), auto_unbox = TRUE)))
  })
  invisible(TRUE)
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
