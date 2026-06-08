#' Bootstrap a campaign: seed the campaign row, load a board from a map CSV,
#' and place each kill team on its starting base.
#'
#' Run AFTER the schema exists (psql -f inst/sql/schema.sql, or apply_schema()).
#' Source the engine + persist + lifecycle first, then call seed_campaign().
#'
#'   source("R/persist.R"); source("R/map_selection.R"); source("R/lifecycle.R")
#'   con <- db_connect()
#'   seed_campaign(con, season_name = "...", teams = <see template at bottom>)
#'
#' `teams` is a data.frame / tibble with one row per player:
#'   player_name, team_name, login_handle, base_hex   (base_hex = hex_number)
#' Bases must be distinct surface hexes on the chosen map.

library(DBI); library(readr); library(jsonlite)

#' Apply schema.sql by statement (no dollar-quoted bodies in our schema, so a
#' naive split on ';' is safe). Prefer `psql -f` in practice.
apply_schema <- function(con, path = "inst/sql/schema.sql") {
  sql <- paste(readLines(path), collapse = "\n")
  stmts <- trimws(strsplit(sql, ";", fixed = TRUE)[[1]])
  for (s in stmts) if (nzchar(s) && !grepl("^\\s*--", s)) DBI::dbExecute(con, s)
  invisible(TRUE)
}

seed_campaign <- function(con, season_name, teams,
                          map_id = NULL, n_players = NULL, max_threat = 7L,
                          maps_dir = "inst/maps", seed = NULL) {
  stopifnot(is.data.frame(teams),
            all(c("player_name","team_name","login_handle","base_hex") %in% names(teams)))
  if (is.null(map_id)) {
    if (is.null(n_players)) n_players <- nrow(teams)
    if (!is.null(seed)) set.seed(seed)
    map_id <- choose_map(n_players)                 # weighted draw (map_selection.R)
  }

  board <- readr::read_csv(file.path(maps_dir, sprintf("map%d.csv", map_id)),
                           show_col_types = FALSE)  # cols: hex_number, type, q, r
  bad <- setdiff(teams$base_hex, board$hex_number[board$type == "surface"])
  if (length(bad)) stop("Base hex(es) not surface or not on map: ", paste(bad, collapse = ", "))
  if (anyDuplicated(teams$base_hex)) stop("Two teams share a base hex.")

  DBI::dbWithTransaction(con, {
    campaign_id <- DBI::dbGetQuery(con,
      "INSERT INTO campaign (season_name, map_id, max_threat)
       VALUES ($1, $2, $3) RETURNING campaign_id",
      params = list(season_name, map_id, max_threat))$campaign_id

    for (i in seq_len(nrow(board))) {
      r <- board[i, ]
      DBI::dbExecute(con,
        "INSERT INTO hexes (campaign_id, hex_number, type, coord_q, coord_r, blocked)
         VALUES ($1, $2, $3, $4, $5, $6)",
        params = list(campaign_id, r$hex_number, r$type, r$q, r$r, r$type == "blocked"))
    }
    uid <- function(num) DBI::dbGetQuery(con,
      "SELECT hex_uid FROM hexes WHERE campaign_id = $1 AND hex_number = $2",
      params = list(campaign_id, num))$hex_uid

    for (i in seq_len(nrow(teams))) {
      t <- teams[i, ]; b <- uid(t$base_hex)
      DBI::dbExecute(con,
        "INSERT INTO kill_teams (campaign_id, player_name, team_name, login_handle,
                                 base_hex_uid, current_hex_uid)
         VALUES ($1, $2, $3, $4, $5, $5) RETURNING team_id",
        params = list(campaign_id, t$player_name, t$team_name, t$login_handle, b))
      tid <- DBI::dbGetQuery(con,
        "SELECT team_id FROM kill_teams WHERE campaign_id = $1 AND login_handle = $2",
        params = list(campaign_id, t$login_handle))$team_id
      DBI::dbExecute(con,
        "INSERT INTO camps (team_id, hex_uid, kind) VALUES ($1, $2, 'base')",
        params = list(tid, b))
    }

    DBI::dbExecute(con,
      "INSERT INTO event_log (campaign_id, phase, event_type, payload)
       VALUES ($1, 'setup', 'campaign_created', $2)",
      params = list(campaign_id, jsonlite::toJSON(
        list(map_id = map_id, teams = nrow(teams), max_threat = max_threat), auto_unbox = TRUE)))

    message(sprintf("Seeded campaign %d on map %d with %d teams. Cursor: setup/open.",
                    campaign_id, map_id, nrow(teams)))
    campaign_id
  })
}

# ---------------------------------------------------------------------------
# TEMPLATE - fill with your real players and base hexes, then run. This is a
# blank to complete, not data: edit it before sourcing the call.
#
# teams <- tibble::tribble(
#   ~player_name, ~team_name, ~login_handle, ~base_hex,
#   # "Real Name", "Team Name", "handle",     <surface hex_number>,
# )
# con <- db_connect()
# seed_campaign(con, season_name = "Summer 2026", teams = teams, n_players = nrow(teams))
# DBI::dbDisconnect(con)
# ---------------------------------------------------------------------------
