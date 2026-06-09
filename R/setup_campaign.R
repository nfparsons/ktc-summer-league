#' Campaign setup: create a campaign shell, let players join, or seed all at once.
#'
#' Run AFTER the schema exists. Source persist.R + map_selection.R first.
#'
#'   create_campaign(con, season, available_teams = c("Faction A", "Faction B", ...))
#'      -> seeds the campaign + board, stores the allowed-teams roster, NO players.
#'   join_campaign(con, campaign_id, player_name, team_name, base_hex, login_handle = NULL)
#'      -> adds one player (used by the GUI Join tab; concurrency-safe).
#'   seed_campaign(con, season, teams)   -> facilitator enters everyone at once.

library(DBI); library(readr); library(jsonlite)

#' Short, unambiguous join code (no 0/O/1/I) for campaign gating.
gen_join_code <- function(n = 6L) {
  a <- strsplit("ABCDEFGHJKLMNPQRSTUVWXYZ23456789", "")[[1]]
  paste(sample(a, n, replace = TRUE), collapse = "")
}
#' Add the join_code column + unique index if missing (idempotent on a live DB).
ensure_join_code_col <- function(con) {
  DBI::dbExecute(con, "ALTER TABLE campaign ADD COLUMN IF NOT EXISTS join_code text")
  DBI::dbExecute(con, "CREATE UNIQUE INDEX IF NOT EXISTS campaign_join_code_uniq ON campaign(join_code)")
  invisible(TRUE)
}
#' Resolve a join code to a campaign_id (case-insensitive); NA if none.
campaign_by_code <- function(con, code) {
  if (is.null(code) || !nzchar(code)) return(NA_integer_)
  r <- DBI::dbGetQuery(con, "SELECT campaign_id FROM campaign WHERE upper(join_code) = upper($1)",
                       params = list(code))
  if (nrow(r)) as.integer(r$campaign_id[1]) else NA_integer_
}

#' Edit campaign settings (any provided field).
update_campaign <- function(con, campaign_id, season_name = NULL, max_threat = NULL,
                            signup_open = NULL) {
  if (!is.null(season_name) && nzchar(season_name))
    DBI::dbExecute(con, "UPDATE campaign SET season_name=$2 WHERE campaign_id=$1",
                   params = list(campaign_id, season_name))
  if (!is.null(max_threat))
    DBI::dbExecute(con, "UPDATE campaign SET max_threat=$2 WHERE campaign_id=$1",
                   params = list(campaign_id, as.integer(max_threat)))
  if (!is.null(signup_open))
    DBI::dbExecute(con, "UPDATE campaign SET signup_open=$2 WHERE campaign_id=$1",
                   params = list(campaign_id, isTRUE(signup_open)))
  invisible(TRUE)
}

#' Mint a fresh join code (e.g. if one leaks). Returns the new code.
regen_join_code <- function(con, campaign_id) {
  ensure_join_code_col(con)
  code <- gen_join_code()
  while (nrow(DBI::dbGetQuery(con,
    "SELECT 1 FROM campaign WHERE join_code=$1 AND campaign_id<>$2", params = list(code, campaign_id))))
    code <- gen_join_code()
  DBI::dbExecute(con, "UPDATE campaign SET join_code=$2 WHERE campaign_id=$1",
                 params = list(campaign_id, code))
  code
}

#' Permanently delete a campaign and everything that references it. The schema
#' FKs have no ON DELETE CASCADE, so children are removed in dependency order.
delete_campaign <- function(con, campaign_id) {
  DBI::dbWithTransaction(con, {
    ex <- function(sql) DBI::dbExecute(con, sql, params = list(campaign_id))
    ex("DELETE FROM event_log WHERE campaign_id = $1")
    ex("DELETE FROM actions   WHERE round_id IN (SELECT round_id FROM rounds WHERE campaign_id=$1)")
    ex("DELETE FROM battles   WHERE round_id IN (SELECT round_id FROM rounds WHERE campaign_id=$1)")
    ex("DELETE FROM movements WHERE round_id IN (SELECT round_id FROM rounds WHERE campaign_id=$1)")
    ex("DELETE FROM camps         WHERE team_id IN (SELECT team_id FROM kill_teams WHERE campaign_id=$1)")
    ex("DELETE FROM hex_resources WHERE hex_uid IN (SELECT hex_uid FROM hexes WHERE campaign_id=$1)")
    ex("DELETE FROM rounds         WHERE campaign_id = $1")
    ex("DELETE FROM kill_teams     WHERE campaign_id = $1")
    ex("DELETE FROM campaign_state WHERE campaign_id = $1")
    ex("DELETE FROM hexes          WHERE campaign_id = $1")
    ex("DELETE FROM campaign       WHERE campaign_id = $1")
  })
  invisible(TRUE)
}

#' Ensure the catalogue table exists (idempotent; safe on a live DB).
ensure_catalogue <- function(con) {
  DBI::dbExecute(con,
    "CREATE TABLE IF NOT EXISTS kill_team_catalogue (
       name text PRIMARY KEY, faction text, active boolean NOT NULL DEFAULT true)")
  invisible(TRUE)
}

#' Bring an existing database up to the current schema (idempotent; safe to run
#' on every boot). Instances created from an older schema.sql - before these
#' columns/types existed - are repaired here, so the app can treat the schema
#' as a precondition instead of assuming a fresh build. Called once per session
#' on the first live connection (see withCon() in app.R).
ensure_schema <- function(con) {
  ensure_join_code_col(con)   # campaign.join_code + unique index
  ensure_catalogue(con)       # kill_team_catalogue table

  # phase_status enum type (CREATE TYPE has no IF NOT EXISTS, so guard it)
  DBI::dbExecute(con,
    "DO $$ BEGIN
       CREATE TYPE phase_status AS ENUM ('open', 'resolved');
     EXCEPTION WHEN duplicate_object THEN NULL;
     END $$;")

  # campaign cursor + sign-up columns
  DBI::dbExecute(con, "ALTER TABLE campaign ADD COLUMN IF NOT EXISTS current_phase phase_name   NOT NULL DEFAULT 'setup'")
  DBI::dbExecute(con, "ALTER TABLE campaign ADD COLUMN IF NOT EXISTS phase_status  phase_status NOT NULL DEFAULT 'open'")
  DBI::dbExecute(con, "ALTER TABLE campaign ADD COLUMN IF NOT EXISTS signup_open   boolean      NOT NULL DEFAULT true")

  # hexes current-status column (topology stays in `type`; `blocked` is live state)
  DBI::dbExecute(con, "ALTER TABLE hexes ADD COLUMN IF NOT EXISTS blocked boolean NOT NULL DEFAULT false")

  invisible(TRUE)
}

#' Load (upsert) the catalogue from the shipped CSV. Run once to seed; safe to
#' re-run after a roster update (new teams added, existing left untouched).
load_catalogue <- function(con, path = "inst/kill_teams.csv") {
  ensure_catalogue(con)
  teams <- readr::read_csv(path, show_col_types = FALSE)$name
  for (n in teams) DBI::dbExecute(con,
    "INSERT INTO kill_team_catalogue (name) VALUES ($1) ON CONFLICT (name) DO NOTHING",
    params = list(n))
  invisible(length(teams))
}

#' Active catalogue team names, for pre-filling the Setup roster.
catalogue_teams <- function(con, active_only = TRUE) {
  q <- if (active_only) "SELECT name FROM kill_team_catalogue WHERE active ORDER BY name"
       else                "SELECT name FROM kill_team_catalogue ORDER BY name"
  DBI::dbGetQuery(con, q)$name
}

#' Apply schema.sql statement-by-statement (RPostgres runs one per call).
apply_schema <- function(con, path = "inst/sql/schema.sql") {
  lines <- readLines(path)
  lines <- sub("--.*$", "", lines)                        # strip ALL -- comments to EOL
  stmts <- trimws(strsplit(paste(lines, collapse = "\n"), ";", fixed = TRUE)[[1]])
  for (s in stmts) if (nzchar(s)) DBI::dbExecute(con, s)   # CREATE TYPE fails if re-run
  invisible(TRUE)
}

.available_teams <- function(con, campaign_id) {
  r <- DBI::dbGetQuery(con,
    "SELECT value FROM campaign_state WHERE campaign_id = $1 AND key = 'available_teams'",
    params = list(campaign_id))
  if (nrow(r)) as.character(jsonlite::fromJSON(r$value)) else character(0)
}

#' Create the campaign shell: campaign row + board hexes + allowed-teams roster.
#' Leaves the cursor at setup/open and adds NO players (they self-join).
create_campaign <- function(con, season_name, available_teams = character(),
                            map_id = NULL, n_players = NULL, max_threat = 7L,
                            maps_dir = "inst/maps", seed = NULL) {
  if (is.null(map_id)) {
    if (is.null(n_players)) n_players <- 4L
    if (!is.null(seed)) set.seed(seed)
    map_id <- choose_map(n_players)
  }
  board <- readr::read_csv(file.path(maps_dir, sprintf("map%d.csv", map_id)),
                           show_col_types = FALSE)
  ensure_join_code_col(con)
  code <- gen_join_code()
  while (nrow(DBI::dbGetQuery(con, "SELECT 1 FROM campaign WHERE join_code = $1", params = list(code))))
    code <- gen_join_code()

  DBI::dbWithTransaction(con, {
    cid <- DBI::dbGetQuery(con,
      "INSERT INTO campaign (season_name, map_id, max_threat, join_code)
       VALUES ($1, $2, $3, $4) RETURNING campaign_id",
      params = list(season_name, as.integer(map_id), as.integer(max_threat), code))$campaign_id
    for (i in seq_len(nrow(board))) {
      r <- board[i, ]
      DBI::dbExecute(con,
        "INSERT INTO hexes (campaign_id, hex_number, type, coord_q, coord_r, blocked)
         VALUES ($1, $2, $3, $4, $5, $6)",
        params = list(cid, as.integer(r$hex_number), r$type,
                      as.integer(r$q), as.integer(r$r), r$type == "blocked"))
    }
    DBI::dbExecute(con,
      "INSERT INTO campaign_state (campaign_id, key, value) VALUES ($1, 'available_teams', $2)",
      params = list(cid, jsonlite::toJSON(as.character(available_teams))))
    DBI::dbExecute(con,
      "INSERT INTO event_log (campaign_id, phase, event_type, payload)
       VALUES ($1, 'setup', 'campaign_created', $2)",
      params = list(cid, jsonlite::toJSON(
        list(map_id = map_id, roster = length(available_teams)), auto_unbox = TRUE)))
    cid
  })
}

#' Add one player to a campaign. Returns list(ok, team_id|msg). Concurrency-safe:
#' duplicate team name or base hex are caught and reported, not fatal.
join_campaign <- function(con, campaign_id, player_name, team_name, base_hex,
                          login_handle = NULL) {
  if (!nzchar(player_name) || !nzchar(team_name))
    return(list(ok = FALSE, msg = "Player name and kill team are required."))
  so <- DBI::dbGetQuery(con, "SELECT signup_open FROM campaign WHERE campaign_id=$1",
                        params = list(campaign_id))$signup_open
  if (length(so) && !isTRUE(so))
    return(list(ok = FALSE, msg = "Sign-ups are closed for this campaign."))
  h <- DBI::dbGetQuery(con,
    "SELECT hex_uid, type FROM hexes WHERE campaign_id = $1 AND hex_number = $2",
    params = list(campaign_id, as.integer(base_hex)))
  if (!nrow(h))               return(list(ok = FALSE, msg = "Base hex is not on this map."))
  if (h$type[1] != "surface") return(list(ok = FALSE, msg = "A base must be a surface hex."))
  roster <- .available_teams(con, campaign_id)
  if (length(roster) && !(team_name %in% roster))
    return(list(ok = FALSE, msg = "That kill team isn't in this campaign's roster."))

  handle <- if (is.null(login_handle) || !nzchar(login_handle)) NA else login_handle
  tryCatch({
    tid <- DBI::dbWithTransaction(con, {
      id <- DBI::dbGetQuery(con,
        "INSERT INTO kill_teams (campaign_id, player_name, team_name, login_handle,
                                 base_hex_uid, current_hex_uid)
         VALUES ($1, $2, $3, $4, $5, $5) RETURNING team_id",
        params = list(campaign_id, player_name, team_name, handle, h$hex_uid[1]))$team_id
      DBI::dbExecute(con, "INSERT INTO camps (team_id, hex_uid, kind) VALUES ($1, $2, 'base')",
                     params = list(id, h$hex_uid[1]))
      DBI::dbExecute(con,
        "INSERT INTO event_log (campaign_id, phase, event_type, payload)
         VALUES ($1, 'setup', 'team_joined', $2)",
        params = list(campaign_id, jsonlite::toJSON(
          list(team = team_name, player = player_name, base = base_hex), auto_unbox = TRUE)))
      id
    })
    list(ok = TRUE, team_id = tid)
  }, error = function(e) {
    m <- conditionMessage(e)
    if (grepl("team_name", m))     list(ok = FALSE, msg = "That kill team is already taken.")
    else if (grepl("base_hex_uid", m)) list(ok = FALSE, msg = "That base hex was just claimed - pick another.")
    else if (grepl("login_handle", m)) list(ok = FALSE, msg = "That handle is already in use.")
    else list(ok = FALSE, msg = m)
  })
}

#' Facilitator convenience: create the shell and join every team in one call.
#' `teams`: data.frame(player_name, team_name, base_hex, [login_handle]).
seed_campaign <- function(con, season_name, teams, map_id = NULL, n_players = NULL,
                          max_threat = 7L, maps_dir = "inst/maps", seed = NULL) {
  stopifnot(is.data.frame(teams),
            all(c("player_name","team_name","base_hex") %in% names(teams)))
  if (is.null(map_id)) { if (is.null(n_players)) n_players <- nrow(teams)
    if (!is.null(seed)) set.seed(seed); map_id <- choose_map(n_players) }
  cid <- create_campaign(con, season_name, available_teams = unique(teams$team_name),
                         map_id = map_id, max_threat = max_threat, maps_dir = maps_dir)
  for (i in seq_len(nrow(teams))) {
    t <- teams[i, ]
    r <- join_campaign(con, cid, t$player_name, t$team_name, t$base_hex,
                       login_handle = if ("login_handle" %in% names(teams)) t$login_handle else NULL)
    if (!isTRUE(r$ok)) stop(sprintf("Team %d (%s): %s", i, t$team_name, r$msg))
  }
  message(sprintf("Seeded campaign %d on map %d with %d teams.", cid, map_id, nrow(teams)))
  cid
}

# ---------------------------------------------------------------------------
# Everything above is driven by the GUI (Setup + Join tabs); no console use is
# required for normal operation.
# ---------------------------------------------------------------------------
