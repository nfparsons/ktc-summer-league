#' Weekly round/phase cursor on the Postgres campaign (rulebook p.53)
#'
#' Depends on: DBI, jsonlite.
#'
#' The live cursor is campaign.(current_round, current_phase, phase_status);
#' `rounds` is the per-week ledger (active while closed_at IS NULL). Phases
#' cycle movement -> battle -> action -> threat, then a new week. The
#' facilitator resolves the current phase (engine + save_board), marks it
#' resolved, then advances.

library(DBI); library(jsonlite)

CURSOR_PHASES <- c("movement", "battle", "action", "threat")

db_cursor <- function(con, campaign_id) {
  DBI::dbGetQuery(con,
    "SELECT current_round, current_phase, phase_status, current_threat, max_threat
       FROM campaign WHERE campaign_id = $1", params = list(campaign_id))
}

db_current_round_id <- function(con, campaign_id) {
  cur <- db_cursor(con, campaign_id)
  rid <- DBI::dbGetQuery(con,
    "SELECT round_id FROM rounds WHERE campaign_id = $1 AND week_number = $2",
    params = list(campaign_id, cur$current_round))$round_id
  if (length(rid)) rid else NA_integer_
}

mark_phase_resolved <- function(con, campaign_id) {
  DBI::dbExecute(con, "UPDATE campaign SET phase_status = 'resolved' WHERE campaign_id = $1",
                 params = list(campaign_id))
}

db_open_week <- function(con, campaign_id, week) {
  threat <- db_cursor(con, campaign_id)$current_threat
  DBI::dbExecute(con,
    "INSERT INTO rounds (campaign_id, week_number, threat_at_start)
     VALUES ($1, $2, $3) ON CONFLICT (campaign_id, week_number) DO NOTHING",
    params = list(campaign_id, week, threat))
  DBI::dbExecute(con,
    "UPDATE campaign SET current_round = $2, current_phase = 'movement', phase_status = 'open'
      WHERE campaign_id = $1", params = list(campaign_id, week))
  DBI::dbExecute(con,
    "INSERT INTO event_log (campaign_id, round_id, phase, event_type, payload)
     VALUES ($1, $2, 'movement', 'week_opened', $3)",
    params = list(campaign_id, db_current_round_id(con, campaign_id),
                  jsonlite::toJSON(list(week = week, threat = threat), auto_unbox = TRUE)))
}

db_close_week <- function(con, campaign_id, week) {
  DBI::dbExecute(con,
    "UPDATE rounds SET closed_at = now() WHERE campaign_id = $1 AND week_number = $2",
    params = list(campaign_id, week))
}

#' Advance the cursor. From 'setup' this opens week 1. Otherwise the current
#' phase must be 'resolved'; movement->battle->action->threat, and after threat
#' the week closes and (unless threat has hit max) the next week opens.
db_advance_phase <- function(con, campaign_id) {
  cur <- db_cursor(con, campaign_id)

  if (cur$current_phase == "setup") { db_open_week(con, campaign_id, 1L); return(invisible(db_cursor(con, campaign_id))) }
  if (cur$phase_status != "resolved")
    stop("Resolve the current phase before advancing.", call. = FALSE)

  if (cur$current_phase == "threat") {
    db_close_week(con, campaign_id, cur$current_round)
    if (cur$current_threat >= cur$max_threat) {
      DBI::dbExecute(con,
        "INSERT INTO event_log (campaign_id, round_id, phase, event_type, payload)
         VALUES ($1, $2, 'threat', 'campaign_ended', $3)",
        params = list(campaign_id, db_current_round_id(con, campaign_id),
                      jsonlite::toJSON(list(final_threat = cur$current_threat), auto_unbox = TRUE)))
      return(invisible(db_cursor(con, campaign_id)))
    }
    db_open_week(con, campaign_id, cur$current_round + 1L)
  } else {
    nxt <- CURSOR_PHASES[match(cur$current_phase, CURSOR_PHASES) + 1L]
    DBI::dbExecute(con,
      "UPDATE campaign SET current_phase = $2, phase_status = 'open' WHERE campaign_id = $1",
      params = list(campaign_id, nxt))
  }
  invisible(db_cursor(con, campaign_id))
}
