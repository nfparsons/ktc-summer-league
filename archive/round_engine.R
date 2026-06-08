# =====================================================================
# Ctesiphus Expedition — round engine
# ---------------------------------------------------------------------
# Submission is asynchronous; resolution is gated. Players submit_*()
# their phase action from a phone in any order. The facilitator calls
# resolve_current_phase(), which applies submissions in priority order
# atomically, then advance_phase() to open the next phase.
#
# Phase order: movement -> battle -> action -> threat -> (next round).
# =====================================================================

library(dplyr)
library(tibble)
library(DBI)
library(jsonlite)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x)) y else x

PHASES <- c("movement", "battle", "action", "threat")

# --- low-level helpers -----------------------------------------------

log_event <- function(con, round, team_id, kind, detail = list()) {
  DBI::dbExecute(con,
    "INSERT INTO event_log (round, team_id, kind, detail)
     VALUES ($1, $2, $3, $4)",
    params = list(round, team_id, kind,
                  jsonlite::toJSON(detail, auto_unbox = TRUE)))
}

phase_cursor <- function(con) {
  DBI::dbGetQuery(con,
    "SELECT current_round, current_phase, phase_status, threat_level
       FROM campaign_state LIMIT 1") |>
    tibble::as_tibble()
}

active_teams <- function(con) {
  DBI::dbGetQuery(con,
    "SELECT team_id, campaign_points, supply_points FROM teams") |>
    tibble::as_tibble()
}

# teams with no submission for the given phase this round
missing_submissions <- function(con, phase, round = NULL) {
  if (is.null(round)) round <- phase_cursor(con)$current_round
  tbl <- switch(phase, movement = "movements", battle = "battles", action = "actions")
  submitted <- DBI::dbGetQuery(con,
    sprintf("SELECT team_id FROM %s WHERE round = $1", tbl),
    params = list(round))$team_id
  setdiff(active_teams(con)$team_id, submitted)
}

# --- priority order (p.53) -------------------------------------------
# Least Campaign points, then least Supply points, then roll-off.
# The roll-off is a uniform tiebreak; the resolved order is logged so
# the draw is auditable, consistent with the event_log philosophy.

priority_order <- function(con, team_ids = NULL, secondary = NULL) {
  standings <- active_teams(con)
  if (!is.null(team_ids)) standings <- dplyr::filter(standings, team_id %in% team_ids)

  ord <- standings |>
    dplyr::mutate(rolloff = stats::runif(dplyr::n()))

  # `secondary` lets the Action phase pre-group by battle result
  # (winners first, then drawers, then losers — p.55) before applying
  # the standard p.53 tiebreak within each group.
  if (!is.null(secondary)) {
    ord <- ord |>
      dplyr::left_join(secondary, by = "team_id") |>
      dplyr::arrange(.data$group_rank, campaign_points, supply_points, rolloff)
  } else {
    ord <- ord |>
      dplyr::arrange(campaign_points, supply_points, rolloff)
  }

  ord |> dplyr::mutate(priority = dplyr::row_number())
}

# --- submission helpers (called from the player's phone) --------------
# These only RECORD intent. Nothing is applied until resolution. Each
# enforces the open-phase gate so a player can't submit out of phase.

assert_open <- function(con, phase) {
  cur <- phase_cursor(con)
  if (cur$current_phase != phase || cur$phase_status != "open")
    stop(sprintf("The %s phase is not open for submissions (currently %s/%s).",
                 phase, cur$current_phase, cur$phase_status), call. = FALSE)
  cur$current_round
}

submit_movement <- function(con, team_id, action, path = NULL, dest_hex = NULL) {
  round <- assert_open(con, "movement")
  stopifnot(action %in% c("manoeuvre", "regroup", "hold"))
  DBI::dbExecute(con,
    "INSERT INTO movements (round, team_id, action, path, dest_hex)
     VALUES ($1, $2, $3, $4, $5)
     ON CONFLICT (round, team_id) DO UPDATE
       SET action = EXCLUDED.action, path = EXCLUDED.path,
           dest_hex = EXCLUDED.dest_hex, submitted_at = now()",
    params = list(round, team_id, action,
                  if (is.null(path)) NA else paste0("{", paste(path, collapse = ","), "}"),
                  dest_hex %||% NA))
  invisible(TRUE)
}

submit_battle <- function(con, team_id, result, opponent_id = NULL, ops_incap = 0) {
  round <- assert_open(con, "battle")
  stopifnot(result %in% c("win", "draw", "loss", "bye"))
  DBI::dbExecute(con,
    "INSERT INTO battles (round, team_id, opponent_id, result, ops_incap)
     VALUES ($1, $2, $3, $4, $5)
     ON CONFLICT (round, team_id) DO UPDATE
       SET opponent_id = EXCLUDED.opponent_id, result = EXCLUDED.result,
           ops_incap = EXCLUDED.ops_incap, submitted_at = now()",
    params = list(round, team_id, opponent_id %||% NA, result, ops_incap))
  invisible(TRUE)
}

submit_action <- function(con, team_id, action, params = list()) {
  round <- assert_open(con, "action")
  stopifnot(action %in% c("scout", "resupply", "encamp", "search", "demolish"))
  DBI::dbExecute(con,
    "INSERT INTO actions (round, team_id, action, params)
     VALUES ($1, $2, $3, $4)
     ON CONFLICT (round, team_id) DO UPDATE
       SET action = EXCLUDED.action, params = EXCLUDED.params, submitted_at = now()",
    params = list(round, team_id, action,
                  jsonlite::toJSON(params, auto_unbox = TRUE)))
  invisible(TRUE)
}

# --- no-show handling (battle phase, p.55) ---------------------------
# The book records a player who could not attend as a loss (their
# opponent a win) and an odd player out as a bye (+2 SP). Per the
# facilitator's choice, the engine never applies these automatically:
# resolve_battle() stops and lists teams with no result; the facilitator
# reviews and applies a default per team, then resolves.

apply_battle_default <- function(con, team_id, result, opponent_id = NULL) {
  stopifnot(result %in% c("win", "draw", "loss", "bye"))
  submit_battle(con, team_id, result, opponent_id)   # reuses the open-phase write
  log_event(con, phase_cursor(con)$current_round, team_id,
            "battle_default_applied", list(result = result, by = "facilitator"))
  invisible(TRUE)
}

# --- resolution -------------------------------------------------------
# resolve_current_phase() dispatches on the cursor's phase. Each resolver
# is wrapped in a transaction so a phase applies all-or-nothing. The
# facilitator may resolve even with submissions missing: a team with no
# movement defaults to Hold; a team with no battle is treated per p.55
# (recorded as a loss if they could not attend — surfaced for the
# facilitator to confirm, not auto-applied silently).

resolve_current_phase <- function(con, force = FALSE) {
  cur <- phase_cursor(con)
  if (cur$phase_status == "resolved")
    stop("Current phase is already resolved; call advance_phase().", call. = FALSE)

  resolver <- switch(cur$current_phase,
    movement = resolve_movement,
    battle   = resolve_battle,
    action   = resolve_action,
    threat   = resolve_threat)

  DBI::dbWithTransaction(con, {
    resolver(con, round = cur$current_round, threat_level = cur$threat_level, force = force)
    DBI::dbExecute(con,
      "UPDATE campaign_state SET phase_status = 'resolved'")
  })
  invisible(phase_cursor(con))
}

resolve_movement <- function(con, round, threat_level, force = FALSE) {
  subs <- DBI::dbGetQuery(con,
    "SELECT team_id, action, path, dest_hex FROM movements WHERE round = $1",
    params = list(round)) |> tibble::as_tibble()

  ord <- priority_order(con) |> dplyr::filter(team_id %in% subs$team_id)

  for (tid in ord$team_id) {
    s <- dplyr::filter(subs, team_id == tid)
    if (s$action == "hold") {
      cost <- 0L; dest <- NULL
    } else if (s$action == "manoeuvre") {
      # SP cost is derived server-side: 1 per hex actually moved through.
      hexes <- if (is.na(s$path)) integer(0) else as.integer(strsplit(gsub("[{}]", "", s$path), ",")[[1]])
      cost <- length(hexes)
      dest <- s$dest_hex
      # NB (debug/add): path validation — no blocked hexes, <=3 hexes,
      # dest not already holding two kill teams, special move costs
      # (Asteroid Impact SL24, Transeptum Maze TL22) — hooks here.
    } else { # regroup: free move to nearest base/camp
      cost <- 0L; dest <- s$dest_hex
    }

    if (cost > 0)
      DBI::dbExecute(con,
        "UPDATE teams SET supply_points = GREATEST(supply_points - $2, 0)
         WHERE team_id = $1", params = list(tid, cost))
    if (!is.null(dest) && !is.na(dest)) {
      DBI::dbExecute(con,
        "UPDATE teams SET current_hex = $2 WHERE team_id = $1",
        params = list(tid, dest))
      explored <- DBI::dbGetQuery(con,
        "SELECT explored FROM hexes WHERE hex_uid = $1", params = list(dest))$explored
      if (isFALSE(explored)) {
        DBI::dbExecute(con,
          "UPDATE movements SET explore_needed = TRUE WHERE round = $1 AND team_id = $2",
          params = list(round, tid))
        # Standard single-draw exploration commits immediately (exploration.R).
        # Locations granting a choice (Observation Tower SL26) are explored
        # via explore_hex(n_options = 2) from the UI instead.
        explore_hex(con, dest, via = "move", round = round, team_id = tid)
      }
    }
    DBI::dbExecute(con,
      "UPDATE movements SET sp_spent = $3, resolved_order = $4, resolved_at = now()
       WHERE round = $1 AND team_id = $2",
      params = list(round, tid, cost, which(ord$team_id == tid)))
    log_event(con, round, tid, "movement_resolved",
              list(action = s$action, sp_spent = cost, dest = dest %||% NA))
  }
}

resolve_battle <- function(con, round, threat_level, force = FALSE) {
  missing <- missing_submissions(con, "battle", round)
  if (length(missing) > 0 && !force)
    stop(sprintf(
      paste0("Battle phase has no result for team(s): %s.\n",
             "Apply a default per team with apply_battle_default() ",
             "(p.55: a no-show is recorded as a loss, an odd player out as a bye), ",
             "then resolve. To proceed without defaults, resolve with force = TRUE."),
      paste(missing, collapse = ", ")), call. = FALSE)

  subs <- DBI::dbGetQuery(con,
    "SELECT team_id, result FROM battles WHERE round = $1",
    params = list(round)) |> tibble::as_tibble()

  for (i in seq_len(nrow(subs))) {
    tid <- subs$team_id[i]; res <- subs$result[i]
    if (res == "win") {
      DBI::dbExecute(con,
        "UPDATE teams SET campaign_points = campaign_points + 1 WHERE team_id = $1",
        params = list(tid))
    } else if (res == "bye") {
      DBI::dbExecute(con,
        "UPDATE teams SET supply_points = LEAST(supply_points + 2, 10) WHERE team_id = $1",
        params = list(tid))
    } else { # draw or loss
      DBI::dbExecute(con,
        "UPDATE teams SET supply_points = LEAST(supply_points + 1, 10) WHERE team_id = $1",
        params = list(tid))
    }
    DBI::dbExecute(con,
      "UPDATE battles SET resolved_at = now() WHERE round = $1 AND team_id = $2",
      params = list(round, tid))
    log_event(con, round, tid, "battle_resolved", list(result = res))
  }
}

resolve_action <- function(con, round, threat_level, force = FALSE) {
  subs <- DBI::dbGetQuery(con,
    "SELECT team_id, action, params FROM actions WHERE round = $1",
    params = list(round)) |> tibble::as_tibble()

  # Action priority groups by battle result first (p.55): winners, then
  # drawers, then losers; standard p.53 tiebreak applies within each group.
  results <- DBI::dbGetQuery(con,
    "SELECT team_id, result FROM battles WHERE round = $1",
    params = list(round)) |>
    tibble::as_tibble() |>
    dplyr::mutate(group_rank = dplyr::case_when(
      result == "win"  ~ 1L,
      result %in% c("draw", "bye") ~ 2L,
      TRUE ~ 3L)) |>
    dplyr::select(team_id, group_rank)

  ord <- priority_order(con, secondary = results) |>
    dplyr::filter(team_id %in% subs$team_id)

  for (tid in ord$team_id) {
    s <- dplyr::filter(subs, team_id == tid)
    params <- jsonlite::fromJSON(s$params)
    outcome <- apply_action(con, round, tid, s$action, params)  # effects.R
    DBI::dbExecute(con,
      "UPDATE actions SET resolved_order = $3, outcome = $4, resolved_at = now()
       WHERE round = $1 AND team_id = $2",
      params = list(round, tid, which(ord$team_id == tid),
                    jsonlite::toJSON(outcome, auto_unbox = TRUE)))
    log_event(con, round, tid, "action_resolved",
              list(action = s$action, outcome = outcome))
  }
}

resolve_threat <- function(con, round, threat_level, force = FALSE) {
  # 1) Resolve Threat-phase location rules in priority order (p.56). These
  #    live in the effects registry keyed by trigger = "threat" (e.g. Beast
  #    Lair SL36, Energy Hot Spot TL26, Hyperfractal Gaol TL32). effects.R.
  apply_threat_triggers(con, round)

  # 2) Raise threat. Competitive: +1 (p.56). Solo/co-op rises differently
  #    (p.57) and is handled by a separate solo engine — branch hook here.
  DBI::dbExecute(con,
    "UPDATE campaign_state SET threat_level = threat_level + 1")
  log_event(con, round, NA, "threat_raised",
            list(from = threat_level, to = threat_level + 1L))
}

# --- advancement ------------------------------------------------------
# Moves the cursor to the next phase (opening it), or to the next round's
# movement phase after threat. The campaign ends when threat hits its max.

advance_phase <- function(con, max_threat = 7L) {
  cur <- phase_cursor(con)
  if (cur$phase_status != "resolved")
    stop("Resolve the current phase before advancing.", call. = FALSE)

  if (cur$current_phase == "threat") {
    if (cur$threat_level >= max_threat) {
      DBI::dbExecute(con, "UPDATE campaign_state SET phase_status = 'resolved'")
      log_event(con, cur$current_round, NA, "campaign_ended",
                list(final_threat = cur$threat_level))
      return(invisible(phase_cursor(con)))
    }
    DBI::dbExecute(con,
      "UPDATE campaign_state
         SET current_round = current_round + 1,
             current_phase = 'movement', phase_status = 'open'")
  } else {
    nxt <- PHASES[match(cur$current_phase, PHASES) + 1L]
    DBI::dbExecute(con,
      "UPDATE campaign_state SET current_phase = $1, phase_status = 'open'",
      params = list(nxt))
  }
  invisible(phase_cursor(con))
}
