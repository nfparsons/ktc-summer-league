#' Battle-phase engine for the Ctesiphus Expedition (rulebook pp. 54-55)
#'
#' Depends, sourced first, on: dice.R (only for any condition rolls).
#'
#' Battles are played asynchronously through the week and entered per team.
#' Scoring (p.54): a win -> +1 Campaign point; a draw or loss -> +1 Supply
#' point; an odd player out takes a bye -> +2 Supply points. Supply is capped
#' at 10; Campaign points are uncapped.
#'
#' No-show policy (facilitator-approved, not automatic): a team with no result
#' is surfaced in `needs_input` rather than silently scored. The facilitator
#' supplies a default per team (loss or bye) and re-resolves.
#'
#' RESULTS CONTRACT
#' ----------------
#' `results` is a named list keyed by team id:
#'   list(result    = "win" | "draw" | "loss" | "bye",
#'        opponent  = <team id | chr label | NULL>,
#'        hex       = <hex_number of the condition used | NULL>,
#'        ops_incap = <int weighted enemy operatives incapacitated, Headhunter>)
#'
#' resolve_battle_phase() returns:
#'   list(board, resolved = tibble(team, result, cp_gained, sp_gained, order),
#'        log, needs_input)

library(dplyr)

# Skull Mounds (SC26): the battle's winner gains 1 extra CP, at most once per
# campaign per player. The only condition with a campaign-layer payout; the
# other SC/TC conditions are in-game (tabletop) rules with no engine effect.
.condition_cp_bonus <- function(team, hex, map) {
  if (is.null(hex) || is.na(hex)) return(0L)
  cond <- map$condition_code[map$hex_number == hex]
  if (length(cond) == 0 || is.na(cond) || cond != "SC26") return(0L)
  if (isTRUE(team$flags$SC26_bonus_taken)) return(0L)
  1L
}

score_one <- function(result) {
  switch(result,
    win  = list(cp = 1L, sp = 0L),
    bye  = list(cp = 0L, sp = 2L),
    draw = list(cp = 0L, sp = 1L),
    loss = list(cp = 0L, sp = 1L),
    stop("Unknown battle result: ", result))
}

resolve_battle_phase <- function(board, results = list(), order = NULL) {
  order <- order %||% board$priority %||% team_priority(board$teams)
  ids   <- names(board$teams)

  missing <- setdiff(ids, names(results))
  if (length(missing) > 0)
    return(list(board = board, resolved = tibble::tibble(), log = character(0),
                needs_input = list(requires = "battle_defaults", teams = missing,
                  note = "No result for these teams. Supply a default per team (loss or bye) and re-resolve.")))

  rows <- list(); log <- character(0)
  for (k in seq_along(order)) {
    tid <- as.character(order[k])
    if (!tid %in% names(results)) next
    r <- results[[tid]]
    sc <- score_one(r$result)
    team <- board$teams[[tid]]

    bonus <- if (r$result == "win") .condition_cp_bonus(team, r$hex, board$map) else 0L
    if (bonus > 0) team$flags$SC26_bonus_taken <- TRUE
    cp <- sc$cp + bonus

    team$campaign <- team$campaign + cp
    team$supply   <- min(10L, team$supply + sc$sp)
    board$teams[[tid]] <- team

    rows[[tid]] <- tibble::tibble(team = tid, result = r$result,
                                  cp_gained = cp, sp_gained = sc$sp, order = k)
    log <- c(log, sprintf("%s: %s -> +%d CP, +%d SP%s", tid, r$result, cp, sc$sp,
                          if (bonus > 0) " (Skull Mounds bonus)" else ""))
  }

  list(board = board, resolved = dplyr::bind_rows(rows), log = log, needs_input = NULL)
}

#' Build a `results` entry for a no-show default (facilitator-approved).
battle_default <- function(result = c("loss", "bye"), opponent = NULL) {
  result <- match.arg(result)
  list(result = result, opponent = opponent, hex = NULL, ops_incap = 0L)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
