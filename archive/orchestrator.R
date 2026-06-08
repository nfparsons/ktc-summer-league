#' Round orchestrator for the Ctesiphus Expedition
#'
#' Depends, sourced first, on: dice.R, hexmap.R, exploration_tables.R, explore.R,
#' actions.R, movement.R, battle.R, threat.R.
#'
#' The Movement/Battle/Threat resolvers already operate on the multi-team
#' `board`. The Action phase lives in actions.R as a PER-TEAM resolver on a
#' `state` list, so this file (a) computes the won -> drew -> lost action order
#' (p.55), and (b) bridges each team's slice of the board into the actions.R
#' `state` and folds the result back - including translating a Demolish (which
#' edits the actor's `opp_sites` view) into the owning team's camp list.
#'
#' A campaign round is then: resolve_movement_phase -> resolve_battle_phase ->
#' run_action_phase -> resolve_threat_phase, each gated by the persistence layer.

library(dplyr)

# ---- action order: win, then draw/bye, then loss; priority within tier ------
action_order <- function(board, round_results = list()) {
  base <- team_priority(board$teams)                       # best-first priority
  tier <- function(tid) {
    r <- round_results[[tid]]$result %||% "loss"
    if (r == "win") 1L else if (r %in% c("draw", "bye")) 2L else 3L
  }
  base[order(vapply(base, tier, integer(1)), seq_along(base))]
}

# ---- board slice <-> actions.R per-team state -------------------------------
.opp_sites_for <- function(board, tid) {
  rows <- list()
  for (o in setdiff(names(board$teams), tid)) {
    ot <- board$teams[[o]]
    rows[[length(rows) + 1L]] <- tibble::tibble(hex_number = ot$base, kind = "base", owner = o)
    for (cmp in ot$camps)
      rows[[length(rows) + 1L]] <- tibble::tibble(hex_number = cmp, kind = "camp", owner = o)
  }
  if (length(rows)) dplyr::bind_rows(rows)
  else tibble::tibble(hex_number = integer(0), kind = character(0), owner = character(0))
}

.beaten_for <- function(tid, round_results) {
  r <- round_results[[tid]]
  if (!is.null(r) && identical(r$result, "win") && !is.null(r$opponent)) as.character(r$opponent)
  else character(0)
}

# ---- the multi-team Action phase --------------------------------------------
#' @param declarations named list keyed by team id; each:
#'   list(action, sp_spent = NULL, target_hex = NULL, remove_camp = NULL)
#'   A missing entry = that team takes no action (the Action is optional).
#' @param round_results named list keyed by team id: list(result, opponent).
run_action_phase <- function(board, declarations = list(), round_results = list()) {
  order <- action_order(board, round_results)
  log <- character(0); rows <- list(); needs <- list()

  for (tid in order) {
    d <- declarations[[tid]]
    if (is.null(d) || is.null(d$action)) next

    opp_before <- .opp_sites_for(board, tid)
    st <- list(map = board$map, team = board$teams[[tid]], opp_sites = opp_before,
               round = list(beaten = .beaten_for(tid, round_results), walkover = character(0)))

    res <- perform_action(st, d$action, sp_spent = d$sp_spent,
                          target_hex = d$target_hex, remove_camp = d$remove_camp)

    board$map           <- res$state$map
    board$teams[[tid]]  <- res$state$team

    # fold a Demolish back: removed opponent camp -> drop from that owner's list
    removed <- dplyr::anti_join(opp_before, res$state$opp_sites,
                                by = c("hex_number", "kind", "owner"))
    for (j in seq_len(nrow(removed)))
      if (removed$kind[j] == "camp")
        board$teams[[removed$owner[j]]]$camps <-
          setdiff(board$teams[[removed$owner[j]]]$camps, removed$hex_number[j])

    if (!is.null(res$needs_input)) needs[[tid]] <- res$needs_input
    rows[[tid]] <- tibble::tibble(team = tid, action = d$action, cost = res$cost,
                                  ok = res$ok, reason = res$reason)
    if (nzchar(res$log)) log <- c(log, sprintf("%s: %s", tid, res$log))
    else if (!res$ok)    log <- c(log, sprintf("%s: %s rejected - %s", tid, d$action, res$reason))
  }

  list(board = board, resolved = dplyr::bind_rows(rows), log = log,
       needs_input = if (length(needs)) needs else NULL)
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
