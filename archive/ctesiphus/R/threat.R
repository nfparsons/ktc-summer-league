#' Threat-phase engine for the Ctesiphus Expedition (rulebook p.56)
#'
#' Depends, sourced first, on: dice.R, hexmap.R.
#'
#' Resolves threat-phase location/camp rules in priority order, then raises the
#' campaign threat by 1 (competitive play). Solo/co-op threat escalation (p.57)
#' is a separate engine. Multi-team and functional, on the BOARD contract from
#' movement.R, with two campaign-level additions:
#'   $threat      <int>  current threat level
#'   $max_threat  <int>  campaign ends when threat reaches this
#'   $tokens      <named list>  campaign tokens, incl. $prisoner_hex (TL32)
#'
#' Design choice: the beast (SL36) and the roaming prisoner (TL32) involve dice
#' and cross-team effects; they AUTO-RESOLVE here with every roll written to the
#' log (so async play is never blocked). The prisoner's "up to D3 hexes" roam
#' has no fixed direction in the rules, so it is a random walk over non-blocked
#' hexes - the one genuinely heuristic decision, flagged.
#'
#' resolve_threat_phase() returns:
#'   list(board, log, threat, ended [lgl], needs_input)

library(dplyr)

.loc_at  <- function(map, hex) { v <- map$location_code[map$hex_number == hex]; if (length(v)) v else NA_character_ }
.teams_on <- function(board, hex) names(board$teams)[vapply(board$teams, function(t) t$hex == hex, logical(1))]
.add_sp  <- function(board, tid, d) { board$teams[[tid]]$supply   <- max(0L, min(10L, board$teams[[tid]]$supply + d)); board }
.add_cp  <- function(board, tid, d) { board$teams[[tid]]$campaign <- board$teams[[tid]]$campaign + d; board }

# ---- camp benefits (each Threat phase, per team holding a camp on the hex) ---
.threat_camp_effect <- function(board, tid, hex) {
  loc <- .loc_at(board$map, hex); log <- character(0)
  i <- which(board$map$hex_number == hex)
  switch(loc %||% "",
    SL33 = , SL34 = , TL24 = { board <- .add_cp(board, tid, 1L)
      log <- sprintf("%s: camp at hex %s (%s) -> +1 CP.", tid, hex, loc) },
    TL26 = { board <- .add_cp(board, tid, 1L); board <- .add_sp(board, tid, 1L)
      log <- sprintf("%s: camp at hex %s (Energy Hot Spot) -> +1 CP, +1 SP.", tid, hex) },
    TL23 = {                                            # Crucible: D6, 5+ takes 1 CP from store
      store <- board$map$campaign_remaining[i]; store <- if (is.na(store)) 0L else store
      roll <- roll_d6()
      if (roll >= 5 && store > 0) { board <- .add_cp(board, tid, 1L); board$map$campaign_remaining[i] <- store - 1L
        log <- sprintf("%s: camp at hex %s (Crucible) rolled %d -> +1 CP (%d left).", tid, hex, roll, store - 1L)
      } else log <- sprintf("%s: camp at hex %s (Crucible) rolled %d -> nothing.", tid, hex, roll) },
    TL31 = {                                            # Vivitrophic: D3, 2+ gain SP = result
      roll <- roll_d3()
      if (roll >= 2) { board <- .add_sp(board, tid, roll)
        log <- sprintf("%s: camp at hex %s (Vivitrophic) rolled %d -> +%d SP.", tid, hex, roll, roll)
      } else log <- sprintf("%s: camp at hex %s (Vivitrophic) rolled 1 -> nothing.", tid, hex) },
    { }                                                 # no threat-phase camp rule
  )
  list(board = board, log = log)
}

# ---- Beast Lair (SL36): attacks players within 2 surface hexes --------------
.threat_beast <- function(board) {
  lair <- board$map$hex_number[!is.na(board$map$location_code) & board$map$location_code == "SL36"]
  if (length(lair) == 0) return(list(board = board, log = character(0)))
  log <- character(0)
  for (h in lair) {
    d <- hex_distance(board$map, h, to = NULL)
    in_range <- names(d)[!is.na(d) & d >= 1 & d <= 2]
    victims <- in_range[vapply(in_range, function(hx) board$map$type[board$map$hex_number == as.integer(hx)] == "surface", logical(1))]
    occupants <- unlist(lapply(as.integer(victims), function(hx) .teams_on(board, hx)))
    if (length(occupants) == 0) next
    if (length(occupants) == 1) {
      roll <- roll_d6()
      if (roll < 5) { loss <- roll_d6(); board <- .add_sp(board, occupants, -loss)
        log <- c(log, sprintf("Beast (hex %s): %s rolled %d (<5) -> loses %d SP.", h, occupants, roll, loss))
      } else log <- c(log, sprintf("Beast (hex %s): %s rolled %d -> avoids.", h, occupants, roll))
    } else {
      tot <- vapply(occupants, function(tid) {
        hx <- board$teams[[tid]]$hex; roll_d6() + min(2L, as.integer(d[as.character(hx)])) }, integer(1))
      loser <- occupants[which(tot == min(tot))]; loser <- loser[sample.int(length(loser), 1L)]
      loss <- roll_d6(); board <- .add_sp(board, loser, -loss)
      log <- c(log, sprintf("Beast (hex %s) roll-off: %s is the loser -> loses %d SP.", h, loser, loss))
    }
  }
  list(board = board, log = log)
}

# ---- Hyperfractal Gaol (TL32): roaming prisoner -----------------------------
.threat_prisoner <- function(board) {
  ph <- board$tokens$prisoner_hex
  if (is.null(ph) || is.na(ph)) return(list(board = board, log = character(0)))
  log <- character(0); steps <- roll_d3(); here <- ph
  for (s in seq_len(steps)) {                            # random walk over non-blocked hexes
    nbrs <- as.integer(names(hex_distance(board$map, here, to = NULL)))
    adj <- nbrs[vapply(nbrs, function(x) are_adjacent(board$map, here, x), logical(1))]
    adj <- adj[vapply(adj, function(x) board$map$type[board$map$hex_number == x] != "blocked", logical(1))]
    if (length(adj) == 0) break
    here <- adj[sample.int(length(adj), 1L)]
  }
  board$tokens$prisoner_hex <- here
  log <- c(log, sprintf("Prisoner roams %d hex(es): %s -> %s.", steps, ph, here))

  cost_incurred <- FALSE
  if (!identical(.loc_at(board$map, here), "TL22")) {     # Transeptum Maze is safe
    for (tid in .teams_on(board, here)) {
      loss <- roll_d6(); board <- .add_sp(board, tid, -loss); cost_incurred <- TRUE
      log <- c(log, sprintf("  prisoner at hex %s: %s loses %d SP.", here, tid, loss))
      hit_camp <- vapply(names(board$teams), function(o) o != tid && here %in% board$teams[[o]]$camps, logical(1))
      for (o in names(board$teams)[hit_camp]) {
        board$teams[[o]]$camps <- setdiff(board$teams[[o]]$camps, here); cost_incurred <- TRUE
        log <- c(log, sprintf("  prisoner removes %s's camp at hex %s.", o, here))
      }
    }
  }
  if (cost_incurred) {                                    # may leave the campaign
    leave <- roll_d6()
    if (leave >= 4) { board$tokens$prisoner_hex <- NULL
      log <- c(log, sprintf("  prisoner departs the campaign (rolled %d).", leave)) }
  }
  list(board = board, log = log)
}

# ---- phase resolution -------------------------------------------------------
resolve_threat_phase <- function(board) {
  order <- board$priority %||% team_priority(board$teams)
  log <- character(0)

  for (tid in order)
    for (camp_hex in board$teams[[tid]]$camps) {
      out <- .threat_camp_effect(board, tid, camp_hex)
      board <- out$board; if (length(out$log)) log <- c(log, out$log)
    }

  b <- .threat_beast(board);    board <- b$board; log <- c(log, b$log)
  p <- .threat_prisoner(board); board <- p$board; log <- c(log, p$log)

  board$threat <- (board$threat %||% 1L) + 1L
  ended <- board$threat >= (board$max_threat %||% 7L)
  log <- c(log, sprintf("Threat raised to %d%s.", board$threat,
                        if (ended) " - campaign ends." else ""))
  list(board = board, log = log, threat = board$threat, ended = ended, needs_input = NULL)
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
