#' Movement-phase engine for the Ctesiphus Expedition (rulebook pp. 53-54)
#'
#' Depends, sourced first, on: dice.R, exploration_tables.R, explore.R, hexmap.R.
#'
#' Unlike actions.R (which resolves ONE team's single action), the Movement
#' phase is inherently multi-team: declarations are resolved in priority order,
#' and a hex may hold at most two kill teams, so where an earlier-priority team
#' ends can make a later team's declared destination illegal. This file owns
#' that cross-team resolution.
#'
#' BOARD CONTRACT
#' -------------
#' `board` is a list:
#'   $map      tibble as in actions.R (hex_number, type, q, r, explored,
#'             location_code, condition_code, supply_remaining, intel_remaining)
#'   $teams    named list keyed by team id; each a team list as in actions.R
#'             (hex, supply, campaign, base, camps, flags)
#'   $priority character vector of team ids in resolution order (see
#'             team_priority(); least CP, then least SP, then roll-off)
#'
#' `declarations` is a named list keyed by team id:
#'   list(action = "manoeuvre" | "regroup" | "hold",
#'        path   = <int vector of hex_numbers ENTERED, in order; manoeuvre only>)
#' A missing declaration is resolved as Hold (the no-show default).
#'
#' resolve_movement_phase() returns:
#'   list(board   = <updated board>,
#'        resolved = tibble(team, action, cost, dest, order, ok, reason),
#'        log     = <chr vector>,
#'        needs_input = <named list of unresolved declarations for the UI>)

library(dplyr)

# ---- geometry / cost helpers ------------------------------------------------
.xy <- function(map, h) c(q = map$q[map$hex_number == h], r = map$r[map$hex_number == h])

are_adjacent <- function(map, a, b) {
  ca <- .xy(map, a); cb <- .xy(map, b)
  any(vapply(AXIAL_DIRS, function(d)
    ca[["q"]] + d[1] == cb[["q"]] && ca[["r"]] + d[2] == cb[["r"]], logical(1)))
}

# Extra SP for the step that LEAVES the current hex (rulebook location rules):
# Asteroid Impact (SL24): +1 to enter a surface hex, -1 to enter a tomb hex.
# Transeptum Maze (TL22): +1 SP to leave.
.leave_modifier <- function(from_loc, entered_type) {
  if (identical(from_loc, "SL24")) return(if (entered_type == "tomb") -1L else 1L)
  if (identical(from_loc, "TL22")) return(1L)
  0L
}

# Manoeuvre cost: 1 SP per hex entered, with the leave-modifier on the first
# step only (you exit the current hex once). A single adjacent tomb step out of
# SL24 is therefore free.
manoeuvre_cost <- function(map, from_hex, path) {
  if (length(path) == 0L) return(0L)
  from_loc   <- map$location_code[map$hex_number == from_hex]
  first_type <- map$type[map$hex_number == path[1]]
  max(0L, length(path) + .leave_modifier(from_loc, first_type))
}

# Nearest base/own-camp by graph distance (excluding blocked); NA if none reach.
nearest_reachable_site <- function(map, from_hex, sites) {
  sites <- unique(sites)
  if (length(sites) == 0L) return(NA_integer_)
  d <- vapply(sites, function(s) as.numeric(hex_distance(map, from_hex, to = s)), numeric(1))
  if (all(is.infinite(d))) return(NA_integer_)
  as.integer(sites[which.min(d)])
}

# Live occupancy: how many kill teams currently sit on each hex.
.occupancy <- function(teams) {
  hx <- vapply(teams, function(t) as.integer(t$hex), integer(1))
  tab <- table(hx)
  setNames(as.integer(tab), names(tab))
}
.occ_at <- function(occ, hex) { v <- occ[as.character(hex)]; if (is.na(v)) 0L else v }

# ---- priority order (rulebook p.53) -----------------------------------------
#' Resolution order: least Campaign points, then least Supply points, then a
#' random roll-off. Returns team ids ordered first (highest priority) to last.
team_priority <- function(teams) {
  tibble::tibble(
    team     = names(teams),
    campaign = vapply(teams, function(t) as.integer(t$campaign), integer(1)),
    supply   = vapply(teams, function(t) as.integer(t$supply),   integer(1)),
    rolloff  = stats::runif(length(teams))
  ) |>
    dplyr::arrange(campaign, supply, rolloff) |>
    dplyr::pull(team)
}

# ---- explore-on-entry (mirrors apply_scout's map update in actions.R) -------
# A team finishing movement in an unexplored, non-blocked hex explores it
# (single draw - the "generate two, keep one" options are Scout/Search/Camp
# benefits, not vanilla movement entry).
.explore_into <- function(map, hex) {
  i <- which(map$hex_number == hex)
  if (isTRUE(map$explored[i]) || map$type[i] == "blocked") return(list(map = map, log = NULL))
  used_loc  <- na.omit(map$location_code)
  used_cond <- na.omit(map$condition_code)
  res <- explore_hex(map$type[i], used_loc, used_cond)     # from explore.R
  map$explored[i]       <- TRUE
  map$location_code[i]  <- res$location_code
  map$condition_code[i] <- res$condition_code
  if (isTRUE(res$becomes_blocked)) map$type[i] <- "blocked"
  if (!is.na(res$init_resource)) {
    if (res$init_resource == "supply") map$supply_remaining[i] <- res$init_amount
    if (res$init_resource == "intel")  map$intel_remaining[i]  <- res$init_amount
  }
  list(map = map,
       log = sprintf("  explored hex %s -> %s / %s%s", hex,
                     res$location_name, res$condition_name,
                     if (!is.na(res$init_resource))
                       sprintf(" (%s %d)", res$init_resource, res$init_amount) else ""))
}

# ---- per-team validation ----------------------------------------------------
can_manoeuvre <- function(board, team_id, path) {
  map <- board$map; team <- board$teams[[team_id]]
  if (length(path) < 1L || length(path) > 3L)
    return(list(ok = FALSE, reason = "Manoeuvre moves 1-3 hexes."))
  if (anyDuplicated(path))
    return(list(ok = FALSE, reason = "Path repeats a hex."))
  chain <- c(team$hex, path)
  for (j in seq_len(length(chain) - 1L)) {
    if (!are_adjacent(map, chain[j], chain[j + 1L]))
      return(list(ok = FALSE, reason = sprintf("Hexes %s and %s are not adjacent.",
                                               chain[j], chain[j + 1L])))
  }
  if (any(map$type[map$hex_number %in% path] == "blocked"))
    return(list(ok = FALSE, reason = "Path passes through a blocked hex."))
  cost <- manoeuvre_cost(map, team$hex, path)
  if (team$supply < cost)
    return(list(ok = FALSE, reason = sprintf("Not enough SP (need %d).", cost)))
  list(ok = TRUE, reason = "", cost = cost, dest = path[length(path)])
}

can_regroup <- function(board, team_id) {
  map <- board$map; team <- board$teams[[team_id]]
  dest <- nearest_reachable_site(map, team$hex, c(team$base, team$camps))
  if (is.na(dest)) return(list(ok = FALSE, reason = "No reachable base or camp."))
  list(ok = TRUE, reason = "", cost = 0L, dest = dest)
}

# ---- phase resolution -------------------------------------------------------
resolve_movement_phase <- function(board, declarations = list()) {
  map  <- board$map
  occ  <- .occupancy(board$teams)
  order <- board$priority %||% team_priority(board$teams)
  log <- character(0); rows <- list(); needs <- list()

  for (k in seq_along(order)) {
    tid  <- as.character(order[k])
    team <- board$teams[[tid]]
    d    <- declarations[[tid]]
    act  <- if (is.null(d)) "hold" else d$action

    if (act == "hold") {
      rows[[tid]] <- tibble::tibble(team = tid, action = "hold", cost = 0L,
                                    dest = team$hex, order = k, ok = TRUE, reason = "")
      log <- c(log, sprintf("%s: hold at hex %s.", tid, team$hex))
      next
    }

    chk <- if (act == "manoeuvre") can_manoeuvre(board, tid, d$path) else can_regroup(board, tid)
    if (!chk$ok) {                                   # illegal as declared -> ask the UI
      needs[[tid]] <- list(requires = "redeclare_movement", reason = chk$reason)
      rows[[tid]] <- tibble::tibble(team = tid, action = act, cost = NA_integer_,
                                    dest = team$hex, order = k, ok = FALSE, reason = chk$reason)
      log <- c(log, sprintf("%s: %s rejected - %s", tid, act, chk$reason))
      next
    }

    # two-kill-team cap on the destination (live, in priority order)
    if (.occ_at(occ, chk$dest) >= 2L && chk$dest != team$hex) {
      msg <- sprintf("Destination hex %s already holds two kill teams.", chk$dest)
      needs[[tid]] <- list(requires = "redeclare_movement", reason = msg)
      rows[[tid]] <- tibble::tibble(team = tid, action = act, cost = NA_integer_,
                                    dest = team$hex, order = k, ok = FALSE, reason = msg)
      log <- c(log, sprintf("%s: %s blocked - %s", tid, act, msg))
      next
    }

    # commit the move
    occ[as.character(team$hex)] <- .occ_at(occ, team$hex) - 1L
    occ[as.character(chk$dest)] <- .occ_at(occ, chk$dest) + 1L
    board$teams[[tid]]$hex    <- chk$dest
    board$teams[[tid]]$supply <- team$supply - chk$cost
    log <- c(log, sprintf("%s: %s to hex %s for %d SP.", tid, act, chk$dest, chk$cost))

    ex <- .explore_into(board$map, chk$dest)         # explore on entry, if needed
    board$map <- ex$map
    if (!is.null(ex$log)) log <- c(log, ex$log)

    rows[[tid]] <- tibble::tibble(team = tid, action = act, cost = chk$cost,
                                  dest = chk$dest, order = k, ok = TRUE, reason = "")
  }

  board$map <- board$map %||% map
  list(board = board,
       resolved = dplyr::bind_rows(rows),
       log = log,
       needs_input = if (length(needs)) needs else NULL)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
