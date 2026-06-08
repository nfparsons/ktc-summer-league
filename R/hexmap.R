#' Hex-grid geometry and pathfinding for the Ctesiphus Expedition engine
#'
#' A map is a tibble with at least: hex_number (int), type ("surface" /
#' "tomb" / "blocked"), q (int), r (int). Coordinates are axial and the same
#' six neighbour offsets apply to every one of the five campaign maps - the
#' per-map vertical phase is already baked into q/r. Blocked hexes can be
#' neither entered nor passed through.

library(dplyr)

#' The six axial neighbour offsets shared by every campaign map.
AXIAL_DIRS <- list(c(0L, -1L), c(0L, 1L), c(1L, -1L),
                   c(1L, 0L), c(-1L, 0L), c(-1L, 1L))

#' Axial coordinates of a hex's six neighbours (pure geometry; ignores the map).
#' @return a list of length-2 c(q, r) vectors.
hex_neighbours <- function(q, r) {
  lapply(AXIAL_DIRS, function(d) c(q = q + d[1], r = r + d[2]))
}

#' Straight-line hex distance between two axial coordinates (ignores terrain).
#' Useful for "within N hexes" effects that are meant as raw distance rather
#' than a walkable path (currently none are wired to this; see hex_distance).
hex_cube_distance <- function(q1, r1, q2, r2) {
  (abs(q1 - q2) + abs(q1 + r1 - (q2 + r2)) + abs(r1 - r2)) %/% 2L
}

#' Graph distance, in hexes, moving only through non-blocked hexes.
#'
#' This is the "excluding blocked hexes" distance the Encamp rule calls for, and
#' the measure used for Scout range (see note in actions.R).
#'
#' @param map a map tibble.
#' @param from_hex starting hex_number.
#' @param to optional hex_number(s). If supplied, returns the distance to the
#'   NEAREST of them (Inf if none reachable). If NULL, returns a named integer
#'   vector of distances (names are hex_number as character) to every reachable
#'   hex, including 0 for `from_hex` itself.
hex_distance <- function(map, from_hex, to = NULL) {
  idx <- setNames(map$hex_number, paste(map$q, map$r, sep = ","))
  qy  <- setNames(map$q, as.character(map$hex_number))
  ry  <- setNames(map$r, as.character(map$hex_number))
  blk <- setNames(map$type == "blocked", as.character(map$hex_number))

  dist <- setNames(rep(NA_integer_, nrow(map)), as.character(map$hex_number))
  dist[as.character(from_hex)] <- 0L
  frontier <- from_hex

  while (length(frontier) > 0) {
    nxt <- integer(0)
    for (h in frontier) {
      hc <- as.character(h)
      cq <- qy[hc]; cr <- ry[hc]
      for (d in AXIAL_DIRS) {
        nb <- idx[paste(cq + d[1], cr + d[2], sep = ",")]
        if (is.na(nb)) next
        nbc <- as.character(nb)
        if (isTRUE(blk[nbc])) next               # cannot pass through blocked
        if (is.na(dist[nbc])) {
          dist[nbc] <- dist[hc] + 1L
          nxt <- c(nxt, nb)
        }
      }
    }
    frontier <- nxt
  }

  if (is.null(to)) return(dist)
  d <- dist[as.character(to)]
  if (all(is.na(d))) Inf else min(d, na.rm = TRUE)
}

#' Hex_numbers reachable within `n` steps (graph distance, excluding blocked),
#' not counting the origin itself.
hexes_within <- function(map, from_hex, n) {
  d <- hex_distance(map, from_hex, to = NULL)
  as.integer(names(d)[!is.na(d) & d >= 1L & d <= n])
}
