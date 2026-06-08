# =====================================================================
# Ctesiphus Expedition — map catalogue and weighted selection
# ---------------------------------------------------------------------
# The campaign board is chosen semi-randomly at setup, weighted by the
# number of players: more players biases toward larger boards. This
# follows the rulebook note (p.52) that the smaller maps suit 2-3
# players.
# =====================================================================

library(dplyr)
library(tibble)

# --- the five printed maps (pp. 68-72) -------------------------------
# `hex_count` is the only field the weighting uses; `page` is reference.
map_catalogue <- tibble::tribble(
  ~map_id, ~page, ~hex_count,
  1L,      68L,   26L,
  2L,      69L,   33L,
  3L,      70L,   36L,
  4L,      71L,   37L,
  5L,      72L,   52L
)

# --- weighting -------------------------------------------------------
# Each player count implies an "ideal" board size. Maps are weighted by
# a Gaussian on the gap between their hex count and that ideal, so the
# best-fit board is favoured but the others keep real probability.
#
# `spread` is the semi-random dial: larger -> flatter (more random),
# smaller -> sharper (more deterministic toward the best-fit map).
#
# To hand-set weights instead, replace map_weights() with your own
# tibble of (map_id, probability) keyed on n_players.

ideal_hexes <- function(n_players) {
  n <- pmin(pmax(n_players, 2), 6)            # clamp to the supported 2-6 range
  26 + (52 - 26) * (n - 2) / (6 - 2)          # 2 -> 26, 4 -> 39, 6 -> 52
}

map_weights <- function(n_players, spread = 8, catalogue = map_catalogue) {
  ideal <- ideal_hexes(n_players)
  catalogue |>
    dplyr::mutate(
      weight      = exp(-((hex_count - ideal)^2) / (2 * spread^2)),
      probability = weight / sum(weight)
    )
}

# --- the draw --------------------------------------------------------
# Set a seed beforehand (and log it to event_log) to make the campaign's
# board draw auditable and reproducible.
choose_map <- function(n_players, spread = 8, catalogue = map_catalogue) {
  w <- map_weights(n_players, spread, catalogue)
  sample(w$map_id, size = 1, prob = w$probability)
}
