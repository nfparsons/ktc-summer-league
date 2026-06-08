# Dice primitives for the Ctesiphus Expedition campaign engine.
# All rollers return integers. Pass an explicit `n` to roll several at once.

roll_d3 <- function(n = 1L) sample.int(3L, n, replace = TRUE)
roll_d6 <- function(n = 1L) sample.int(6L, n, replace = TRUE)

# A "D36" is one D3 (the tens digit) combined with one D6 (the units digit):
# a D3 of 2 and a D6 of 5 reads as 25. Valid results: 11-16, 21-26, 31-36.
roll_d36 <- function() 10L * sample.int(3L, 1L) + sample.int(6L, 1L)

# Resolve simple dice notation ("d6", "2d6", "d3", "3d3", ...) to a single total.
# Returns NA for an NA/empty input so it composes cleanly with table columns.
roll_notation <- function(notation) {
  if (length(notation) != 1L) stop("roll_notation() is not vectorised; pass one string.")
  if (is.na(notation) || notation == "") return(NA_integer_)
  m <- regmatches(notation, regexec("^([0-9]*)d([0-9]+)$", tolower(notation)))[[1]]
  if (length(m) != 3L) stop(sprintf("Unrecognised dice notation: '%s'", notation))
  n     <- if (m[2L] == "") 1L else as.integer(m[2L])
  sides <- as.integer(m[3L])
  sum(sample.int(sides, n, replace = TRUE))
}
