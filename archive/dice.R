# =====================================================================
# Ctesiphus Expedition — dice
# ---------------------------------------------------------------------
# d36() implements the book's compound roll (p.58): roll one D3, then one
# D6, and read them as a two-digit row (e.g. a 2 and a 5 -> row 25). This
# yields the 18 valid rows 11-16, 21-26, 31-36 used by every exploration
# table. d3()/d6() are the plain dice the location/condition rules call.
# =====================================================================

d6  <- function(n = 1L) sample.int(6L, n, replace = TRUE)
d3  <- function(n = 1L) sample.int(3L, n, replace = TRUE)
d36 <- function(n = 1L) 10L * d3(n) + d6(n)
