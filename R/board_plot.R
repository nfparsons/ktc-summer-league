#' Base-R hex map of the current board. Fill encodes hex type; a team's base
#' hex is outlined in amber and labelled. No extra package dependency.
plot_board <- function(map, teams = list()) {
  size <- 1
  cx <-  sqrt(3) * (map$q + map$r / 2) * size
  cy <- -1.5 * map$r * size                          # invert so it reads top-down
  ang <- (0:5) * pi / 3 + pi / 6                     # pointy-top vertices
  hx <- size * 0.92 * cos(ang); hy <- size * 0.92 * sin(ang)

  base_name <- setNames(
    vapply(teams, function(t) t$name %||% "", character(1)),
    vapply(teams, function(t) as.character(t$base %||% NA), character(1)))

  op <- par(bg = "#0b0e0d", mar = c(0, 0, 0, 0)); on.exit(par(op))
  plot.new(); plot.window(xlim = range(cx) + c(-1.5, 1.5),
                          ylim = range(cy) + c(-1.5, 1.5), asp = 1)
  for (i in seq_len(nrow(map))) {
    fill <- switch(map$type[i], surface = "#1d3b2c", tomb = "#2b2552",
                   blocked = "#14181b", "#1d3b2c")
    hn <- as.character(map$hex_number[i]); is_base <- hn %in% names(base_name)
    polygon(cx[i] + hx, cy[i] + hy, col = fill,
            border = if (is_base) "#e0a44d" else "#2f4a3e",
            lwd = if (is_base) 3 else 1)
    text(cx[i], cy[i], map$hex_number[i], col = "#cfe8d8", cex = 0.75)
    if (is_base) text(cx[i], cy[i] - 0.42, base_name[[hn]], col = "#e0a44d", cex = 0.55)
  }
}
