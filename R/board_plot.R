#' Base-R hex map of the current board, sized to stay readable in the app.
#' Light surface hexes / mid-grey tomb / near-black blocked on a dark-grey
#' field; a team's base hex is outlined and labelled in Kill Team orange.
#' asp = 1 keeps hexes regular; give the plotOutput plenty of height.
plot_board <- function(map, teams = list()) {
  size <- 1
  cx <-  sqrt(3) * (map$q + map$r / 2) * size
  cy <- -1.5 * map$r * size                          # invert so it reads top-down
  ang <- (0:5) * pi / 3 + pi / 6                     # pointy-top vertices
  hx <- size * 0.92 * cos(ang); hy <- size * 0.92 * sin(ang)

  base_name <- setNames(
    vapply(teams, function(t) t$name %||% "", character(1)),
    vapply(teams, function(t) as.character(t$base %||% NA), character(1)))

  op <- par(bg = "#16161a", mar = c(0, 0, 1.6, 0)); on.exit(par(op))
  plot.new(); plot.window(xlim = range(cx) + c(-1.5, 1.5),
                          ylim = range(cy) + c(-1.5, 1.5), asp = 1)
  mtext("surface  |  tomb  |  blocked   -   base outlined in orange",
        side = 3, line = 0.2, col = "#8a8a90", cex = 0.8)
  for (i in seq_len(nrow(map))) {
    ty   <- map$type[i]
    fill <- switch(ty, surface = "#d9d7d2", tomb = "#4b4b50", blocked = "#0e0e12", "#d9d7d2")
    tcol <- switch(ty, surface = "#16161a", tomb = "#e8e6e2", blocked = "#6a6a70", "#16161a")
    hn <- as.character(map$hex_number[i]); is_base <- hn %in% names(base_name)
    polygon(cx[i] + hx, cy[i] + hy, col = fill,
            border = if (is_base) "#e0571c" else "#3a3a40",
            lwd    = if (is_base) 4 else 1)
    text(cx[i], cy[i], map$hex_number[i],
         col = if (is_base) "#e0571c" else tcol, cex = 1.05, font = 2)
    if (is_base && nzchar(base_name[[hn]]))
      text(cx[i], cy[i] - 0.5, base_name[[hn]], col = "#e0571c", cex = 0.7, font = 2)
  }
}
