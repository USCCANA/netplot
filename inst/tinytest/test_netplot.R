
# Placeholder with simple test
expect_equal(1 + 1, 2)

expect_equal(
  netplot:::rescale_size(c(.1, .2, .3), rel = NULL),
  c(.1, .2, .3),
  info = "NULL range should suppress size scaling"
)

# Test that edge.width values are reflected in the line-grob lwd gpar
if (requireNamespace("igraph", quietly = TRUE)) {
  set.seed(1)
  x <- igraph::make_ring(4, directed = FALSE)
  igraph::E(x)$weight <- c(1, 2, 3, 4)
  l <- igraph::layout_in_circle(x)

  g <- nplot(x, layout = l, skip.arrows = TRUE,
             edge.width = igraph::E(x)$weight,
             edge.width.range = c(1, 4))

  lwds <- get_edge_gpar(g, element = "line", "lwd")$lwd

  # All lwd values should be within the specified range
  expect_true(all(lwds >= 1 & lwds <= 4),
              info = "edge.width values should be mapped to edge.width.range")

  # Edges with larger weights should have larger (or equal) lwd
  expect_true(all(diff(lwds) >= 0),
              info = "lwd values should be non-decreasing with increasing weight")

  g_raw <- nplot(x, layout = l, skip.arrows = TRUE,
                 vertex.size = rep(.05, 4),
                 vertex.size.range = NULL,
                 edge.width = igraph::E(x)$weight,
                 edge.width.range = NULL)

  raw_lwds <- get_edge_gpar(g_raw, element = "line", "lwd")$lwd
  expect_equal(
    raw_lwds,
    igraph::E(x)$weight,
    info = "edge.width.range = NULL should use edge.width values as is"
  )

  frame <- g_raw$children$graph$children$vertex.1$children$frame
  frame_xy <- cbind(as.numeric(frame$x), as.numeric(frame$y))
  vertex_radius <- max(sqrt(rowSums(
    (frame_xy - matrix(g_raw$.layout[1, ], nrow(frame_xy), 2, byrow = TRUE))^2
  )))

  expect_equal(
    vertex_radius,
    .05,
    tolerance = 1e-8,
    info = "vertex.size.range = NULL should use vertex.size values as is"
  )

  base_raw <- nplot_base(x, layout = l, skip.arrows = TRUE,
                         vertex.size = rep(.05, 4),
                         vertex.size.range = NULL,
                         edge.width = igraph::E(x)$weight,
                         edge.width.range = NULL)

  expect_equal(
    base_raw$edge.width,
    igraph::E(x)$weight,
    info = "nplot_base should also suppress edge-width scaling for NULL range"
  )
}

if (requireNamespace("network", quietly = TRUE)) {
  x_network <- network::network(
    matrix(c(
      0, 1, 0, 1,
      1, 0, 1, 0,
      0, 1, 0, 1,
      1, 0, 1, 0
    ), nrow = 4, byrow = TRUE),
    directed = FALSE,
    matrix.type = "adjacency"
  )

  l_network <- cbind(
    cos(seq(0, 2*pi, length.out = 5)[-5]),
    sin(seq(0, 2*pi, length.out = 5)[-5])
  )

  g_network <- nplot(x_network, layout = l_network, skip.arrows = TRUE,
                     edge.width.range = NULL)

  network_lwds <- get_edge_gpar(g_network, element = "line", "lwd")$lwd
  expect_equal(
    network_lwds,
    rep(1, g_network$.M),
    info = "nplot.network should use width 1 when no edge weight is present"
  )
}
