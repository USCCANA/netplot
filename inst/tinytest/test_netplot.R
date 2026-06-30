
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

  g_rot <- nplot(x, layout = l, skip.edges = TRUE, skip.arrows = TRUE,
                 vertex.size = rep(.05, 4),
                 vertex.size.range = NULL,
                 vertex.frame.prop = 0,
                 vertex.nsides = rep(3, 4),
                 vertex.rot = pi/4)
  core <- g_rot$children$graph$children$vertex.1$children$core
  core_xy <- unname(cbind(as.numeric(core$x), as.numeric(core$y)))
  expected_core_xy <- unname(npolygon(
    g_rot$.layout[1, 1], g_rot$.layout[1, 2],
    n = 3, r = .05, d = pi/4
  ))

  expect_equal(
    core_xy,
    expected_core_xy,
    tolerance = 1e-8,
    info = "vertex.rot should rotate grid vertex polygons using radians"
  )

  g_shape_rot <- nplot(x, layout = l, skip.edges = TRUE, skip.arrows = TRUE,
                       vertex.size = rep(.05, 4),
                       vertex.size.range = NULL,
                       vertex.frame.prop = 0,
                       vertex.nsides = "triangle",
                       vertex.rot = pi/4)
  shape_core <- g_shape_rot$children$graph$children$vertex.1$children$core
  shape_core_xy <- unname(cbind(
    as.numeric(shape_core$x),
    as.numeric(shape_core$y)
  ))
  expected_shape_core_xy <- unname(npolygon(
    g_shape_rot$.layout[1, 1], g_shape_rot$.layout[1, 2],
    n = 3, r = .05, d = pi/4
  ))

  expect_equal(
    shape_core_xy,
    expected_shape_core_xy,
    tolerance = 1e-8,
    info = "vertex.rot should add to named-shape vertex rotations"
  )

  g_square <- nplot(x, layout = l, skip.edges = TRUE, skip.arrows = TRUE,
                    vertex.size = rep(.05, 4),
                    vertex.size.range = NULL,
                    vertex.frame.prop = 0,
                    vertex.nsides = "square")
  square_core <- g_square$children$graph$children$vertex.1$children$core
  square_core_xy <- unname(cbind(
    as.numeric(square_core$x),
    as.numeric(square_core$y)
  ))
  expected_square_core_xy <- unname(npolygon(
    g_square$.layout[1, 1], g_square$.layout[1, 2],
    n = 4, r = .05, d = pi/4
  ))

  expect_equal(
    square_core_xy,
    expected_square_core_xy,
    tolerance = 1e-8,
    info = "the named square shape should use a square orientation"
  )

  g_label <- nplot(x, layout = l, skip.arrows = TRUE,
                   vertex.label = letters[seq_len(igraph::vcount(x))],
                   vertex.label.range = NULL)

  label_fontsizes <- get_vertex_gpar(g_label, element = "label", "fontsize")$fontsize
  expect_true(
    all(label_fontsizes >= 5 & label_fontsizes <= 15),
    info = "vertex.label.range = NULL should fall back to visible default font sizes"
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

  base_rot <- nplot_base(x, layout = l, skip.edges = TRUE, skip.arrows = TRUE,
                         vertex.size = rep(.05, 4),
                         vertex.size.range = NULL,
                         vertex.frame.prop = .2,
                         vertex.nsides = rep(3, 4),
                         vertex.rot = pi/4)
  layout_fit <- netplot:::fit_coords_to_dev(l)
  frame_outer_xy <- unname(as.matrix(
    base_rot$vertex.frame.coords[[1]][seq_len(3), ]
  ))
  expected_frame_outer_xy <- unname(npolygon(
    layout_fit[1, 1], layout_fit[1, 2],
    n = 3, r = .05, d = pi/4
  ))

  expect_equal(
    frame_outer_xy,
    expected_frame_outer_xy,
    tolerance = 1e-8,
    info = "nplot_base should rotate vertex frames with vertex.rot"
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
