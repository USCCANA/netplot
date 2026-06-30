
# Placeholder with simple test
expect_equal(1 + 1, 2)

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
}

