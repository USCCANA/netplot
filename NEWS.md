# netplot 0.3-0

* Documentation overhaul: `nplot()` now documents the formula interface for
  mapping vertex/edge aesthetics from graph attributes, the README highlights
  what sets netplot apart, a package-level help page was added, and the pkgdown
  reference is organized by feature.

* New vignette `"formulas"` demonstrating how to color, shape, and size vertices
  (and scale edge widths) from graph attributes using formulas.

* `nplot.network()` now uses the `"weight"` edge attribute for `edge.width`
  by default (like `nplot.igraph()` already did), so edge widths reflect edge
  weights instead of being drawn uniformly. `edge.width` values are also
  explicitly re-applied to each edge grob after color post-processing to keep
  the drawn line widths robust. Closes #17.

* `edge.width.range`, `vertex.size.range`, and `vertex.label.range` now accept
  `NULL` to suppress scaling and use the supplied values as-is.

* Fixed `vertex.rot`, which was truncated to an integer and so ignored
  fractional (radian) rotations; the named `"square"` shape orientation was
  corrected as well.

* `edge.line.lty` is now validated to be length 1 or one value per plotted
  edge, and is subset correctly when `sample.edges` drops edges.

* Invalid arguments passed to `nplot()` now raise an error.

* Figures with legends are not drawn twice.

* Values supported by `nplot()` are now included in all methods (helps with
  argument completion).

* Changed some default values for better results.


# netplot 0.2-0

* The arguments `skip.vertex`, `skip.edges`, and `skip.arrows` now work as
  documented.

* New function `nplot_legend()` helps to add legends to the figure.

* New `nplot()` method for matrices.

* New feature: Gradients.

* The argument `sample.edges` now work as expected.

* `vertex.color`, `vertex.size`, and `vertex.nsides` now accepts formulas.

* `edge.width` now accepts formulas.

* New function: `locate_vertex()`.


# netplot 0.1-0

* First CRAN release.
