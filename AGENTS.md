# What is this project about

This is an R package that uses the `grid` package to create network plots. The package is focused on aesthetics and tries to provide an interface that has a lower footprint in dependencies than other packages, leveraging existing base R packages.

## Development workflow and preferences

- We follow the tinyverse style for code (so minimal dependencies, but still using `roxygen2` for documentation).

- For testing the package, we use `tinytest`.

- For checking the R package, we use `devtools::check()`, and for documenting we call `devtools::document()`.
