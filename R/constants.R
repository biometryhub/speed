.DEFAULT <- list(
  swap_within = "1",
  spatial_factors = ~ row + col,
  grid_factors = list(dim1 = "row", dim2 = "col"),
  iterations = 10000,
  early_stop_iterations = 2000,
  swap_all = FALSE,
  # Empty rather than `optim_params()`, so a level left out of a per-level list
  # takes the defaults at the point `optim_params()` is called
  optimise_params = list()
)
