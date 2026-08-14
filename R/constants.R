.DEFAULT <- list(
  swap_within = "1",
  spatial_factors = ~ row + col,
  grid_factors = list(dim1 = "row", dim2 = "col"),
  iterations = 10000,
  early_stop_iterations = 2000,
  swap_all = FALSE
)
