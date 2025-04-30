test_that("get_vertices works", {
  # TODO: manually calculate the actual variance
  design_matrix <- matrix(c(
    1, 1, 1,
    2, 3, 3,
    2, 3, 3
  ), ncol = 3) |>
    t()

  vertices <- get_vertices(design_matrix)
  edges <- get_edges(vertices)

  expected_edges <- list(
    "1" = list(c(1, 2, 1), c(1, 3, 2), c(2, 3, 1)),
    "2" = list(c(1, 2, 1)),
    "3" = list(c(1, 2, 1), c(1, 3, 1), c(1, 4, sqrt(2)), c(2, 3, sqrt(2)), c(2, 4, 1), c(3, 4, 1))
  )

  for (item in names(expected_edges)) {
    expect_setequal(edges[[item]], expected_edges[[item]])
  }
})
