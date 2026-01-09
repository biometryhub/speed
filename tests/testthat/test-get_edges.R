test_that("get_vertices works", {
  # fmt: skip
  design_matrix <- matrix(c(
    1, 1, 4,
    2, 3, 3,
    2, 3, 3
  ), ncol = 3)
  design_matrix <- t(design_matrix)

  vertices <- get_vertices(design_matrix)
  edges <- get_edges(vertices)

  expected_edges <- list(
    "1" = c(1),
    "2" = c(1),
    "3" = c(1, 1, sqrt(2), sqrt(2), 1, 1),
    "4" = c()
  )

  # order matters
  for (item in names(expected_edges)) {
    expect_equal(edges[[item]], expected_edges[[item]])
  }
})
