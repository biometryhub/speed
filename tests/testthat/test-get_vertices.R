test_that("get_vertices works", {
  # fmt: skip
  design_matrix <- matrix(c(
    1, 1, 1,
    2, 3, 3,
    2, 3, 3
  ), ncol = 3)
  design_matrix <- t(design_matrix)

  # somehow it spits out 1:2 instead of c(1,2)
  expected_vertices <- list(
    "1" = list(c(1, 1), 1:2, c(1, 3)),
    "2" = list(2:1, c(3, 1)),
    "3" = list(c(2, 2), 2:3, 3:2, c(3, 3))
  )

  vertices <- get_vertices(design_matrix)

  for (item in names(expected_vertices)) {
    expect_setequal(vertices[[item]], expected_vertices[[item]])
  }
})
