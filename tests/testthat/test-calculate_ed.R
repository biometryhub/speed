test_that("calculate_ed works for partial replications", {
  # fmt: skip
  design_matrix <- matrix(c(
    1, 1, 1, 4,
    2, 3, 3, 4,
    2, 3, 3, 5,
    6, 6, 5, 4
  ), ncol = 4, byrow = TRUE)

  expected_msts <- c(
    "1" = 2,
    "2" = 1,
    "3" = 3,
    "4" = 3,
    "5" = sqrt(2),
    "6" = 1
  )

  result <- calculate_ed(design_matrix)
  expect_equal(result$msts, expected_msts, tolerance = 1e-10)
  expect_equal(result$total_mst, sum(expected_msts), tolerance = 1e-10)
  expect_equal(
    result$inv_total_mst,
    sum(1 / expected_msts[expected_msts > 0]),
    tolerance = 1e-10
  )
})
