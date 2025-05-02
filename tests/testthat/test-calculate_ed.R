test_that("calculate_ed works for partial replications", {
  design_matrix <- matrix(c(
    1, 1, 1, 4,
    2, 3, 3, 4,
    2, 3, 3, 5,
    6, 6, 5, 4
  ), ncol = 4) |>
    t()

  expected_ed <- list(
    "2" = list(
      msts = list(
        "2" = 1,
        "5" = sqrt(2),
        "6" = 1
      ),
      min_mst = 1,
      min_items = c("2", "6")
    ),
    "3" = list(
      msts = list(
        "1" = 2,
        "4" = 3
      ),
      min_mst = 2,
      min_items = c("1")
    ),
    "4" = list(
      msts = list("3" = 3),
      min_mst = 3,
      min_items = c("3")
    )
  )

  expect_mapequal(calculate_ed(design_matrix), expected_ed)
})
