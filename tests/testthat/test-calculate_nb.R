test_that("calculate_nb works", {
  # TODO: manually calculate the actual variance
  design_matrix <- matrix(c(
    1, 1, 1,
    2, 2, 3,
    2, 3, 1
  ), ncol = 3)

  expected_nb <- list(
    nb = list(
      "1,1" = 2,
      "1,2" = 2,
      "1,3" = 3,
      "2,2" = 2,
      "2,3" = 3
    ),
    max_nb = 3,
    max_pairs = c("1,3", "2,3")
  )

  nb <- calculate_nb(design_matrix)
  expect_mapequal(nb$nb, expected_nb$nb)
  expect_equal(nb$max_nb, expected_nb$max_nb)
  expect_setequal(nb$max_pairs, expected_nb$max_pairs)
})
