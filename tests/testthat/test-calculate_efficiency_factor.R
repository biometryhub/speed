test_that("calculate_efficiency_factor provides the same results as the paper", {
  # fmt: skip
  df_design1 <- initialize_design_df(c(
    7, 5, 6, 9, 4, 1, 3, 2, 8,
    5, 6, 3, 1, 7, 8, 2, 4, 9,
    8, 9, 5, 6, 3, 4, 1, 7, 2,
    1, 8, 2, 7, 6, 3, 9, 5, 4
  ), 4, 9)

  expect_equal(
    round(calculate_efficiency_factor(df_design1, "treatment"), 3),
    0.834
  )

  # fmt: skip
  df_design2 <- initialize_design_df(c(
    8, 5, 7, 2, 4, 1, 6, 9, 3,
    1, 9, 8, 6, 3, 2, 4, 7, 5,
    7, 6, 4, 9, 5, 8, 3, 2, 1,
    4, 2, 3, 1, 7, 9, 5, 8, 6
  ), 4, 9)

  expect_equal(
    round(calculate_efficiency_factor(df_design2, "treatment"), 3),
    0.783
  )

  # fmt: skip
  df_design3 <- initialize_design_df(c(
    9, 8, 1, 4, 3, 7, 5, 2, 6,
    7, 5, 6, 2, 9, 1, 3, 8, 4,
    2, 4, 3, 5, 6, 8, 9, 7, 1,
    1, 9, 8, 7, 4, 3, 2, 6, 5
  ), 4, 9)

  expect_equal(
    round(calculate_efficiency_factor(df_design3, "treatment"), 3),
    0.827
  )

  # fmt: skip
  df_design4 <- initialize_design_df(c(
    47, 16, 43, 42, 37, 35,  1, 59, 24, 19,  4, 18, 40, 28, 51, 29, 54, 57,
    12, 25,  6, 57, 47, 32, 39, 17, 31, 50, 15,  5, 55, 51,  9, 54, 41,  3,
    23, 18, 45, 36, 49,  7,  8, 60, 41, 29,  3, 58, 26, 52,  2, 15, 28, 27,
    56, 52, 42, 31, 53, 17, 27, 48,  7, 13, 10, 21, 19, 12, 33, 30, 34, 16,
    43, 22, 13, 20, 32, 58, 48,  9, 46,  8, 37, 40, 56, 14,  4, 11, 38, 44
  ), 5, 18)

  expect_equal(
    round(calculate_efficiency_factor(df_design4, "treatment"), 3),
    0.529
  )
})

test_that("calculate_efficiency_factor provides better result for an optimized design", {
  # fmt: skip
  df_design_initial <- initialize_design_df(c(
    1, 1, 2, 2,
    3, 3, 4, 4,
    5, 5, 6, 6
  ), 3, 4)

  # fmt: skip
  df_design_optimized <- initialize_design_df(c(
    1, 2, 4, 3,
    5, 1, 6, 2,
    3, 6, 5, 4
  ), 3, 4)

  expect_lt(
    abs(1 - calculate_efficiency_factor(df_design_optimized, "treatment")),
    abs(1 - calculate_efficiency_factor(df_design_initial, "treatment"))
  )
})

test_that("calculate_efficiency_factor provides same result for mathematically identical designs", {
  # fmt: skip
  df_design1 <- initialize_design_df(c(
    1, 2, 4, 3,
    5, 1, 6, 2,
    3, 6, 5, 4
  ), 3, 4)

  # fmt: skip
  df_design2 <- initialize_design_df(c(
    "a", "b", "d", "c",
    "e", "a", "f", "b",
    "c", "f", "e", "d"
  ), 3, 4)

  # fmt: skip
  df_design3 <- initialize_design_df(c(
    "b", "a", "c", "d",
    "e", "b", "f", "a",
    "d", "f", "e", "c"
  ), 3, 4)

  expect_equal(
    calculate_efficiency_factor(df_design1, "treatment"),
    calculate_efficiency_factor(df_design2, "treatment")
  )

  expect_equal(
    calculate_efficiency_factor(df_design3, "treatment"),
    calculate_efficiency_factor(df_design2, "treatment")
  )
})
