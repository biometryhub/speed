test_that(".verify_speed_inputs works correctly", {
  test_data <- data.frame(
    row = 1:6,
    col = rep(1:3, 2),
    treatment = rep(c("A", "B", "C"), 2),
    block = rep(1:2, each = 3)
  )

  expect_silent(.verify_speed_inputs(
    data = test_data,
    swap = "treatment",
    swap_within = "block",
    spatial_factors = ~ row + col,
    iterations = 100,
    early_stop_iterations = 50,
    quiet = TRUE,
    seed = 123
  ))

  expect_silent(.verify_speed_inputs(
    data = test_data,
    swap = "treatment",
    swap_within = "1",
    spatial_factors = ~ row + col,
    iterations = 100,
    early_stop_iterations = 50,
    quiet = TRUE,
    seed = NULL
  ))

  expect_error(
    .verify_speed_inputs(
      data = "not_a_dataframe",
      swap = "treatment",
      swap_within = "block",
      spatial_factors = ~ row + col,
      iterations = 100,
      early_stop_iterations = 50,
      quiet = TRUE,
      seed = 123
    ),
    "`data` must be an initial data frame"
  )

  expect_error(
    .verify_speed_inputs(
      data = test_data,
      swap = "nonexistent_column",
      swap_within = "block",
      spatial_factors = ~ row + col,
      iterations = 100,
      early_stop_iterations = 50,
      quiet = TRUE,
      seed = 123
    ),
    "'nonexistent_column' not found in"
  )

  expect_error(
    .verify_speed_inputs(
      data = test_data,
      swap = "treatment",
      swap_within = "block",
      spatial_factors = "row + col",
      iterations = 100,
      early_stop_iterations = 50,
      quiet = TRUE,
      seed = 123
    ),
    "spatial_factors must be a one sided formula"
  )

  expect_error(
    .verify_speed_inputs(
      data = test_data,
      swap = "treatment",
      swap_within = "block",
      spatial_factors = ~ row + nonexistent_col,
      iterations = 100,
      early_stop_iterations = 50,
      quiet = TRUE,
      seed = 123
    ),
    "'nonexistent_col' not found in"
  )

  expect_error(
    .verify_speed_inputs(
      data = test_data,
      swap = "treatment",
      swap_within = "block",
      spatial_factors = ~ row + col,
      iterations = -1,
      early_stop_iterations = 50,
      quiet = TRUE,
      seed = 123
    ),
    "must be a positive whole number"
  )
})

test_that(".verify_hierarchical_inputs works correctly", {
  test_data <- data.frame(
    row = 1:6,
    col = rep(1:3, 2),
    treatment = rep(c("A", "B", "C"), 2),
    subplot = rep(c("a", "b"), 3),
    block = rep(1:2, each = 3)
  )

  expect_silent(.verify_hierarchical_inputs(
    data = test_data,
    swap = list(main = "treatment", sub = "subplot"),
    swap_within = list(main = "block", sub = "1"),
    spatial_factors = ~ row + col,
    iterations = 100,
    early_stop_iterations = 50,
    obj_function = "balance",
    quiet = TRUE,
    seed = 123
  ))

  expect_error(
    .verify_hierarchical_inputs(
      data = test_data,
      swap = list(main = "treatment", sub = "subplot"),
      swap_within = list(different = "block", sub = "1"),
      spatial_factors = ~ row + col,
      iterations = 100,
      early_stop_iterations = 50,
      obj_function = "balance",
      quiet = TRUE,
      seed = 123
    ),
    "Names of `swap` and `swap_within` must match"
  )

  expect_error(
    .verify_hierarchical_inputs(
      data = test_data,
      swap = list(main = "nonexistent", sub = "subplot"),
      swap_within = list(main = "block", sub = "1"),
      spatial_factors = ~ row + col,
      iterations = 100,
      early_stop_iterations = 50,
      obj_function = "balance",
      quiet = TRUE,
      seed = 123
    ),
    "Column nonexistent not found in data"
  )

  expect_error(
    .verify_hierarchical_inputs(
      data = test_data,
      swap = list(main = "treatment", sub = "subplot"),
      swap_within = list(main = "nonexistent", sub = "1"),
      spatial_factors = ~ row + col,
      iterations = 100,
      early_stop_iterations = 50,
      obj_function = "balance",
      quiet = TRUE,
      seed = 123
    ),
    "Column nonexistent not found in data"
  )

  expect_error(
    .verify_hierarchical_inputs(
      data = test_data,
      swap = list(main = "treatment", sub = "subplot"),
      swap_within = list(main = "block", sub = "1"),
      spatial_factors = ~ row + col,
      iterations = 100,
      early_stop_iterations = 50,
      obj_function = "balance",
      quiet = "not_logical",
      seed = 123
    ),
    "`quiet` must be logical"
  )

  expect_error(
    .verify_hierarchical_inputs(
      data = test_data,
      swap = list(main = "treatment", sub = "subplot"),
      swap_within = list(main = "block", sub = "1"),
      spatial_factors = ~ row + col,
      iterations = 100,
      early_stop_iterations = 50,
      obj_function = "balance",
      quiet = TRUE,
      seed = "not_numeric"
    ),
    "`seed` must be numeric or NULL"
  )
})

test_that("is_between_ works correctly", {
  between_1_10 <- is_between_(1, 10)
  expect_true(between_1_10(5))
  expect_true(between_1_10(1))
  expect_true(between_1_10(10))
  expect_false(between_1_10(0))
  expect_false(between_1_10(11))
  expect_false(between_1_10("5"))

  between_1_10_exclusive <- is_between_(
    1,
    10,
    lower_exclude = TRUE,
    upper_exclude = TRUE
  )
  expect_true(between_1_10_exclusive(5))
  expect_false(between_1_10_exclusive(1))
  expect_false(between_1_10_exclusive(10))
  expect_false(between_1_10_exclusive(0))
  expect_false(between_1_10_exclusive(11))

  greater_than_0 <- is_between_(0, Inf, lower_exclude = TRUE)
  expect_true(greater_than_0(1))
  expect_false(greater_than_0(0))
  expect_false(greater_than_0(-1))

  less_than_100 <- is_between_(-Inf, 100, upper_exclude = TRUE)
  expect_true(less_than_100(50))
  expect_false(less_than_100(100))
  expect_false(less_than_100(101))
})

test_that("is_boolean works correctly", {
  expect_true(is_boolean(TRUE))
  expect_true(is_boolean(FALSE))
  expect_false(is_boolean(1))
  expect_false(is_boolean(0))
  expect_false(is_boolean("TRUE"))
  expect_false(is_boolean(c(TRUE, FALSE)))
  expect_false(is_boolean(NA))
})

test_that("is_whole_number works correctly", {
  expect_true(is_whole_number(1))
  expect_true(is_whole_number(0))
  expect_true(is_whole_number(-1))
  expect_true(is_whole_number(100))
  expect_true(is_whole_number(1.0))
  expect_false(is_whole_number(1.5))
  expect_false(is_whole_number(pi))
  expect_false(is_whole_number("1"))
  expect_false(is_whole_number(NA))
  expect_equal(is_whole_number(Inf), NA)

  expect_true(is_whole_number(1.0000001, tol = 1e-6))
  expect_false(is_whole_number(1.001, tol = 1e-6))
})

test_that("is_positive_whole_number works correctly", {
  expect_true(is_positive_whole_number(1))
  expect_true(is_positive_whole_number(100))
  expect_false(is_positive_whole_number(0))
  expect_false(is_positive_whole_number(-1))
  expect_false(is_positive_whole_number(1.5))
  expect_false(is_positive_whole_number("1"))
  expect_false(is_positive_whole_number(NA))
})

test_that("is_non_negative_whole_number works correctly", {
  expect_true(is_non_negative_whole_number(0))
  expect_true(is_non_negative_whole_number(1))
  expect_true(is_non_negative_whole_number(100))
  expect_false(is_non_negative_whole_number(-1))
  expect_false(is_non_negative_whole_number(1.5))
  expect_false(is_non_negative_whole_number("1"))
  expect_false(is_non_negative_whole_number(NA))
})

test_that("is_positive_whole_numbers works correctly", {
  expect_true(is_positive_whole_numbers(c(1, 2, 3)))
  expect_true(is_positive_whole_numbers(1))
  expect_false(is_positive_whole_numbers(c(1, 2, 0)))
  expect_false(is_positive_whole_numbers(c(1, 2, -1)))
  expect_false(is_positive_whole_numbers(c(1, 2.5, 3)))
  expect_false(is_positive_whole_numbers(c(1, 2, "3")))
})

test_that("is_multiple_of works correctly", {
  expect_true(is_multiple_of(10, 5))
  expect_true(is_multiple_of(6, 2))
  expect_true(is_multiple_of(0, 5))
  expect_false(is_multiple_of(7, 3))
  expect_false(is_multiple_of(5, 10))
})

test_that("must_be works correctly", {
  expect_true(must_be(1, c(1, 2, 3)))
  expect_true(must_be("a", c("a", "b", "c")))
  expect_true(must_be(TRUE, c(TRUE, FALSE)))
  expect_false(must_be(4, c(1, 2, 3)))
  expect_false(must_be("d", c("a", "b", "c")))
  expect_false(must_be(1, c("1", "2", "3")))
})

test_that("verify_column_exists works correctly", {
  test_data <- data.frame(a = 1:3, b = 4:6, c = 7:9)

  expect_silent(verify_column_exists("a", test_data))
  expect_silent(verify_column_exists("b", test_data))

  expect_error(verify_column_exists("d", test_data), "'d' not found in a, b, c")

  expect_error(
    verify_column_exists("missing", test_data, "Custom suffix."),
    "'missing' not found in a, b, c. Custom suffix."
  )
})

test_that("verify_between works correctly", {
  expect_silent(verify_between(5, lower = 1, upper = 10))
  expect_silent(verify_between(1, 5, 10, lower = 0, upper = 15))

  expect_error(
    verify_between(11, lower = 1, upper = 10),
    "must be inclusively between 1 and 10"
  )

  expect_error(
    verify_between(0, lower = 1, upper = 10, lower_exclude = TRUE),
    "must be between 1 \\(exclusive\\) and 10"
  )

  expect_error(
    verify_between(10, lower = 1, upper = 10, upper_exclude = TRUE),
    "must be between 1 and 10 \\(exclusive\\)"
  )

  expect_error(verify_between(50, upper = 10), "must be at most 10")

  expect_error(verify_between(-5, lower = 0), "must be at least 0")

  expect_error(
    verify_between(5, lower = 10, lower_exclude = TRUE),
    "must be greater than 10"
  )

  expect_error(
    verify_between(15, upper = 10, upper_exclude = TRUE),
    "must be less than 10"
  )
})

test_that("verify_boolean works correctly", {
  expect_silent(verify_boolean(TRUE))
  expect_silent(verify_boolean(FALSE))
  expect_silent(verify_boolean(TRUE, FALSE, TRUE))

  expect_error(verify_boolean(1), "must be a boolean")
  expect_error(verify_boolean("TRUE"), "must be a boolean")
  expect_error(verify_boolean(c(TRUE, FALSE)), "must be a boolean")
})

test_that("verify_positive_whole_number works correctly", {
  expect_silent(verify_positive_whole_number(1))
  expect_silent(verify_positive_whole_number(1, 2, 100))

  expect_error(
    verify_positive_whole_number(0),
    "must be a positive whole number"
  )
  expect_error(
    verify_positive_whole_number(-1),
    "must be a positive whole number"
  )
  expect_error(
    verify_positive_whole_number(1.5),
    "must be a positive whole number"
  )
  expect_error(
    verify_positive_whole_number("1"),
    "must be a positive whole number"
  )
})

test_that("verify_non_negative_whole works correctly", {
  expect_silent(verify_non_negative_whole(0))
  expect_silent(verify_non_negative_whole(1))
  expect_silent(verify_non_negative_whole(0, 1, 100))

  expect_error(
    verify_non_negative_whole(-1),
    "must be a non-negative whole number"
  )
  expect_error(
    verify_non_negative_whole(1.5),
    "must be a non-negative whole number"
  )
  expect_error(
    verify_non_negative_whole("1"),
    "must be a non-negative whole number"
  )
})

test_that("verify_positive_whole_numbers works correctly", {
  expect_silent(verify_positive_whole_numbers(c(1, 2, 3)))
  expect_silent(verify_positive_whole_numbers(1))

  expect_error(
    verify_positive_whole_numbers(c(1, 2, 0)),
    "must be a vector of positive whole numbers"
  )
  expect_error(
    verify_positive_whole_numbers(c(1, 2, -1)),
    "must be a vector of positive whole numbers"
  )
  expect_error(
    verify_positive_whole_numbers(c(1, 2.5, 3)),
    "must be a vector of positive whole numbers"
  )
})

test_that("verify_must_be works correctly", {
  expect_silent(verify_must_be("a", valid_values = c("a", "b", "c")))
  expect_silent(verify_must_be(1, valid_values = c(1, 2, 3)))

  expect_error(
    verify_must_be("d", valid_values = c("a", "b", "c")),
    'must be `"a"`, `"b"`, or `"c"`'
  )
  expect_error(
    verify_must_be(4, valid_values = c(1, 2, 3)),
    "must be `1`, `2`, or `3`"
  )
  expect_error(
    verify_must_be("x", valid_values = c("a", "b")),
    'must be `"a"` or `"b"`'
  )
})

test_that("verify_multiple_of works correctly", {
  expect_silent(verify_multiple_of(10, 5))
  expect_silent(verify_multiple_of(6, 2))

  expect_error(verify_multiple_of(7, 3), "must be a multiple of")
})

test_that("get_literal_values works correctly", {
  expect_equal(get_literal_values(c("a")), '`"a"`')
  expect_equal(get_literal_values(c("a", "b")), '`"a"` or `"b"`')
  expect_equal(get_literal_values(c("a", "b", "c")), '`"a"`, `"b"`, or `"c"`')
  expect_equal(get_literal_values(c(1, 2, 3)), "`1`, `2`, or `3`")
})

test_that("literal works correctly", {
  expect_equal(literal("test"), '`"test"`')
  expect_equal(literal(123), "`123`")
  expect_equal(literal(TRUE), "`TRUE`")
})

test_that("get_var_names works correctly", {
  # This function extracts variable names from function calls
  test_func <- function(...) {
    get_var_names(...)
  }

  x <- 1
  y <- 2
  result <- test_func(x, y)
  expect_equal(result, c("x", "y"))

  result2 <- test_func(x)
  expect_equal(result2, "x")
})

test_that("data_type_error works correctly", {
  expect_error(
    data_type_error("test_var", "a number"),
    "`test_var` must be a number."
  )
  expect_error(data_type_error("x", "positive"), "`x` must be positive.")
})

test_that("edge cases and error handling", {
  empty_df <- data.frame()
  expect_error(verify_column_exists("any", empty_df), "'any' not found in")

  expect_silent(verify_between(0, lower = 0, upper = 0))
  expect_error(
    verify_between(0, lower = 0, upper = 0, lower_exclude = TRUE),
    "must be between 0 \\(exclusive\\) and 0"
  )

  large_num <- .Machine$integer.max
  expect_true(is_whole_number(large_num))
  expect_true(is_positive_whole_number(large_num))

  small_num <- .Machine$double.eps
  expect_false(is_whole_number(small_num, tol = .Machine$double.eps))
  expect_true(is_whole_number(small_num, tol = small_num * 2))
})

test_that(".verify_swap_all_replication rejects unequal within-group replication", {
  # A occupies 3 plots per block, B 2 and C 1, so exchanging every plot of one
  # for every plot of another would swap their replication counts
  unequal <- data.frame(
    row = rep(1:6, times = 2),
    col = rep(1:2, each = 6),
    block = rep(1:2, each = 6),
    treatment = rep(c("A", "A", "A", "B", "B", "C"), 2)
  )

  optimise <- list(
    `all treatment within block` = list(
      swap = "treatment",
      swap_within = "block",
      swap_all = TRUE
    )
  )

  expect_error(
    .verify_swap_all_replication(unequal, optimise),
    "requires equal replication within each swap group"
  )

  msg <- tryCatch(
    .verify_swap_all_replication(unequal, optimise),
    error = conditionMessage
  )
  # reports the offending column, group and counts
  expect_match(msg, "`treatment` is unequally replicated", fixed = TRUE)
  expect_match(msg, "within `block` 1: A (3), B (2), C (1)", fixed = TRUE)
  expect_match(msg, "and in 1 other group(s)", fixed = TRUE)
  # no level name for a single-level design
  expect_false(grepl("level", msg, fixed = TRUE))

  # levels that do not use swap_all are not checked
  optimise[[1]]$swap_all <- FALSE
  expect_silent(.verify_swap_all_replication(unequal, optimise))
})

test_that(".verify_swap_all_replication accepts designs the swap can preserve", {
  # equal replication within each block
  equal <- data.frame(
    block = rep(1:2, each = 6),
    treatment = rep(c("A", "A", "B", "B", "C", "C"), 2)
  )
  optimise <- list(
    lvl = list(swap = "treatment", swap_within = "block", swap_all = TRUE)
  )
  expect_silent(.verify_swap_all_replication(equal, optimise))

  # incomplete blocks are fine: only the treatments present in a group can be
  # chosen for a swap, so absent treatments cannot cause an unequal exchange
  incomplete <- data.frame(
    block = rep(1:3, each = 2),
    treatment = c("A", "B", "B", "C", "A", "C")
  )
  expect_silent(.verify_swap_all_replication(incomplete, optimise))

  # NA groups and NA treatments are excluded, matching the neighbour generator
  with_na <- data.frame(
    block = c(1, 1, 1, 1, NA, NA),
    treatment = c("A", "A", "B", "B", "C", NA)
  )
  expect_silent(.verify_swap_all_replication(with_na, optimise))

  # a group holding a single treatment is never swapped, so is not an error
  one_treatment <- data.frame(
    block = c(1, 1, 1, 2, 2, 2),
    treatment = c("A", "A", "A", "B", "B", "C")
  )
  expect_error(
    .verify_swap_all_replication(one_treatment, optimise),
    "within `block` 2: B (2), C (1)",
    fixed = TRUE
  )
})

test_that(".verify_swap_all_replication names the level and the whole design", {
  split_plot <- data.frame(
    block = rep(1:2, each = 6),
    wholeplot = rep(1:4, times = c(3, 3, 3, 3)),
    wp_trt = rep(c("A", "A", "A", "B", "B", "B"), 2),
    sp_trt = rep(c("a", "a", "b", "a", "b", "b"), 2)
  )

  two_level <- list(
    wp = list(swap = "wp_trt", swap_within = "block", swap_all = TRUE),
    sp = list(swap = "sp_trt", swap_within = "wholeplot", swap_all = TRUE)
  )

  # wp is balanced within block; sp is not balanced within wholeplot
  expect_error(
    .verify_swap_all_replication(split_plot, two_level),
    "(level `sp`)",
    fixed = TRUE
  )

  # a level with no swap_within boundary is described as the whole design
  no_group <- data.frame(
    treatment = c("A", "A", "A", "B", "B", "C"),
    dummy_1 = factor(rep(1, 6))
  )
  dummy <- list(
    lvl = list(swap = "treatment", swap_within = "dummy_1", swap_all = TRUE)
  )
  expect_error(
    .verify_swap_all_replication(no_group, dummy, dummy_group = "dummy_1"),
    "unequally replicated across the whole design",
    fixed = TRUE
  )
})

test_that("speed() errors instead of altering replication when swap_all = TRUE", {
  df <- data.frame(
    row = rep(1:6, times = 2),
    col = rep(1:2, each = 6),
    block = rep(1:2, each = 6),
    treatment = rep(c("A", "A", "A", "B", "B", "C"), 2)
  )

  expect_error(
    speed(
      df,
      swap = "treatment",
      swap_within = "block",
      swap_all = TRUE,
      seed = 1,
      quiet = TRUE
    ),
    "requires equal replication within each swap group"
  )

  # unequal replication with no swap_within is equally undefined
  expect_error(
    speed(df, swap = "treatment", swap_all = TRUE, seed = 1, quiet = TRUE),
    "unequally replicated across the whole design",
    fixed = TRUE
  )

  # the same design is untouched by the single-swap generator
  result <- speed(
    df,
    swap = "treatment",
    swap_within = "block",
    swap_all = FALSE,
    iterations = 100,
    seed = 1,
    quiet = TRUE
  )
  expect_equal(c(table(result$design_df$treatment)), c(table(df$treatment)))
})
