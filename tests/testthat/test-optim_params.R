test_that("optim_params works with no arguments", {
  params <- c(
    "swap_count",
    "swap_all_blocks",
    "adaptive_swaps",
    "start_temp",
    "cooling_rate",
    "random_initialisation",
    "adj_weight",
    "bal_weight",
    "conc_weight",
    "stop_at_optimal",
    "plateau_window",
    "plateau_tol"
  )

  optimize_params <- optim_params()

  expect_named(optimize_params, params)
  expect_true(is.list(optimize_params))
})

test_that("optim_params works with arguments", {
  params <- c(
    "swap_count",
    "swap_all_blocks",
    "adaptive_swaps",
    "start_temp",
    "cooling_rate",
    "random_initialisation",
    "adj_weight",
    "bal_weight",
    "conc_weight",
    "stop_at_optimal",
    "plateau_window",
    "plateau_tol"
  )

  optimize_params <- optim_params(swap_count = 2)

  expect_named(optimize_params, params)
  expect_true(is.list(optimize_params))
  expect_equal(optimize_params$swap_count, 2)

  optimize_params <- optim_params(
    swap_count = 2,
    swap_all_blocks = TRUE,
    start_temp = 99
  )

  expect_named(optimize_params, params)
  expect_true(is.list(optimize_params))
  expect_equal(optimize_params$swap_count, 2)
  expect_equal(optimize_params$swap_all_blocks, TRUE)
  expect_equal(optimize_params$start_temp, 99)

  optimize_params_expected <- list(
    swap_count = 2,
    swap_all_blocks = TRUE,
    adaptive_swaps = TRUE,
    cooling_rate = 0.88,
    start_temp = 99,
    random_initialisation = 3,
    adj_weight = 2,
    bal_weight = 3,
    conc_weight = 4,
    stop_at_optimal = FALSE,
    plateau_window = 500,
    plateau_tol = 0.02
  )

  optimize_params <- do.call(optim_params, optimize_params_expected)

  expect_named(optimize_params, params)
  expect_true(is.list(optimize_params))
  expect_equal(optimize_params[params], optimize_params_expected[params])
})

test_that("optim_params works with legacy options", {
  params <- c(
    "swap_count",
    "swap_all_blocks",
    "adaptive_swaps",
    "start_temp",
    "cooling_rate",
    "random_initialisation",
    "adj_weight",
    "bal_weight",
    "conc_weight",
    "stop_at_optimal",
    "plateau_window",
    "plateau_tol"
  )

  withr::with_options(
    list(speed.swap_count = 2),
    expect_warning(
      {
        optimize_params <- optim_params()
      },
      "Setting options with `options\\(speed.\\{option\\}=...\\)` is deprecated. Please use `optim_params\\(\\)` instead."
    )
  )

  expect_named(optimize_params, params)
  expect_true(is.list(optimize_params))
  expect_equal(optimize_params$swap_count, 2)

  withr::with_options(
    list(speed.swap_count = 2, speed.start_temp = 88),
    expect_warning(
      {
        optimize_params <- optim_params(swap_all_blocks = TRUE)
      },
      "Setting options with `options\\(speed.\\{option\\}=...\\)` is deprecated. Please use `optim_params\\(\\)` instead."
    )
  )

  expect_named(optimize_params, params)
  expect_true(is.list(optimize_params))
  expect_equal(optimize_params$swap_count, 2)
  expect_equal(optimize_params$start_temp, 88)
  expect_equal(optimize_params$swap_all_blocks, TRUE)
})

test_that("optim_params throws error for invalid inputs", {
  expect_error(optim_params(swap_count = "a"))
  expect_error(optim_params(swap_count = c(1, 2)))
  expect_error(optim_params(swap_count = -1))
  expect_error(optim_params(swap_count = 0))

  expect_error(optim_params(swap_all_blocks = "a"))
  expect_error(optim_params(swap_all_blocks = "TRUE"))
  expect_error(optim_params(swap_all_blocks = 1))

  expect_error(optim_params(adaptive_swaps = "a"))
  expect_error(optim_params(adaptive_swaps = "TRUE"))
  expect_error(optim_params(adaptive_swaps = 1))

  expect_error(optim_params(start_temp = "a"))
  expect_error(optim_params(start_temp = c(1, 2)))
  expect_error(optim_params(start_temp = -1))
  expect_error(optim_params(start_temp = 1.1))

  expect_error(optim_params(cooling_rate = "a"))
  expect_error(optim_params(cooling_rate = c(0.2, 0.3)))
  expect_error(optim_params(cooling_rate = -0.1))
  expect_error(optim_params(cooling_rate = 1))
  expect_error(optim_params(cooling_rate = 1.1))

  expect_error(optim_params(random_initialisation = "a"))
  expect_error(optim_params(random_initialisation = c(1, 2)))
  expect_error(optim_params(random_initialisation = -1))
  expect_error(optim_params(random_initialisation = 1.1))

  expect_error(optim_params(adj_weight = "a"))
  expect_error(optim_params(adj_weight = c(1, 2)))

  expect_error(optim_params(bal_weight = "a"))
  expect_error(optim_params(bal_weight = c(1, 2)))
  expect_error(optim_params(conc_weight = "a"))

  expect_error(optim_params(stop_at_optimal = "a"))
  expect_error(optim_params(stop_at_optimal = "TRUE"))
  expect_error(optim_params(stop_at_optimal = 1))

  expect_error(optim_params(plateau_window = 0))
  expect_error(optim_params(plateau_window = 1.5))
  expect_error(optim_params(plateau_window = "a"))
  expect_error(optim_params(plateau_tol = -0.1))
  expect_error(optim_params(plateau_tol = "a"))
})

test_that(".is_plateau needs every non-zero component to stall", {
  expect_true(.is_plateau(c(10, 1), c(9.95, 0.995), 0.01))
  expect_false(.is_plateau(c(10, 1), c(9.95, 0.9), 0.01))
  # zero components are ignored, a worse one counts as stalled
  expect_true(.is_plateau(c(0, 1), c(0, 1.2), 0.01))
  expect_true(.is_plateau(c(0, 0), c(0, 0), 0.01))
})

test_that(".score_components falls back to the score", {
  expect_equal(.score_components(list(score = 3, components = c(a = 1, b = 2))), c(1, 2))
  expect_equal(.score_components(list(score = 3)), 3)
  expect_equal(.score_components(list(score = 3, components = list(a = 1, b = 2))), c(1, 2))
})

test_that("plateau_window stops a level once the best components stall", {
  df <- initialise_design_df(1:8, nrows = 6, ncols = 4, block_nrows = 2, block_ncols = 4)
  run <- function(params) {
    return(speed(
      df, "treatment",
      swap_within = "block",
      concurrence_factors = ~ row + col,
      iterations = 3000, early_stop_iterations = 3000,
      optimise_params = params,
      quiet = TRUE, seed = 1
    ))
  }

  full <- run(optim_params(stop_at_optimal = FALSE))
  plateau <- run(optim_params(stop_at_optimal = FALSE, plateau_window = 200, plateau_tol = 0.01))

  expect_equal(plateau$metadata$per_level[[1]]$stop_reason, "plateau")
  expect_true(plateau$stopped_early)
  n <- length(plateau$scores)
  expect_lt(n, length(full$scores))
  # same seed, same path up to the stop
  expect_equal(plateau$scores[seq_len(n - 1)], full$scores[seq_len(n - 1)])

  off <- run(optim_params(stop_at_optimal = FALSE, plateau_window = NULL))
  expect_identical(off$scores, full$scores)
})
