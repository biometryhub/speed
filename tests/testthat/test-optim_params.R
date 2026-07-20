test_that("optim_params works with no arguments", {
  params <- c(
    "swap_count",
    "swap_all_blocks",
    "adaptive_swaps",
    "start_temp",
    "cooling_rate",
    "random_initialisation",
    "adj_weight",
    "bal_weight"
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
    "bal_weight"
  )

  optimize_params <- optim_params(swap_count = 2)

  expect_named(optimize_params, params)
  expect_true(is.list(optimize_params))
  expect_equal(optimize_params$swap_count, 2)

  optimize_params <- optim_params(swap_count = 2, swap_all_blocks = TRUE, start_temp = 99)

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
    bal_weight = 3
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
    "bal_weight"
  )

  withr::with_options(list(speed.swap_count = 2), expect_warning(
    {
      optimize_params <- optim_params()
    },
    "Setting options with `options\\(speed.\\{option\\}=...\\)` is deprecated. Please use `optim_params\\(\\)` instead."
  ))

  expect_named(optimize_params, params)
  expect_true(is.list(optimize_params))
  expect_equal(optimize_params$swap_count, 2)

  withr::with_options(list(speed.swap_count = 2, speed.start_temp = 88), expect_warning(
    {
      optimize_params <- optim_params(swap_all_blocks = TRUE)
    },
    "Setting options with `options\\(speed.\\{option\\}=...\\)` is deprecated. Please use `optim_params\\(\\)` instead."
  ))

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
})
