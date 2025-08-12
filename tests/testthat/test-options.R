test_that("speed_options returns all default options when called with no arguments", {
  result <- speed_options()
  
  expect_type(result, "list")
  expect_named(result, c("swap_count", "swap_all_blocks", "adaptive_swaps", 
                         "start_temp", "cooling_rate", "random_initialisation", 
                         "adj_weight", "bal_weight"))
  
  # Check default values
  expect_equal(result$swap_count, 1)
  expect_equal(result$swap_all_blocks, FALSE)
  expect_equal(result$adaptive_swaps, FALSE)
  expect_equal(result$start_temp, 100)
  expect_equal(result$cooling_rate, 0.99)
  expect_equal(result$random_initialisation, FALSE)
  expect_equal(result$adj_weight, 0)
  expect_equal(result$bal_weight, 1)
})

test_that("speed_options can get specific options", {
  result <- speed_options("swap_count", "cooling_rate")
  
  expect_type(result, "list")
  expect_named(result, c("swap_count", "cooling_rate"))
  expect_equal(result$swap_count, 1)
  expect_equal(result$cooling_rate, 0.99)
})

test_that("speed_options can set single option", {
  # Store original value
  original <- speed_options("swap_count")
  
  # Set new value
  old_values <- speed_options(swap_count = 5)
  
  # Check that old value was returned
  expect_equal(old_values$swap_count, 1)
  
  # Check that new value is set
  current <- speed_options("swap_count")
  expect_equal(current$swap_count, 5)
  
  # Reset to original
  speed_options(swap_count = original$swap_count)
})

test_that("speed_options can set multiple options", {
  # Store original values
  original <- speed_options("swap_count", "cooling_rate", "start_temp")
  
  # Set new values
  old_values <- speed_options(swap_count = 3, cooling_rate = 0.95, start_temp = 50)
  
  # Check that old values were returned
  expect_equal(old_values$swap_count, original$swap_count)
  expect_equal(old_values$cooling_rate, original$cooling_rate)
  expect_equal(old_values$start_temp, original$start_temp)
  
  # Check that new values are set
  current <- speed_options("swap_count", "cooling_rate", "start_temp")
  expect_equal(current$swap_count, 3)
  expect_equal(current$cooling_rate, 0.95)
  expect_equal(current$start_temp, 50)
  
  # Reset to original
  speed_options(swap_count = original$swap_count, 
                cooling_rate = original$cooling_rate,
                start_temp = original$start_temp)
})

test_that("speed_options validates swap_count correctly", {
  expect_error(speed_options(swap_count = 0), "swap_count must be a positive integer")
  expect_error(speed_options(swap_count = -1), "swap_count must be a positive integer")
  expect_error(speed_options(swap_count = 1.5), "swap_count must be a positive integer")
  expect_error(speed_options(swap_count = "1"), "swap_count must be a positive integer")
  expect_error(speed_options(swap_count = c(1, 2)), "swap_count must be a single value")
  expect_error(speed_options(swap_count = NA), "swap_count must be a single value")
})

test_that("speed_options validates start_temp correctly", {
  expect_error(speed_options(start_temp = 0), "start_temp must be positive")
  expect_error(speed_options(start_temp = -10), "start_temp must be positive")
  expect_error(speed_options(start_temp = "100"), "start_temp must be positive")
  expect_error(speed_options(start_temp = c(100, 200)), "start_temp must be a single value")
  expect_error(speed_options(start_temp = NA), "start_temp must be a single value")
})

test_that("speed_options validates cooling_rate correctly", {
  expect_error(speed_options(cooling_rate = 0), "cooling_rate must be between 0 and 1")
  expect_error(speed_options(cooling_rate = 1), "cooling_rate must be between 0 and 1")
  expect_error(speed_options(cooling_rate = 1.1), "cooling_rate must be between 0 and 1")
  expect_error(speed_options(cooling_rate = -0.5), "cooling_rate must be between 0 and 1")
  expect_error(speed_options(cooling_rate = "0.99"), "cooling_rate must be between 0 and 1")
  expect_error(speed_options(cooling_rate = c(0.99, 0.95)), "cooling_rate must be a single value")
  expect_error(speed_options(cooling_rate = NA), "cooling_rate must be a single value")
})

test_that("speed_options validates weight parameters correctly", {
  expect_error(speed_options(adj_weight = -1), "adj_weight must be non-negative")
  expect_error(speed_options(bal_weight = -0.5), "bal_weight must be non-negative")
  expect_error(speed_options(adj_weight = "0"), "adj_weight must be non-negative")
  expect_error(speed_options(bal_weight = c(1, 2)), "bal_weight must be a single value")
  expect_error(speed_options(adj_weight = NA), "adj_weight must be a single value")
})

test_that("speed_options validates logical parameters correctly", {
  expect_error(speed_options(swap_all_blocks = "TRUE"), "swap_all_blocks must be TRUE or FALSE")
  expect_error(speed_options(adaptive_swaps = 1), "adaptive_swaps must be TRUE or FALSE")
  expect_error(speed_options(random_initialisation = c(TRUE, FALSE)), "random_initialisation must be a single value")
  expect_error(speed_options(swap_all_blocks = NA), "swap_all_blocks must be a single value")
})

test_that("speed_options rejects unknown option names", {
  expect_error(speed_options("unknown_option"), "Unknown option: unknown_option")
  expect_error(speed_options(unknown_option = 1), "Unknown option: unknown_option")
  expect_error(speed_options(swap_count = 1, unknown = 2), "Unknown option: unknown")
})

test_that("speed_options rejects mixed named and unnamed arguments", {
  expect_error(speed_options("swap_count", cooling_rate = 0.95), 
               "Cannot mix named and unnamed arguments")
})

test_that("speed_options check mode works correctly", {
  # Should return TRUE invisibly when all options are valid
  result <- speed_options(check = TRUE)
  expect_true(result)
  expect_invisible(speed_options(check = TRUE))
})

test_that("speed_options check mode with verbose works correctly", {
  # Capture output
  output <- capture.output({
    result <- speed_options(check = TRUE, verbose = TRUE)
  })
  
  expect_true(result)
  expect_true(any(grepl("Speed package options:", output)))
  expect_true(any(grepl("swap_count:", output)))
  expect_true(any(grepl("All options are valid", output)))
})

test_that("speed_options check mode detects invalid options", {
  # Set an invalid option manually
  options(speed.swap_count = -1)
  
  expect_error(speed_options(check = TRUE), "swap_count must be a positive integer")
  
  # Reset to valid value
  options(speed.swap_count = 1)
})

test_that("speed_options handles edge cases for valid values", {
  # Store original values
  original <- speed_options()
  
  # Test edge cases that should work
  expect_silent(speed_options(swap_count = 1))
  expect_silent(speed_options(start_temp = 0.001))
  expect_silent(speed_options(cooling_rate = 0.001))
  expect_silent(speed_options(cooling_rate = 0.999))
  expect_silent(speed_options(adj_weight = 0))
  expect_silent(speed_options(bal_weight = 0))
  expect_silent(speed_options(swap_all_blocks = TRUE))
  expect_silent(speed_options(swap_all_blocks = FALSE))
  
  # Reset all to original values using do.call
  do.call(speed_options, original)
})

test_that("speed_options preserves option state across calls", {
  # Set some options
  speed_options(swap_count = 10, start_temp = 200)
  
  # Get options in different call
  result <- speed_options("swap_count", "start_temp")
  expect_equal(result$swap_count, 10)
  expect_equal(result$start_temp, 200)
  
  # Reset
  speed_options(swap_count = 1, start_temp = 100)
})
