test_that("speed:::.onAttach does not print message when versions match", {
  local_mocked_bindings(
    get_package_version = function(pkg) package_version("0.1.0"),
    read_lines_wrapper = function(url, warn = FALSE) {
      c("Package: speed", "Version: 0.1.0", "Title: Test Package")
    }
  )

  # Should not produce a message when versions match
  expect_silent(speed:::.onAttach("test", "speed"))
})

test_that("speed:::.onAttach prints message when newer version is available", {
  local_mocked_bindings(
    get_package_version = function(pkg) package_version("0.1.0"),
    read_lines_wrapper = function(url, warn = FALSE) {
      c("Package: speed", "Version: 0.2.0", "Title: Test Package")
    }
  )

  # Should produce a message when newer version is available
  expect_message(
    speed:::.onAttach("test", "speed"),
    "A newer version of speed is available"
  )

  expect_message(
    speed:::.onAttach("test", "speed"),
    "installed: 0.1.0, available: 0.2.0"
  )
})

test_that("speed:::.onAttach does not print message when local version is newer", {
  local_mocked_bindings(
    get_package_version = function(pkg) package_version("0.3.0"),
    read_lines_wrapper = function(url, warn = FALSE) {
      c("Package: speed", "Version: 0.2.0", "Title: Test Package")
    }
  )

  # Should not produce a message when local version is newer
  expect_silent(speed:::.onAttach("test", "speed"))
})

test_that("speed:::.onAttach silently fails on network error", {
  local_mocked_bindings(
    get_package_version = function(pkg) package_version("0.1.0"),
    read_lines_wrapper = function(url, warn = FALSE) {
      stop("Could not connect to remote")
    }
  )

  # Should silently fail without error
  expect_silent(speed:::.onAttach("test", "speed"))
})

test_that("speed:::.onAttach handles malformed DESCRIPTION file", {
  local_mocked_bindings(
    get_package_version = function(pkg) package_version("0.1.0"),
    read_lines_wrapper = function(url, warn = FALSE) {
      c("Package: speed", "Title: Test Package")
    }
  )

  # Should silently fail without error
  expect_silent(speed:::.onAttach("test", "speed"))
})

test_that("speed:::.onAttach handles empty remote response", {
  local_mocked_bindings(
    get_package_version = function(pkg) package_version("0.1.0"),
    read_lines_wrapper = function(url, warn = FALSE) character(0)
  )

  # Should silently fail without error
  expect_silent(speed:::.onAttach("test", "speed"))
})

test_that("speed:::.onAttach detects version with multiple components", {
  local_mocked_bindings(
    get_package_version = function(pkg) package_version("1.2.3"),
    read_lines_wrapper = function(url, warn = FALSE) {
      c("Package: speed", "Version: 1.2.4", "Title: Test Package")
    }
  )

  # Should detect newer version with multiple components
  expect_message(
    speed:::.onAttach("test", "speed"),
    "installed: 1.2.3, available: 1.2.4"
  )
})

test_that("speed:::.onAttach detects major version difference", {
  local_mocked_bindings(
    get_package_version = function(pkg) package_version("0.9.9"),
    read_lines_wrapper = function(url, warn = FALSE) {
      c("Package: speed", "Version: 1.0.0", "Title: Test Package")
    }
  )

  # Should detect major version update
  expect_message(
    speed:::.onAttach("test", "speed"),
    "A newer version of speed is available"
  )
})

test_that("speed:::.onAttach message includes update instructions", {
  local_mocked_bindings(
    get_package_version = function(pkg) package_version("1.0.0"),
    read_lines_wrapper = function(url, warn = FALSE) {
      c("Package: speed", "Version: 1.1.0", "Title: Test Package")
    }
  )

  # Check that message includes update instructions
  expect_message(
    speed:::.onAttach("test", "speed"),
    "devtools::install_github"
  )

  # Check that message includes correct repo
  expect_message(
    speed:::.onAttach("test", "speed"),
    "biometryhub/speed"
  )

  # Check that message includes both versions
  msg <- capture_messages(speed:::.onAttach("test", "speed"))
  expect_true(grepl("1.0.0", msg))
  expect_true(grepl("1.1.0", msg))
})
