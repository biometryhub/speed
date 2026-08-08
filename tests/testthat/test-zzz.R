test_that("speed:::.onAttach does not print message when versions match", {
  rlang::local_interactive(TRUE)
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
  rlang::local_interactive(TRUE)
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
  rlang::local_interactive(TRUE)
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
  rlang::local_interactive(TRUE)
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
  rlang::local_interactive(TRUE)
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
  rlang::local_interactive(TRUE)
  local_mocked_bindings(
    get_package_version = function(pkg) package_version("0.1.0"),
    read_lines_wrapper = function(url, warn = FALSE) character(0)
  )

  # Should silently fail without error
  expect_silent(speed:::.onAttach("test", "speed"))
})

test_that("speed:::.onAttach detects version with multiple components", {
  rlang::local_interactive(TRUE)
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
  rlang::local_interactive(TRUE)
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
  rlang::local_interactive(TRUE)
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

test_that(".onAttach skips the version check when not interactive", {
  called <- FALSE
  local_mocked_bindings(
    read_lines_wrapper = function(url, warn = FALSE) {
      called <<- TRUE
      character(0)
    }
  )

  # testthat runs non-interactively, so nothing should reach the network
  expect_silent(speed:::.onAttach("test", "speed"))
  expect_false(called)
})

test_that(".onAttach skips the version check when SPEED_NO_VERSION_CHECK is set", {
  rlang::local_interactive(TRUE)
  withr::local_envvar(SPEED_NO_VERSION_CHECK = "1")
  called <- FALSE
  local_mocked_bindings(
    get_package_version = function(pkg) package_version("0.1.0"),
    read_lines_wrapper = function(url, warn = FALSE) {
      called <<- TRUE
      c("Package: speed", "Version: 9.9.9")
    }
  )

  # A newer version is available, but the opt-out must suppress the check
  expect_silent(speed:::.onAttach("test", "speed"))
  expect_false(called)
})

test_that("get_package_version reads the installed version", {
  # The real accessor; every other test here mocks it away.
  version <- speed:::get_package_version("speed")

  expect_s3_class(version, "package_version")
  expect_equal(version, utils::packageVersion("speed"))
})

test_that("read_lines_wrapper reads a connection without leaking its timeout", {
  path <- withr::local_tempfile()
  writeLines(c("Package: speed", "Version: 9.9.9"), path)
  before <- getOption("timeout")

  expect_equal(
    speed:::read_lines_wrapper(path, warn = FALSE),
    c("Package: speed", "Version: 9.9.9")
  )
  # the shortened timeout is for the version check only, not the session
  expect_equal(getOption("timeout"), before)
})
