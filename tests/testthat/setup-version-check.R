# Clear `SPEED_NO_VERSION_CHECK` for the suite, so the `.onAttach()` tests still
# run on a machine where a developer has set it.
withr::local_envvar(
  SPEED_NO_VERSION_CHECK = "",
  .local_envir = testthat::teardown_env()
)
