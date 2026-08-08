# `SPEED_NO_VERSION_CHECK` suppresses the startup version check. Clear it for
# the suite so the `.onAttach()` tests still exercise the check on a machine
# where a developer has set it; the test for the opt-out sets it locally.
withr::local_envvar(
  SPEED_NO_VERSION_CHECK = "",
  .local_envir = testthat::teardown_env()
)
