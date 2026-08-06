# `add_buffers()` is deprecated (buffers are moving to biometryassist) and warns
# on every call. Most buffer tests exercise the layout behaviour, not the
# deprecation, so they call this wrapper; the warning itself is asserted once, in
# test-buffers.R.
add_buffers_quiet <- function(...) {
  return(suppressWarnings(add_buffers(...)))
}
