.onAttach <- function(libname, pkgname) {
  # Only useful interactively, so scripts and `R CMD check` do not hit the
  # network on attach. `SPEED_NO_VERSION_CHECK` opts out entirely.
  no_check <- nzchar(Sys.getenv("SPEED_NO_VERSION_CHECK"))
  if (!rlang::is_interactive() || no_check) {
    return(invisible(NULL))
  }

  tryCatch(
    {
      # Get the GitHub raw URL for the DESCRIPTION file
      github_desc_url <- "https://raw.githubusercontent.com/biometryhub/speed/refs/heads/main/DESCRIPTION"

      # Get current package version
      current_version <- get_package_version(pkgname)

      # Try to read the remote DESCRIPTION file
      remote_desc <- read_lines_wrapper(github_desc_url, warn = FALSE)

      # Extract version line
      version_line <- grep("^Version:", remote_desc, value = TRUE)

      if (length(version_line) > 0) {
        # Extract version number
        remote_version <- sub("^Version:\\s*", "", version_line[1])
        remote_version <- package_version(remote_version)

        # Compare versions
        if (remote_version > current_version) {
          packageStartupMessage(
            sprintf(
              "A newer version of %s is available on GitHub (installed: %s, available: %s).\nUpdate with: devtools::install_github(\"biometryhub/speed\")",
              pkgname,
              current_version,
              remote_version
            )
          )
        }
      }
    },
    error = function(e) {
      # Silently fail - no output if there's any error
    }
  )

  return(invisible(NULL))
}

# Wrapper functions for easier mocking in tests
get_package_version <- function(pkg) {
  return(utils::packageVersion(pkg))
}

read_lines_wrapper <- function(con, warn = TRUE) {
  # The 60s default is far too long to block package attach on.
  old <- options(timeout = 2)
  on.exit(options(old), add = TRUE)

  return(base::readLines(con, warn = warn))
}
