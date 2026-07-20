.onAttach <- function(libname, pkgname) {
  tryCatch({
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
  }, error = function(e) {
    # Silently fail - no output if there's any error
  })
}

# Wrapper functions for easier mocking in tests
get_package_version <- function(pkg) {
  utils::packageVersion(pkg)
}

read_lines_wrapper <- function(con, warn = TRUE) {
  base::readLines(con, warn = warn)
}
