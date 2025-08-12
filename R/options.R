#' Get, set, or check speed package options
#'
#' A function to manage speed package options for controlling the optimisation algorithms.
#'
#' @section Available options:
#' \describe{
#'   \item{`swap_count`}{Number of treatment swaps per iteration (default: 1).}
#'   \item{`swap_all_blocks`}{Logical; if `TRUE`, performs swaps in all blocks at each iteration (default: `FALSE`).}
#'   \item{`adaptive_swaps`}{Logical; if `TRUE`, adjusts swap parameters based on temperature (default: `FALSE`).}
#'   \item{`start_temp`}{Starting temperature for simulated annealing (default: 100). A higher start temperature allows 
#' the algorithm to accept worse solutions early on, encouraging exploration of the solution space and helping to avoid local 
#' optima. Lower values make the algorithm greedier from the start, which can speed up convergence but increases the risk of 
#' getting stuck in a poor solution. A good starting temperature allows moderately worse solutions to be accepted with a 
#' probability of 70–90% at the beginning of the optimisation.}
#'   \item{`cooling_rate`}{Rate at which temperature decreases for simulated annealing (default: 0.99).
#'   This controls how quickly the algorithm shifts from exploration to exploitation.
#'        The temperature is updated at each iteration by multiplying it by this rate:  
#'        `T_i = start_temp * cooling_rate^i`.  
#'        A higher cooling rate (e.g. 0.995–0.999) results in slower cooling and a longer
#'        exploration phase, which is generally better for complex or noisy optimisation
#'        landscapes. Lower values (e.g. 0.95–0.98) cool quickly, leading to faster
#'        convergence but greater risk of premature convergence to a suboptimal design.}
#'   \item{`random_initialisation`}{Logical; if TRUE, randomly shuffle items within `swap_within` (default: FALSE).}
#'   \item{`adj_weight`}{Weight for adjacency score (default: 0).}
#'   \item{`bal_weight`}{Weight for balance score (default: 1).}
#' }
#'
#' @param ... Option names to retrieve (unnamed) or option values to set (named).
#' @param check Logical; if TRUE, validates all current options.
#' @param verbose Logical; if TRUE and `check = TRUE`, prints current option values.
#' @return When getting: a named list of option values. When setting: invisibly 
#'   returns previous values. When checking: invisibly returns TRUE if valid.
#' @export
#' @examples
#' # Get all speed options
#' speed_options()
#' 
#' # Get specific options
#' speed_options("swap_count", "cooling_rate")
#' 
#' # Set options
#' speed_options(swap_count = 3, cooling_rate = 0.95)
#' 
#' # Check all options
#' speed_options(check = TRUE)
#' 
#' # Check with verbose output
#' speed_options(check = TRUE, verbose = TRUE)
speed_options <- function(..., check = FALSE, verbose = FALSE) {
  # Default values for all options
  defaults <- list(
    swap_count = 1,
    swap_all_blocks = FALSE,
    adaptive_swaps = FALSE,
    start_temp = 100,
    cooling_rate = 0.99,
    random_initialisation = FALSE,
    adj_weight = 0,
    bal_weight = 1
  )
  
  # Helper function to validate a single option
  .validate_option <- function(name, value) {
    # Check for single values (no vectors allowed)
    if (length(value) != 1 || is.na(value)) {
      stop(name, " must be a single value")
    }
    
    switch(name,
      "swap_count" = if (!is.numeric(value) || value <= 0 || value != round(value)) {
        stop("swap_count must be a positive integer")
      },
      "start_temp" = if (!is.numeric(value) || value <= 0) {
        stop("start_temp must be positive")
      },
      "cooling_rate" = if (!is.numeric(value) || value <= 0 || value >= 1) {
        stop("cooling_rate must be between 0 and 1")
      },
      "adj_weight" = , "bal_weight" = if (!is.numeric(value) || value < 0) {
        stop(name, " must be non-negative")
      },
      "swap_all_blocks" = , "adaptive_swaps" = , "random_initialisation" = if (!is.logical(value)) {
        stop(name, " must be TRUE or FALSE")
      }
    )
  }
  
  # Helper function to get current options
  .get_options <- function(option_names = NULL) {
    if (is.null(option_names)) {
      option_names <- names(defaults)
    }
    
    result <- list()
    for (name in option_names) {
      if (!name %in% names(defaults)) stop("Unknown option: ", name)
      result[[name]] <- getOption(paste0("speed.", name), defaults[[name]])
    }
    result
  }
  
  
  # Handle check mode
  if (check) {
    current_options <- .get_options()
    
    if (verbose) {
      cat("Speed package options:\n")
      for (name in names(current_options)) {
        cat("  ", name, ": ", current_options[[name]], "\n", sep = "")
      }
    }
    # Validate all options
    for (name in names(current_options)) {
      .validate_option(name, current_options[[name]])
    }
    if (verbose) cat("All options are valid.\n")
    return(invisible(TRUE))
  }
  
  args <- list(...)
  
  # No arguments: return all options
  if (length(args) == 0) {
    return(.get_options())
  }
  
  # Named arguments: set options
  if (!is.null(names(args)) && all(names(args) != "")) {
    # Validate names and values
    for (name in names(args)) {
      if (!name %in% names(defaults)) {
        stop("Unknown option: ", name)
      }
      .validate_option(name, args[[name]])
    }
    
    # Get old values and set new ones
    old_values <- .get_options(names(args))
    for (name in names(args)) {
      options(setNames(list(args[[name]]), paste0("speed.", name)))
    }
    return(invisible(old_values))
  }
  
  # Unnamed arguments: get specific options
  if (is.null(names(args)) || all(names(args) == "")) {
    return(.get_options(unlist(args)))
  }
  
  stop("Cannot mix named and unnamed arguments")
}
