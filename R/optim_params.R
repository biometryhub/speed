#' Create and Verify Optimization Parameters
#'
#' @description
#' Create and verify optimization parameters. These parameters are used to control the behaviour of
#' simulated annealing algorithm.
#'
#' @param swap_count Number of treatment swaps per iteration (default: 1).
#' @param swap_all_blocks Logical; if `TRUE`, performs swaps in all blocks at each iteration (default: `FALSE`).
#' @param adaptive_swaps Logical; if `TRUE`, adjusts swap parameters based on temperature (default: `FALSE`).
#' @param start_temp Starting temperature for simulated annealing (default: 100). A higher start temperature
#'   allows the algorithm to accept worse solutions early on, encouraging exploration of the solution space and
#'   helping to avoid local optima. Lower values make the algorithm greedier from the start, which can speed up
#'   convergence but increases the risk of getting stuck in a poor solution. A good starting temperature allows
#'   moderately worse solutions to be accepted with a probability of 70–90% at the beginning of the optimisation.
#' @param cooling_rate Rate at which temperature decreases for simulated annealing (default: 0.99). This
#'   controls how quickly the algorithm shifts from exploration to exploitation. The temperature is updated at
#'   each iteration by multiplying it by this rate: `T_i = start_temp * cooling_rate^i`. A higher cooling rate
#'   (e.g. 0.995–0.999) results in slower cooling and a longer exploration phase, which is generally better for
#'   complex or noisy optimisation landscapes. Lower values (e.g. 0.95–0.98) cool quickly, leading to faster
#'   convergence but greater risk of premature convergence to a suboptimal design.
#' @param random_initialisation Number of times to randomly shuffle items within `swap_within`; the design with
#'   the best score is used as an initial design (default: 0).
#' @param adj_weight Weight for adjacency score (default: 1).
#' @param bal_weight Weight for balance score (default: 1).
#'
#' @returns A named list of optimization parameters.
#'
#' @examples
#' @export
optim_params <- function(swap_count = 1,
                         swap_all_blocks = FALSE,
                         adaptive_swaps = FALSE,
                         start_temp = 100,
                         cooling_rate = 0.99,
                         random_initialisation = 0,
                         adj_weight = 1,
                         bal_weight = 1) {
  params <- list(
    swap_count = swap_count,
    swap_all_blocks = swap_all_blocks,
    adaptive_swaps = adaptive_swaps,
    start_temp = start_temp,
    cooling_rate = cooling_rate,
    random_initialisation = random_initialisation,
    adj_weight = adj_weight,
    bal_weight = bal_weight
  )

  # check legacy options
  option_names <- c(
    "swap_count",
    "swap_all_blocks",
    "adaptive_swaps",
    "start_temp",
    "cooling_rate",
    "random_initialisation",
    "adj_weight",
    "bal_weight"
  )
  legacy_options <- setNames(vector("list", length(option_names)), option_names)

  is_legacy <- FALSE
  for (legacy_option in names(legacy_options)) {
    option <- getOption(paste0("speed.", legacy_option))
    legacy_options[[legacy_option]] <- option
    if (!is.null(option)) {
      is_legacy <- TRUE
      params[[legacy_option]] <- option
    }
  }

  if (is_legacy) {
    warning(
      "Setting options with `options(speed.{option}=...)` is deprecated. Please use `optim_params()` instead."
    )
  }

  do.call(.verify_optim_params, params)
  return(params)
}
