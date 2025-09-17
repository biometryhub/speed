#' Package Options for `speed`
#'
#' This page describes the options you can set to control the behaviour of the `speed`
#' package, especially technical options in the `speed()` function controlling the
#' behaviour of the optimisation algorithm.
#'
#' \describe{
#'   \item{`speed.swap_count`}{Number of treatment swaps per iteration (default: 1).}
#'   \item{`speed.swap_all_blocks`}{Logical; if `TRUE`, performs swaps in all blocks at each iteration (default: `FALSE`).}
#'   \item{`speed.adaptive_swaps`}{Logical; if `TRUE`, adjusts swap parameters based on temperature (default: `FALSE`).}
#'   \item{`speed.start_temp`}{Starting temperature for simulated annealing (default: 100). A higher start temperature allows 
#' the algorithm to accept worse solutions early on, encouraging exploration of the solution space and helping to avoid local 
#' optima. Lower values make the algorithm greedier from the start, which can speed up convergence but increases the risk of 
#' getting stuck in a poor solution.
#' A good starting temperature allows moderately worse solutions to be accepted with a probability of 70–90% at the beginning of the optimisation.}
#'   \item{`speed.cooling_rate`}{Rate at which temperature decreases for simulated annealing (default: 0.99).
#'   This controls how quickly the algorithm shifts from exploration to exploitation.
#'        The temperature is updated at each iteration by multiplying it by this rate:  
#'        `T_i = start_temp * cooling_rate^i`.  
#'        A higher cooling rate (e.g. 0.995–0.999) results in slower cooling and a longer
#'        exploration phase, which is generally better for complex or noisy optimisation
#'        landscapes. Lower values (e.g. 0.95–0.98) cool quickly, leading to faster
#'        convergence but greater risk of premature convergence to a suboptimal design.}
#'   \item{`speed.random_initialisation`}{Logical; if TRUE, randomly shuffle items within `swap_within` (default: FALSE)}
#'   \item{`speed.adj_weight`}{Weight for adjacency score (default: 0).}
#'   \item{`speed.bal_weight`}{Weight for balance score (default: 1).}
#' }
#'
#' @section Setting options:
#' You can set these options using [base::options()], either at the start of a session
#' or within your code:
#'
#' ```r
#' options(speed.swap_count = 5, speed.swap_all_blocks = TRUE)
#' ```
#'
#' @name speed-options
NULL
