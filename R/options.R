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
#'   \item{`speed.start_temp`}{Starting temperature for simulated annealing (default: 100).}
#'   \item{`speed.cooling_rate`}{Rate at which temperature decreases for simulated annealing (default: 0.99).}
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
