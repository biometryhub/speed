#' Add One to Environment
#'
#' @description
#' Add one to the value of a key in an environment if exists, otherwise set one to the key.
#'
#' @param env An environment
#' @param key A key in the environment
#'
#' @keywords internal
env_add_one <- function(env, key) {
  if (is.null(env[[key]])) {
    env[[key]] <- 1
  } else {
    env[[key]] <- env[[key]] + 1
  }
}

#' Pseudo Inverse
#'
#' @description
#' Calculates a Moore-Penrose pseudo inverse of a matrix.
#'
#' @param a_matrix A matrix
#' @param tolerance A tolerance value for singular values
#'
#' @return A Moore-Penrose pseudo inverse of the design matrix.
#'
#' @keywords internal
pseudo_inverse <- function(a_matrix, tolerance = 1e-10) {
  matrix_name <- deparse(substitute(a_matrix))
  svd_a <- svd(a_matrix)
  rank_a <- sum(svd_a$d > tolerance)

  # Moore-Penrose inverse (variance matrix)
  if (rank_a > 0) {
    return(
      svd_a$v[, 1:rank_a] %*%
        diag(1 / svd_a$d[1:rank_a]) %*%
        t(svd_a$u[, 1:rank_a])
    )
  } else {
    stop(paste0(matrix_name, " has rank 0 - design may be invalid"))
  }
}

# #' Evaluate NSE While Allowing Wrapping
# #'
# #' @description
# #' Evaluates NSE while allowing wrapping of the variable in another function call.
# #'
# #' @param var A symbol or a string
# #'
# #' @return A string representing the symbol or the literal string
# #'
# #' @keywords internal
# wrappable_nse <- function(var) {
#   expr <- substitute(var)
#
#   # Try to evaluate to see if it's a string value being passed
#   tryCatch(
#     {
#       # If we can evaluate it and it's a string, use the string value
#       evaluated <- eval(expr, envir = parent.frame())
#       if (is.character(evaluated)) {
#         return(evaluated)
#       } else {
#         # If it evaluates but isn't a string, treat as NSE (return symbol name)
#         return(deparse(expr))
#       }
#     },
#     error = function(e) {
#       # If evaluation fails (undefined variable), treat as NSE (return symbol name)
#       return(deparse(expr))
#     }
#   )
# }
