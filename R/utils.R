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

pseudo_inverse = function(a_matrix, tolerance = 1e-10) {
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
