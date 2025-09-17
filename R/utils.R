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

#' Convert Data Frame Data to Factors
#'
#' @param df A data frame
#'
#' @returns A list containing:
#' - **df** - A data frame with factors
#' - **input_types** - A named list of the original type of each column
#'
#' @keywords internal
to_factor <- function(df) {
  input_types <- sapply(df, class)
  for (col in names(df)) {
    if (input_types[col] != "factor") {
      df[[col]] <- as.factor(df[[col]])
    }
  }

  return(list(df = df, input_types = input_types))
}

# TODO: generalize this, not excluding factors
#' Convert Data Frame Data to Provided Types
#'
#' @inheritParams to_factor
#' @param types A named list of the types for each column
#'
#' @returns A data frame with new types
#'
#' @keywords internal
to_types <- function(df, types) {
  for (col in names(df)) {
    if (col %in% names(types) && types[col] != "factor") {
      df[[col]] <- do.call(paste0("as.", types[[col]]), list(df[[col]]))
    }
  }

  return(df)
}

#' Add Names to A List
#'
#' @description
#' Add names to a list if not exist or fill in missing names.
#'
#' @param a_list A list
#'
#' @return A named list
#'
#' @keywords internal
add_names <- function(a_list) {
  if (is.null(names(a_list))) {
    names(a_list) <- seq_along(a_list)
  } else {
    existing_names <- new.env()
    for (name in names(a_list)) {
      if (name != "") {
        existing_names[[name]] <- TRUE
      }
    }

    running_name <- 1
    for (i in seq_along(a_list)) {
      if (names(a_list)[[i]] == "") {
        while (exists(as.character(running_name), existing_names)) {
          running_name <- running_name + 1
        }

        names(a_list)[[i]] <- running_name
        running_name <- running_name + 1
      }
    }
  }

  return(a_list)
}

#' `rbind` for Unequal Columns
#'
#' @param ... Data frames to be combined
#' @param fill A filling value for missing columns (default: `NA`)
#'
#' @return A combined data frame
#'
#' @keywords internal
rbind_fill <- function(..., fill = NA) {
  dfs <- list(...)
  all_cols <- unique(unlist(lapply(dfs, names)))

  dfs_filled <- lapply(dfs, function(df) {
    if (length(df) == 0) {
      return(df)
    }

    missing_cols <- setdiff(all_cols, names(df))
    for (col in missing_cols) {
      df[[col]] <- fill
    }

    df <- df[all_cols]
    return(df)
  })

  return(do.call(rbind, dfs_filled))
}
