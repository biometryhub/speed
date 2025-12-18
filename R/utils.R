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
  svd_a <- svd(a_matrix)
  rank_a <- sum(svd_a$d > tolerance)

  # Moore-Penrose inverse (variance matrix)
  if (rank_a > 0) {
    diag_values <- numeric(ncol(svd_a$v))
    diag_values[1:rank_a] <- 1 / svd_a$d[1:rank_a]
    return(svd_a$v %*% diag(diag_values) %*% t(svd_a$u))
  } else {
    matrix_name <- deparse(substitute(a_matrix))
    stop(paste0(matrix_name, " has rank 0 - design may be invalid"))
  }
}

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

#' Convert Data Frame Data to Provided Types
#'
#' @inheritParams to_factor
#' @param types A named list of the types for each column
#'
#' @returns A data frame with new types
#'
#' @keywords internal
to_types <- function(df, types) {
  df[names(types)] <- mapply(
    \(t, x) get(sprintf("as.%s", t), mode = "function")(x),
    types,
    df[names(types)],
    SIMPLIFY = FALSE
  )
  return(df)
}

# parse_swap_formula <- function(formula) {
#   # split a + b expression recursively
#   split_terms <- function(expr) {
#     if (is.call(expr) && identical(expr[[1]], as.name("+"))) {
#       return(c(split_terms(expr[[2]]), split_terms(expr[[3]])))
#     } else {
#       return(list(expr))
#     }
#   }
#
#   parse_term <- function(call_expr) {
#     fn_name <- as.character(call_expr[[1]])
#     args <- as.list(call_expr[-1])
#
#     return(list(
#       fn_name,
#       if (length(args) >= 1) all.vars(args[[1]])[1] else stop("Missing first argument"),
#       if (length(args) >= 2) all.vars(args[[2]]) else c("row", "col"),
#       if (length(args) >= 3) all.vars(args[[3]])[1] else "1"
#     ))
#   }
#
#   rhs <- formula[[2]]
#   terms <- split_terms(rhs)
#
#
#   parsed_args <- lapply(terms, parse_term)
#   names(parsed_args) <- sapply(
#     parsed_args,
#     function(swap) {
#       paste0(
#         swap[[1]],
#         " ",
#         swap[[2]],
#         " within ",
#         ifelse(swap[[4]] == "1", "whole design", swap[[4]])
#       )
#     }
#   )
#
#   return(parsed_args)
# }

#' Create Input for Internal speed Function
#'
#' @inheritParams speed
#'
#' @keywords internal
create_speed_input <- function(swap,
                               swap_within,
                               spatial_factors,
                               grid_factors,
                               iterations,
                               early_stop_iterations,
                               obj_function,
                               swap_all,
                               optimize_params,
                               optimize = NULL,
                               row_col_inferred = TRUE) {
  speed_args <- c(
    "swap",
    "swap_within",
    "spatial_factors",
    "grid_factors",
    "iterations",
    "early_stop_iterations",
    "obj_function",
    "swap_all",
    "optimize_params"
  )

  if (!is.null(optimize)) {
    for (optimize_name in names(optimize)) {
      for (arg in speed_args) {
        if (is.null(optimize[[optimize_name]][[arg]])) {
          optimize[[optimize_name]][[arg]] <- get(arg)
        }
      }

      # if (!row_col_inferred) {
      #   optimize[[optimize_name]]$optimize_params$adj_weight <- 0
      # }
    }
  } else if (is.list(swap)) {
    optimize <- list()
    for (optimize_name in names(swap)) {
      optimize[[optimize_name]] <- list(
        swap = swap[[optimize_name]],
        swap_within = swap_within[[optimize_name]] %||% .DEFAULT$swap_within,
        grid_factors = if (is.list(grid_factors[[1]])) {
          grid_factors[[optimize_name]] %||% .DEFAULT$grid_factors
        } else {
          grid_factors
        },
        optimize_params = if (is.list(optimize_params[[1]])) {
          optimize_params[[optimize_name]] %||% list()
        } else {
          optimize_params
        }
      )

      for (arg in speed_args) {
        if (!(arg %in% c("swap", "swap_within", "grid_factors", "optimize_params"))) {
          if (is.null(optimize[[optimize_name]][[arg]])) {
            optimize_var <- get(arg)
            optimize[[optimize_name]][[arg]] <- if (is.list(optimize_var)) {
              optimize_var[[optimize_name]] %||% .DEFAULT$spatial_factors
            } else {
              optimize_var
            }
          }
        }
      }
    }
  } else {
    optimize <- list()
    optimize_name <- paste(
      ifelse(swap_all, "all", "single"),
      swap,
      "within",
      ifelse(swap_within %in% c("1", "none"), "whole design", swap_within),
      sep = " "
    )

    optimize[[optimize_name]] <- list(
      swap = swap,
      swap_within = swap_within,
      spatial_factors = spatial_factors,
      grid_factors = grid_factors,
      iterations = iterations,
      early_stop_iterations = early_stop_iterations,
      obj_function = obj_function,
      swap_all = swap_all,
      optimize_params = optimize_params
    )
  }

  if (!row_col_inferred) {
    for (optimize_name in names(optimize)) {
      optimize[[optimize_name]]$optimize_params$adj_weight <- 0
    }
  }

  return(optimize)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

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

#' Convert Factor to Numeric
#'
#' @param x A factor
#' @returns A numeric vector
#' @keywords internal
as_numeric_factor <- function(x) as.numeric(as.character(x))
