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

#' Base Type of a Column for Round-Tripping
#'
#' Maps a column to a single base type name (usable as `as.<type>()`) so its
#' type can be restored after the SA loop. Columns with an exotic or
#' multi-class `class()` (e.g. \pkg{vctrs}-backed columns such as those in an
#' \pkg{edibble} design) cannot be reconstructed with an `as.<class>()`
#' function, so they are restored as `character`.
#'
#' @param x A vector (data frame column)
#'
#' @returns A length-1 character string naming a base type.
#'
#' @keywords internal
base_type <- function(x) {
  if (is.factor(x)) {
    "factor"
  } else if (is.integer(x)) {
    "integer"
  } else if (is.logical(x)) {
    "logical"
  } else if (is.numeric(x)) {
    "numeric"
  } else {
    # character, or any vctrs / multi-class column
    "character"
  }
}

#' Convert Data Frame Data to Factors
#'
#' Converts the named columns to factors, which is what the SA loop requires.
#' Names not present in `df` are ignored, so a caller may pass the `"1"` /
#' `"none"` placeholder used for a level with no `swap_within` boundary. Columns
#' outside `cols` are left untouched and are not recorded in `input_types`, so
#' [to_types()] returns them exactly as they came in - the only way to preserve
#' a class [base_type()] cannot rebuild, such as `Date`.
#'
#' @param df A data frame
#' @param cols Names of the columns to convert (default: every column).
#'
#' @returns A list containing:
#' - **df** - A data frame with the named columns as factors
#' - **input_types** - A named character vector of the original base type of
#'   each converted column (for restoring via [to_types()])
#'
#' @keywords internal
to_factor <- function(df, cols = names(df)) {
  cols <- intersect(cols, names(df))
  input_types <- vapply(df[cols], base_type, character(1))
  for (col in cols) {
    if (!is.factor(df[[col]])) {
      df[[col]] <- as.factor(df[[col]])
    }
  }

  return(list(df = df, input_types = input_types))
}

#' Convert Data Frame Data to Provided Types
#'
#' Columns are converted via `as.<type>()`. Factors are routed through
#' [as.character()] first, because `as.numeric()` and friends applied to a
#' factor return its integer level codes rather than its labels - the labels are
#' what hold the original values. Columns whose target type is `factor` are left
#' as-is, since re-factoring would re-sort the levels.
#'
#' @inheritParams to_factor
#' @param types A named list of the types for each column
#'
#' @returns A data frame with new types
#'
#' @keywords internal
to_types <- function(df, types) {
  df[names(types)] <- mapply(
    \(t, x) {
      if (is.factor(x) && t != "factor") {
        x <- as.character(x)
      }
      get(sprintf("as.%s", t), mode = "function")(x)
    },
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

#' Value a Single Level Takes From an Argument of `speed()`
#'
#' @description
#' A named list naming only levels of the design supplies a value per level;
#' anything else applies whole to every level. Matching on the level names is
#' what tells `iterations = list(wp = 5, sp = 7)` apart from `grid_factors` and
#' `optim_params()`, which are named lists of their own fields rather than of
#' levels.
#'
#' @param value An argument of [speed()], as the user passed it.
#' @param level Name of the level being built.
#' @param levels Names of every level.
#'
#' @return The value for `level`; `NULL` where a per-level list omits it, which
#'   leaves the caller to fall back to `.DEFAULT`.
#'
#' @keywords internal
.level_value <- function(value, level, levels) {
  if (
    is.list(value) && !is.null(names(value)) && all(names(value) %in% levels)
  ) {
    return(value[[level]])
  }

  return(value)
}

#' Create Input for Internal speed Function
#'
#' @inheritParams speed
#'
#' @keywords internal
create_speed_input <- function(
  swap,
  swap_within,
  spatial_factors,
  grid_factors,
  iterations,
  early_stop_iterations,
  obj_function,
  swap_all,
  optimise_params,
  linked_cols = NULL,
  optimise = NULL,
  row_col_inferred = TRUE
) {
  speed_args <- c(
    "swap",
    "swap_within",
    "spatial_factors",
    "grid_factors",
    "iterations",
    "early_stop_iterations",
    "obj_function",
    "swap_all",
    "optimise_params",
    "linked_cols"
  )

  # Name the levels. `optimise` names them itself; otherwise they come from
  # `swap`, or there is the one level described by the arguments.
  if (is.null(optimise)) {
    optimise <- list()
    if (is.list(swap)) {
      for (optimise_name in names(swap)) {
        optimise[[optimise_name]] <- list()
      }
    } else {
      optimise[[paste(
        ifelse(swap_all, "all", "single"),
        swap,
        "within",
        ifelse(swap_within %in% c("1", "none"), "whole design", swap_within),
        sep = " "
      )]] <- list()
    }
  }

  # Fill each level from the arguments, so all three input shapes resolve by the
  # same rule. A value a level sets itself wins; `.DEFAULT` covers a per-level
  # list that names some levels but not others.
  for (optimise_name in names(optimise)) {
    for (arg in speed_args) {
      optimise[[optimise_name]][[arg]] <- optimise[[optimise_name]][[arg]] %||%
        .level_value(get(arg), optimise_name, names(optimise)) %||%
        .DEFAULT[[arg]]
    }
  }

  if (!row_col_inferred) {
    for (optimise_name in names(optimise)) {
      optimise[[optimise_name]]$optimise_params$adj_weight <- 0
    }
  }

  return(optimise)
}

#' Columns a Single Level Treats as Fixed Layout
#'
#' @description
#' The columns that say where a plot sits and which group it belongs to. Unlike
#' the `swap` column these never move, so they can be neither optimised by
#' another level nor linked to one.
#'
#' @param opt One level of the `optimise` list.
#'
#' @return A character vector of column names.
#'
#' @keywords internal
.level_fixed_cols <- function(opt) {
  return(unique(c(
    opt$swap_within,
    all.vars(opt$spatial_factors),
    unlist(opt$grid_factors)
  )))
}

#' Columns a Single Level Optimises or Scores On
#'
#' @description
#' Everything one level of `optimise` needs present in the design: the column it
#' swaps, plus the fixed layout columns it groups and scores by.
#'
#' @inheritParams .level_fixed_cols
#'
#' @return A character vector of column names.
#'
#' @keywords internal
.level_optimised_cols <- function(opt) {
  return(unique(c(opt$swap, .level_fixed_cols(opt))))
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
