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
#' @param df A data frame
#' @param cols Names of the columns to convert, defaulting to every column.
#'   Columns outside this set are left untouched and are not recorded in
#'   `input_types`, so [to_types()] returns them exactly as they came in. This
#'   is the only way to preserve a class [base_type()] cannot rebuild, such as
#'   `Date`, and is why `speed()` converts only the columns it optimises on.
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
    "optimise_params"
  )

  if (!is.null(optimise)) {
    for (optimise_name in names(optimise)) {
      for (arg in speed_args) {
        if (is.null(optimise[[optimise_name]][[arg]])) {
          optimise[[optimise_name]][[arg]] <- get(arg)
        }
      }

      # if (!row_col_inferred) {
      #   optimise[[optimise_name]]$optimise_params$adj_weight <- 0
      # }
    }
  } else if (is.list(swap)) {
    optimise <- list()
    for (optimise_name in names(swap)) {
      optimise[[optimise_name]] <- list(
        swap = swap[[optimise_name]],
        swap_within = swap_within[[optimise_name]] %||% .DEFAULT$swap_within,
        grid_factors = if (is.list(grid_factors[[1]])) {
          grid_factors[[optimise_name]] %||% .DEFAULT$grid_factors
        } else {
          grid_factors
        },
        optimise_params = if (is.list(optimise_params[[1]])) {
          optimise_params[[optimise_name]] %||% list()
        } else {
          optimise_params
        }
      )

      for (arg in speed_args) {
        if (
          !(arg %in%
            c("swap", "swap_within", "grid_factors", "optimise_params"))
        ) {
          if (is.null(optimise[[optimise_name]][[arg]])) {
            optimise_var <- get(arg)
            optimise[[optimise_name]][[arg]] <- if (is.list(optimise_var)) {
              optimise_var[[optimise_name]] %||% .DEFAULT$spatial_factors
            } else {
              optimise_var
            }
          }
        }
      }
    }
  } else {
    optimise <- list()
    optimise_name <- paste(
      ifelse(swap_all, "all", "single"),
      swap,
      "within",
      ifelse(swap_within %in% c("1", "none"), "whole design", swap_within),
      sep = " "
    )

    optimise[[optimise_name]] <- list(
      swap = swap,
      swap_within = swap_within,
      spatial_factors = spatial_factors,
      grid_factors = grid_factors,
      iterations = iterations,
      early_stop_iterations = early_stop_iterations,
      obj_function = obj_function,
      swap_all = swap_all,
      optimise_params = optimise_params
    )
  }

  # `linked_cols` resolves per level - a bare character vector applies to every
  # level, a named list only to the levels it names - so it is set here rather
  # than in each branch above. A per-level value from `optimise` wins.
  for (optimise_name in names(optimise)) {
    level_cols <- optimise[[optimise_name]][["linked_cols"]]
    optimise[[optimise_name]][["linked_cols"]] <- level_cols %||%
      .level_linked_cols(linked_cols, optimise_name)
  }

  if (!row_col_inferred) {
    for (optimise_name in names(optimise)) {
      optimise[[optimise_name]]$optimise_params$adj_weight <- 0
    }
  }

  return(optimise)
}

#' Resolve `linked_cols` for A Single Level
#'
#' @description
#' `linked_cols` is either a bare character vector, which applies to every
#' level, or a named list with one entry per hierarchy level. Resolution only -
#' the input is checked by [.verify_linked_cols()].
#'
#' @inheritParams speed
#' @param level Name of the hierarchy level being resolved.
#'
#' @return A character vector of column names, or `NULL`.
#'
#' @keywords internal
.level_linked_cols <- function(linked_cols, level) {
  # `[[` yields NULL for a level the list does not name, so a missing level needs
  # no guard of its own. Anything else - a bare vector, or NULL - applies as-is.
  if (is.list(linked_cols)) {
    return(linked_cols[[level]])
  }

  return(linked_cols)
}

#' Map Each Linked Column to The Swap Column It Travels With
#'
#' @param optimise Per-level `optimise` list as built by [create_speed_input()].
#'
#' @return A named character vector; names are linked column names, values are
#'   the swap column each one follows. Empty when no level uses `linked_cols`.
#'
#' @keywords internal
.linked_col_map <- function(optimise) {
  map <- character(0)
  for (level in names(optimise)) {
    cols <- optimise[[level]]$linked_cols
    for (col in cols) {
      map[[col]] <- optimise[[level]]$swap
    }
  }

  return(map)
}

#' Name One Provenance Index Column per Distinct Swap Column
#'
#' @description
#' Provenance is tracked per *swap column*, not per level: two levels that
#' optimise the same column (as in a MET design) share one index so it
#' accumulates both passes, while levels with different swap columns get
#' independent indices.
#'
#' @param linked_map A named character vector from `.linked_col_map()`.
#'
#' @return A named character vector mapping swap column to index column name.
#'
#' @keywords internal
.origin_col_names <- function(linked_map) {
  swap_cols <- unique(unname(linked_map))
  if (length(swap_cols) == 0) {
    return(character(0))
  }

  # Timestamped like `dummy_<timestamp>` in `speed()` so the name cannot collide
  # with a user column. It is stripped before returning, so it is never seen.
  origin_cols <- paste0(
    ".origin_",
    seq_along(swap_cols),
    "_",
    as.integer(Sys.time())
  )
  names(origin_cols) <- swap_cols

  return(origin_cols)
}

#' Set Linked Columns Aside Before The Search
#'
#' @description
#' Linked columns take no part in scoring, so they are lifted out of `data` and
#' held until the design is returned - leaving them out avoids copying them on
#' every iteration. Each swap column carrying linked columns is given a
#' provenance index (see [.origin_col_names()]) that follows its treatment
#' through the swaps, which is how the held values are put back in the
#' optimised order.
#'
#' @param data The design data frame, as the user passed it.
#' @param optimise Per-level `optimise` list as built by [create_speed_input()].
#'
#' @return A list holding the stripped `data`, `optimise` with each level's
#'   `origin_col` set, the held `values`, the linked column `map`, the
#'   `origin_cols` names and the input `col_order`.
#'
#' @keywords internal
.detach_linked_cols <- function(data, optimise) {
  map <- .linked_col_map(optimise)
  origin_cols <- .origin_col_names(map)
  col_order <- names(data)

  for (level in names(optimise)) {
    swap_col <- optimise[[level]]$swap
    if (swap_col %in% names(origin_cols)) {
      optimise[[level]]$origin_col <- unname(origin_cols[[swap_col]])
    }
  }

  values <- NULL
  if (length(map) > 0) {
    values <- data[names(map)]
    data[names(map)] <- NULL
  }

  return(list(
    data = data,
    optimise = optimise,
    values = values,
    map = map,
    origin_cols = origin_cols,
    col_order = col_order
  ))
}

#' Re-attach Linked Columns in The Optimised Order
#'
#' @description
#' Each linked column is rebuilt by indexing the held values with the
#' provenance index, then the input column order is restored - which also drops
#' the provenance columns.
#'
#' @param design_df The optimised design data frame.
#' @param linked The list returned by [.detach_linked_cols()].
#'
#' @return `design_df`, with the linked columns back in their input positions.
#'
#' @keywords internal
.reattach_linked_cols <- function(design_df, linked) {
  if (length(linked$map) == 0) {
    return(design_df)
  }

  for (col in names(linked$map)) {
    origin_col <- linked$origin_cols[[linked$map[[col]]]]
    design_df[[col]] <- linked$values[[col]][design_df[[origin_col]]]
  }

  return(design_df[linked$col_order])
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
