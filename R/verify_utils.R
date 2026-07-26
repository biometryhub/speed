#' Verify Inputs for `speed`
#'
#' @description
#' Verify inputs for the `speed` function.
#'
#' @rdname verify
#'
#' @inheritParams speed
#'
#' @keywords internal
.verify_speed_inputs <- function(data,
                                 swap,
                                 swap_within,
                                 spatial_factors,
                                 iterations,
                                 early_stop_iterations,
                                 quiet,
                                 seed) {
  if (!is.data.frame(data)) {
    stop("`data` must be an initial data frame of the design")
  }

  verify_column_exists(swap, data, "treatment")

  # currently support only 1 constraint
  if (swap_within != "1") {
    verify_column_exists(swap_within, data, "constraint")
  }

  if (!inherits(spatial_factors, "formula")) {
    stop("spatial_factors must be a one sided formula", call. = FALSE)
  }

  for (col in all.vars(spatial_factors)) {
    verify_column_exists(col, data, "spatial factor")
  }

  verify_positive_whole_number(iterations, early_stop_iterations)
  verify_boolean(quiet)
  verify_between(lower = 0, upper = 1, upper_exclude = TRUE)
  if (!is.null(seed)) {
    verify_between(seed, lower = -.Machine$integer.max, upper = .Machine$integer.max)
  }
}

#' Verify hierarchical inputs
#' @rdname verify
#' @keywords internal
.verify_hierarchical_inputs <- function(data, swap, swap_within, spatial_factors,
                                        iterations, early_stop_iterations, obj_function,
                                        quiet, seed) {
  # Check that swap and swap_within have same names
  if (!all(names(swap) == names(swap_within))) {
    stop("Names of `swap` and `swap_within` must match for hierarchical designs")
  }

  # Check that all specified columns exist in data
  for (level in names(swap)) {
    if (!swap[[level]] %in% names(data)) {
      stop(paste("Column", swap[[level]], "not found in data"))
    }
    if (!swap_within[[level]] %in% names(data) &&
      !(swap_within[[level]] %in% c("1", "none"))) {
      stop(paste("Column", swap_within[[level]], "not found in data"))
    }
  }

  # Verify other parameters
  if (!is.logical(quiet)) {
    stop("`quiet` must be logical")
  }

  if (!is.null(seed) && !is.numeric(seed)) {
    stop("`seed` must be numeric or NULL")
  }
}

#' Verify linked columns
#'
#' @description
#' Checks the columns named in `linked_cols` after they have been merged into
#' the per-level `optimise` list, so all three input shapes are covered by one
#' set of rules.
#'
#' @rdname verify
#'
#' @param optimise Per-level `optimise` list as built by [create_speed_input()].
#'
#' @keywords internal
.verify_linked_cols <- function(data, optimise, linked_cols = NULL) {
  if (is.list(linked_cols) && !is.null(names(linked_cols))) {
    unknown <- setdiff(names(linked_cols), names(optimise))
    if (length(unknown) > 0) {
      stop("`linked_cols` has no matching level for ", paste0("'", unknown, "'", collapse = ", "),
           ". Available levels: ", paste0("'", names(optimise), "'", collapse = ", "), ".",
           call. = FALSE)
    }
  }

  # Columns taking part in the optimisation cannot also travel as passengers
  reserved <- unique(unlist(lapply(optimise, function(opt) {
    c(opt$swap, opt$swap_within, all.vars(opt$spatial_factors), unlist(opt$grid_factors))
  })))

  owner <- character(0)
  for (level in names(optimise)) {
    opt <- optimise[[level]]
    cols <- opt$linked_cols
    if (length(cols) == 0) {
      next
    }

    if (!is.character(cols)) {
      stop("`linked_cols` must be a character vector of column names.", call. = FALSE)
    }

    for (col in cols) {
      verify_column_exists(col, data, "linked column")

      if (col %in% reserved) {
        stop("`linked_cols` column '", col, "' is already used as a swap, swap_within or spatial ",
             "factor column. Linked columns must be separate from the columns being optimised.",
             call. = FALSE)
      }

      if (col %in% names(owner) && owner[[col]] != opt$swap) {
        stop("`linked_cols` column '", col, "' is linked to both '", owner[[col]], "' and '",
             opt$swap, "'. A column can only travel with one swap column.", call. = FALSE)
      }
      owner[[col]] <- opt$swap

      # Multi-swap levels move whole treatment groups, which have no unit-level
      # bijection, so only functionally dependent columns are well defined there
      if (isTRUE(opt$swap_all)) {
        .verify_functionally_dependent(data, opt$swap, col, level)
      }
    }
  }

  return(invisible(NULL))
}

#' Verify a linked column has one value per treatment
#'
#' @rdname verify
#'
#' @param col Name of the linked column being checked.
#' @param level Name of the hierarchy level the column is declared on.
#'
#' @keywords internal
.verify_functionally_dependent <- function(data, swap, col, level) {
  keys <- as.character(data[[swap]])
  values <- as.character(data[[col]])
  keep <- !is.na(keys)
  if (!any(keep)) {
    return(invisible(NULL))
  }

  n_values <- tapply(values[keep], keys[keep], function(v) length(unique(v)))
  clashes <- names(n_values)[n_values > 1]
  if (length(clashes) == 0) {
    return(invisible(NULL))
  }

  example_values <- unique(values[keep][keys[keep] == clashes[1]])
  stop(
    "`linked_cols` column '", col, "' is not uniquely determined by '", swap, "' at level '",
    level, "': treatment '", clashes[1], "' has ", length(example_values), " different values (",
    paste0("'", utils::head(example_values, 3), "'", collapse = ", "),
    if (length(example_values) > 3) ", ..." else "", "). ",
    "A level using `swap_all = TRUE` moves whole treatment groups at once, so its linked ",
    "columns must have exactly one value per treatment.",
    call. = FALSE
  )
}

#' Verify Optimization Parameters for `speed`
#'
#' @rdname verify
#'
#' @keywords internal
.verify_optim_params <- function(swap_count,
                                 swap_all_blocks,
                                 adaptive_swaps,
                                 start_temp,
                                 cooling_rate,
                                 random_initialisation,
                                 adj_weight,
                                 bal_weight) {
  verify_positive_whole_number(swap_count)
  verify_non_negative_whole(start_temp)
  verify_boolean(adaptive_swaps, swap_all_blocks)
  verify_between(cooling_rate, lower = 0, upper = 1, upper_exclude = TRUE)
  verify_numeric(adj_weight, bal_weight)

  if (!(random_initialisation %in% c(TRUE, FALSE))) {
    verify_non_negative_whole(random_initialisation)
  }
}


# Other functions for verifying

default_tolerance <- .Machine$double.eps^0.5

is_between_ <- function(lower, upper, lower_exclude = FALSE, upper_exclude = FALSE) {
  return(function(x) {
    is_between <- is.numeric(x)

    if (lower_exclude) {
      is_between <- is_between && x > lower
    } else {
      is_between <- is_between && x >= lower
    }

    if (upper_exclude) {
      is_between <- is_between && x < upper
    } else {
      is_between <- is_between && x <= upper
    }

    return(is_between)
  })
}

is_boolean <- function(v) {
  return(must_be(v, c(TRUE, FALSE)))
}

is_non_negative_whole_number <- function(x, tol = default_tolerance) {
  return(is_whole_number(x, tol) & x >= 0)
}

is_multiple_of <- function(x, y) {
  return(x %% y == 0)
}

is_positive_whole_number <- function(x, tol = default_tolerance) {
  return(is_whole_number(x, tol) & x > 0)
}

is_whole_number <- function(x, tol = default_tolerance) {
  if (!is.numeric(x)) {
    return(FALSE)
  }

  return(abs(x - round(x)) < tol)
}

is_single_numeric <- function(x) {
  return(is.numeric(x) && length(x) == 1)
}

is_positive_whole_numbers <- function(x, tol = default_tolerance) {
  if (!all(is_positive_whole_number(x, tol))) {
    return(FALSE)
  }
  return(TRUE)
}

must_be <- function(x, valid_values) {
  return(must_be_(valid_values)(x))
}

must_be_ <- function(valid_values) {
  return(function(x) {
    for (v in valid_values) {
      if (identical(x, v)) {
        return(TRUE)
      }
    }

    return(FALSE)
  })
}

verify_between <- function(
    ..., lower = -Inf, upper = Inf, lower_exclude = FALSE, upper_exclude = FALSE, var_names = NULL) {
  if (lower != -Inf && upper != Inf) {
    object_type <- paste0("between ", lower)
    if (lower_exclude) {
      object_type <- paste0(object_type, " (exclusive)")
    }

    object_type <- paste0(object_type, " and ", upper)
    if (upper_exclude) {
      object_type <- paste0(object_type, " (exclusive)")
    }

    if (!lower_exclude && !upper_exclude) {
      object_type <- paste0("inclusively ", object_type)
    }
  } else if (upper == Inf) {
    if (lower_exclude) {
      object_type <- paste0("greater than ", lower)
    } else {
      object_type <- paste0("at least ", lower)
    }
  } else if (lower == -Inf) {
    if (upper_exclude) {
      object_type <- paste0("less than ", upper)
    } else {
      object_type <- paste0("at most ", upper)
    }
  }

  verify_data_type(is_between_(lower, upper, lower_exclude, upper_exclude), object_type, var_names, ...)
}

verify_boolean <- function(..., var_names = NULL) {
  verify_data_type(is_boolean, "a boolean", var_names, ...)
}

verify_column_exists <- function(col, data, suffix = NULL) {
  if (!(col %in% names(data))) {
    msg <- c(paste0("'", col, "' not found in ", paste(colnames(data), collapse = ", "), ". "), suffix)
    stop(msg, call. = FALSE)
  }
}

verify_non_negative_whole <- function(..., var_names = NULL) {
  verify_data_type(is_non_negative_whole_number, "a non-negative whole number", var_names, ...)
}

verify_multiple_of <- function(..., var_names = NULL) {
  if (is.null(var_names)) {
    var_names <- get_var_names(...)
  }

  args <- list(...)
  if (!is_multiple_of(args[[1]], args[[2]])) {
    stop(paste0("`", var_names[[1]], "` must be a multiple of `", var_names[[2]], "`."), call. = FALSE)
  }
}

verify_positive_whole_number <- function(..., var_names = NULL) {
  verify_data_type(is_positive_whole_number, "a positive whole number", var_names, ...)
}

verify_character <- function(..., var_names = NULL) {
  verify_data_type(is.character, "a character", var_names, ...)
}

verify_list <- function(..., var_names = NULL) {
  verify_data_type(is.list, "a list", var_names, ...)
}

verify_numeric <- function(..., var_names = NULL) {
  verify_data_type(is_single_numeric, "a numeric", var_names, ...)
}

verify_positive_whole_numbers <- function(..., var_names = NULL) {
  verify_data_type(is_positive_whole_numbers, "a vector of positive whole numbers", var_names, ...)
}

verify_must_be <- function(..., valid_values, var_names = NULL) {
  literal_values <- get_literal_values(valid_values)
  verify_data_type(must_be_(valid_values), literal_values, var_names, ...)
}

verify_data_type <- function(verify_func, data_type, var_names = NULL, ...) {
  if (is.null(var_names)) {
    var_names <- get_var_names(...)
  }

  args <- list(...)
  for (i in seq_along(args)) {
    v <- args[[i]]

    if (!verify_func(v)) {
      data_type_error(var_names[[i]], data_type)
    }
  }
}

get_literal_values <- function(values) {
  n_values <- length(values)
  literal_values <- literal(values[[1]])
  if (n_values == 1) {
    return(literal_values)
  }

  if (n_values == 2) {
    return(paste0(literal_values, " or ", literal(values[[2]])))
  }

  for (i in 2:n_values) {
    if (i < n_values) {
      literal_values <- paste0(literal_values, ", ", literal(values[[i]]))
    } else {
      literal_values <- paste0(literal_values, ", or ", literal(values[[i]]))
    }
  }
  return(literal_values)
}

get_var_names <- function(...) {
  raw_names <- deparse(substitute(list(...)))
  names <- substr(raw_names, 6, nchar(raw_names) - 1)
  return(strsplit(names, ", ")[[1]])
}

data_type_error <- function(var_name, expected_data_type) {
  stop(paste0("`", var_name, "` must be ", expected_data_type, "."), call. = FALSE)
}

literal <- function(v) {
  if (is.character(v)) {
    return(paste0('`"', v, '"`'))
  }

  return(paste0("`", v, "`"))
}
