#' Verify Inputs for `speed`
#'
#' @description
#' Validates the arguments as the user passed them, dispatching to the checker
#' for the input shape given and running the checks that apply to all three.
#' Checks needing the resolved per-level list - [.verify_level_columns()],
#' [.verify_linked_cols()] and [.verify_swap_all_replication()] - cannot run
#' here, and are called from [speed()] once [create_speed_input()] has built it.
#'
#' @rdname verify
#'
#' @inheritParams speed
#'
#' @param optimise The `optimise` argument as passed to [speed()] for
#'   [.verify_inputs()], or the resolved per-level list built by
#'   [create_speed_input()] for the checks that run after it.
#'
#' @keywords internal
.verify_inputs <- function(
  data,
  swap,
  swap_within,
  spatial_factors,
  grid_factors,
  iterations,
  early_stop_iterations,
  obj_function,
  quiet,
  seed,
  optimise = NULL
) {
  if (is.null(optimise)) {
    # A named list of swap columns is the legacy hierarchical shape
    if (is.list(swap) && !is.null(names(swap))) {
      .verify_hierarchical_inputs(
        data,
        swap,
        swap_within,
        spatial_factors,
        iterations,
        early_stop_iterations,
        obj_function,
        quiet,
        seed
      )
    } else {
      .verify_speed_inputs(
        data,
        swap,
        swap_within,
        spatial_factors,
        iterations,
        early_stop_iterations,
        quiet,
        seed
      )
    }
  }

  # `by` groups plots into separate grids (a multi-environment trial), and is
  # checked for every input shape, including `optimise`
  .verify_grid_by(data, grid_factors)

  return(invisible(NULL))
}

#' Verify simple inputs
#'
#' @rdname verify
#'
#' @keywords internal
.verify_speed_inputs <- function(
  data,
  swap,
  swap_within,
  spatial_factors,
  iterations,
  early_stop_iterations,
  quiet,
  seed
) {
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
    verify_between(
      seed,
      lower = -.Machine$integer.max,
      upper = .Machine$integer.max
    )
  }
}

#' Verify hierarchical inputs
#' @rdname verify
#' @keywords internal
.verify_hierarchical_inputs <- function(
  data,
  swap,
  swap_within,
  spatial_factors,
  iterations,
  early_stop_iterations,
  obj_function,
  quiet,
  seed
) {
  # Check that swap and swap_within have same names
  if (!all(names(swap) == names(swap_within))) {
    stop(
      "Names of `swap` and `swap_within` must match for hierarchical designs"
    )
  }

  # Check that all specified columns exist in data
  for (level in names(swap)) {
    if (!swap[[level]] %in% names(data)) {
      stop(paste("Column", swap[[level]], "not found in data"))
    }
    if (
      !swap_within[[level]] %in% names(data) &&
        !(swap_within[[level]] %in% c("1", "none"))
    ) {
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

#' Verify each level's columns exist
#'
#' @description
#' The `optimise` input shape bypasses both per-shape checkers, so a level
#' naming a column that is not there reaches the search unchallenged: a bad
#' `swap_within` leaves [swappable_groups()] with nothing to group by, and the
#' level is reported as frozen rather than as a mistake. Checked on the resolved
#' list so all three shapes are covered.
#'
#' @rdname verify
#'
#' @keywords internal
.verify_level_columns <- function(data, optimise) {
  for (level in names(optimise)) {
    opt <- optimise[[level]]
    verify_column_exists(opt$swap, data, "treatment")

    # `"1"` / `"none"` is the placeholder for no boundary, swapped for the dummy
    # group column later
    if (!opt$swap_within %in% c("1", "none")) {
      verify_column_exists(opt$swap_within, data, "constraint")
    }
  }

  return(invisible(NULL))
}

#' Verify linked columns
#'
#' @description
#' Checks the columns named in `linked_cols` after they have been merged into
#' the per-level `optimise` list, so all three input shapes are covered by one
#' set of rules. The rules are cross-level, so like
#' [.verify_swap_all_replication()] this runs on the resolved list rather than
#' in the per-shape checkers.
#'
#' @rdname verify
#'
#' @param named_levels Whether the levels of `optimise` carry names the user
#'   chose. `FALSE` for a simple design, whose single level name is synthesised
#'   by [create_speed_input()] and so cannot be named in `linked_cols`.
#'
#' @keywords internal
.verify_linked_cols <- function(
  data,
  optimise,
  linked_cols = NULL,
  named_levels = TRUE
) {
  if (is.list(linked_cols)) {
    if (!named_levels) {
      stop(
        "`linked_cols` must be a character vector for a non-hierarchical ",
        "design; there are no levels to name.",
        call. = FALSE
      )
    }

    if (is.null(names(linked_cols)) || any(names(linked_cols) == "")) {
      stop(
        "`linked_cols` must be a character vector, or a named list with names matching `swap`.",
        call. = FALSE
      )
    }

    unknown <- setdiff(names(linked_cols), names(optimise))
    if (length(unknown) > 0) {
      stop(
        "`linked_cols` has no matching level for ",
        paste0("'", unknown, "'", collapse = ", "),
        ". Available levels: ",
        paste0("'", names(optimise), "'", collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    for (level_cols in linked_cols) {
      verify_character(level_cols, var_names = "linked_cols")
    }
  } else if (!is.null(linked_cols)) {
    verify_character(linked_cols)
  }

  # A simple design's one level is named by `create_speed_input()`, so naming it
  # back at the user would point them at something they never wrote
  at_level <- function(level) {
    if (!named_levels) {
      return("")
    }

    return(paste0(" at level '", level, "'"))
  }

  owner <- character(0)
  for (level in names(optimise)) {
    opt <- optimise[[level]]
    cols <- opt$linked_cols
    if (length(cols) == 0) {
      next
    }

    # Also covers values set per level inside `optimise`, which the shape checks
    # above never see
    verify_character(cols, var_names = "linked_cols")

    earlier <- names(optimise)[seq_len(which(names(optimise) == level) - 1)]

    for (col in cols) {
      verify_column_exists(col, data, "linked column")

      # Checked across every level, since one level's grouping is fixed for all
      # of them
      fixes_layout <- Filter(
        function(other) {
          return(col %in% .level_fixed_cols(optimise[[other]]))
        },
        names(optimise)
      )
      if (length(fixes_layout) > 0) {
        stop(
          "`linked_cols` column '",
          col,
          "' defines the layout",
          at_level(fixes_layout[[1]]),
          " as a swap_within, spatial or grid factor, so it cannot be moved.",
          call. = FALSE
        )
      }

      if (identical(col, opt$swap)) {
        stop(
          "`linked_cols` column '",
          col,
          "' is the swap column",
          at_level(level),
          ", so it cannot also travel with itself.",
          call. = FALSE
        )
      }

      # A child treatment may ride with its parent, but only if the parent moves
      # first - otherwise this level would undo the child level's optimisation
      optimised_earlier <- Filter(
        function(other) {
          return(identical(col, optimise[[other]]$swap))
        },
        earlier
      )
      if (length(optimised_earlier) > 0) {
        stop(
          "`linked_cols` column '",
          col,
          "' is optimised at level '",
          optimised_earlier[[1]],
          "', which runs before '",
          level,
          "'. Carrying it here would undo that level's work; order the levels so ",
          "'",
          level,
          "' comes first.",
          call. = FALSE
        )
      }

      if (col %in% names(owner) && owner[[col]] != opt$swap) {
        stop(
          "`linked_cols` column '",
          col,
          "' is linked to both '",
          owner[[col]],
          "' and '",
          opt$swap,
          "'. A column can only travel with one swap column.",
          call. = FALSE
        )
      }
      owner[[col]] <- opt$swap
    }
  }

  return(invisible(NULL))
}

#' Verify Optimization Parameters for `speed`
#'
#' @rdname verify
#'
#' @inheritParams optim_params
#'
#' @keywords internal
.verify_optim_params <- function(
  swap_count,
  swap_all_blocks,
  adaptive_swaps,
  start_temp,
  cooling_rate,
  random_initialisation,
  adj_weight,
  bal_weight,
  stop_at_optimal
) {
  verify_positive_whole_number(swap_count)
  verify_non_negative_whole(start_temp)
  verify_boolean(adaptive_swaps, swap_all_blocks, stop_at_optimal)
  verify_between(cooling_rate, lower = 0, upper = 1, upper_exclude = TRUE)
  verify_numeric(adj_weight, bal_weight)

  if (!(random_initialisation %in% c(TRUE, FALSE))) {
    verify_non_negative_whole(random_initialisation)
  }
}

#' Verify equal replication for `swap_all` levels
#'
#' @description
#' `swap_all = TRUE` proposes a move by exchanging *every* plot holding one
#' treatment with *every* plot holding another. That is a rearrangement of the
#' design only when both treatments occupy the same number of plots within the
#' swap group; when they do not, the two treatments exchange replication counts
#' and the design that comes back is not the design that went in. Error before
#' any optimisation happens rather than silently altering replication.
#'
#' Called on the resolved `optimise` list, so it covers simple, legacy
#' hierarchical and `optimise = ` calls alike, including levels that set
#' `swap_all` individually.
#'
#' @param dummy_group Name of the internal placeholder column used for a level
#'   with no `swap_within` boundary, so it can be described as the whole design.
#'
#' @rdname verify
#'
#' @keywords internal
.verify_swap_all_replication <- function(data, optimise, dummy_group = NULL) {
  for (level in names(optimise)) {
    opt <- optimise[[level]]
    if (!isTRUE(opt$swap_all)) {
      next
    }

    groups <- as.character(data[[opt$swap_within]])
    treatments <- as.character(data[[opt$swap]])
    keep <- !is.na(groups) & !is.na(treatments)
    by_group <- split(treatments[keep], groups[keep])

    # The generator only swaps in groups holding two or more treatments, so a
    # single-treatment group can never produce an unequal exchange.
    unequal <- vapply(
      by_group,
      function(x) {
        counts <- table(x)
        length(counts) > 1 && length(unique(as.integer(counts))) > 1
      },
      logical(1)
    )

    if (!any(unequal)) {
      next
    }

    bad <- names(by_group)[unequal]
    counts <- table(by_group[[bad[1]]])
    stop(
      "`swap_all = TRUE` requires equal replication within each swap group",
      if (length(optimise) > 1) paste0(" (level `", level, "`)") else "",
      ", because a swap exchanges every plot of one treatment with every plot",
      " of another.\n`",
      opt$swap,
      "` is unequally replicated ",
      if (identical(opt$swap_within, dummy_group)) {
        "across the whole design"
      } else {
        paste0("within `", opt$swap_within, "` ", bad[1])
      },
      ": ",
      paste0(names(counts), " (", as.integer(counts), ")", collapse = ", "),
      if (length(bad) > 1) {
        paste0(", and in ", length(bad) - 1, " other group(s)")
      } else {
        ""
      },
      ".\nSwapping these would change the replication of the design. Use",
      " `swap_all = FALSE`, or correct the replication of `",
      opt$swap,
      "`.",
      call. = FALSE
    )
  }
}

#' Verify the `by` element of `grid_factors`
#'
#' @description
#' `grid_factors` is a plain list, so a mistyped `by` would be ignored and every
#' grid silently pooled. Checked before any optimisation happens.
#'
#' @inheritParams speed
#'
#' @rdname verify
#'
#' @keywords internal
.verify_grid_by <- function(data, grid_factors) {
  grid_by <- grid_factors$by
  if (!is.null(grid_by)) {
    if (!is.character(grid_by) || length(grid_by) != 1) {
      data_type_error("grid_factors$by", "a single column name")
    }
    verify_column_exists(grid_by, data, "grid grouping column")
  }

  return(invisible(NULL))
}


# Other functions for verifying

default_tolerance <- .Machine$double.eps^0.5

is_between_ <- function(
  lower,
  upper,
  lower_exclude = FALSE,
  upper_exclude = FALSE
) {
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
  ...,
  lower = -Inf,
  upper = Inf,
  lower_exclude = FALSE,
  upper_exclude = FALSE,
  var_names = NULL
) {
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

  verify_data_type(
    is_between_(lower, upper, lower_exclude, upper_exclude),
    object_type,
    var_names,
    ...
  )
}

verify_boolean <- function(..., var_names = NULL) {
  verify_data_type(is_boolean, "a boolean", var_names, ...)
}

verify_column_exists <- function(col, data, suffix = NULL) {
  if (!(col %in% names(data))) {
    msg <- c(
      paste0(
        "'",
        col,
        "' not found in ",
        paste(colnames(data), collapse = ", "),
        ". "
      ),
      suffix
    )
    stop(msg, call. = FALSE)
  }
}

verify_non_negative_whole <- function(..., var_names = NULL) {
  verify_data_type(
    is_non_negative_whole_number,
    "a non-negative whole number",
    var_names,
    ...
  )
}

verify_multiple_of <- function(..., var_names = NULL) {
  if (is.null(var_names)) {
    var_names <- get_var_names(...)
  }

  args <- list(...)
  if (!is_multiple_of(args[[1]], args[[2]])) {
    stop(
      paste0(
        "`",
        var_names[[1]],
        "` must be a multiple of `",
        var_names[[2]],
        "`."
      ),
      call. = FALSE
    )
  }
}

verify_positive_whole_number <- function(..., var_names = NULL) {
  verify_data_type(
    is_positive_whole_number,
    "a positive whole number",
    var_names,
    ...
  )
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
  verify_data_type(
    is_positive_whole_numbers,
    "a vector of positive whole numbers",
    var_names,
    ...
  )
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
  stop(
    paste0("`", var_name, "` must be ", expected_data_type, "."),
    call. = FALSE
  )
}

literal <- function(v) {
  if (is.character(v)) {
    return(paste0('`"', v, '"`'))
  }

  return(paste0("`", v, "`"))
}
