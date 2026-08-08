#' Summarise a speed design
#'
#' Produces a richer, statistically meaningful evaluation of a design than
#' [print()][print.design()]. Where `print()` is a compact output,
#' `summary()` decomposes the optimised score and reports structural and
#' evaluation metrics that let you interrogate and defend a design.
#'
#' @param object A `"design"` object returned by [speed()].
#' @param efficiency Logical (default `FALSE`); if `TRUE`, compute the
#'   A-efficiency factor. Returns `NA` with a reason when its assumptions are
#'   not met (columns named `row` and `col`, and at least 3 treatments). See
#'   Details for more information.
#' @param connectedness `NULL` (default) checks whether the design is
#'   connected, but skips the check for very large designs where the model fit
#'   would be expensive; `TRUE` forces it regardless of size; `FALSE` skips it.
#'   See Details for more information.
#' @param concurrence `NULL` (default) computes within-block treatment
#'   concurrence only when an *incomplete* block factor is present; `TRUE`
#'   forces it even for complete blocks, `FALSE` skips it. See Details for more
#'   information.
#' @param neighbour `NULL` (default) or `TRUE` reports neighbour-balance
#'   diagnostics whenever the design has a row/column grid; `FALSE` skips them.
#'   See Details for more information.
#' @param ... Unused; for S3 compatibility.
#'
#' @details
#' The returned object is a list of class `"summary.design"`; it can be assigned
#' and queried programmatically (e.g. `s <- summary(d); s$per_level[[1]]$score`).
#' Printing it is handled by [print.summary.design()].
#'
#' What the evaluation metrics report:
#'
#' - **Connectedness:** whether every pairwise treatment difference is
#'   *estimable* (statistically distinguishable) once row, column and any block
#'   effects are accounted for. A disconnected design is usually not desirable as
#'   it confounds some comparisons with the layout itself, no matter how many
#'   replicates there are. Adjusts for the design's actual input spatial factors
#'   (row, column, and any block or site factor), not just columns literally
#'   named `row`/`col`.
#' - **Concurrence:** how many blocks each pair of treatments shares (`lambda`);
#'   equal lambda across all pairs indicates a balanced incomplete-block
#'   design. Uninformative for complete blocks such as RCBD/split-plot, where
#'   every pair always shares every block.
#' - **Replicate spread:** how many distinct blocks each treatment's replicates
#'   reach, and how many treatments land more than once in a single block. The
#'   block counterpart of replicate span: a block factor is nominal, so there is
#'   no distance to measure along, but the number of blocks a treatment reaches
#'   is still well defined. Reported whenever a block factor is present,
#'   including for complete blocks (where concurrence is skipped) - there it
#'   confirms every treatment reaches every block.
#' - **Replicate span:** how far apart a treatment's own replicates are placed
#'   along the two grid axes (whichever columns `grid_factors` resolved to,
#'   reported by name). For each replicated treatment, the closest its
#'   replicates come to each other, counted inclusively - so a span of 1 means
#'   two replicates share a row, 2 means they are in adjacent rows, and larger
#'   is better. The printed figure is the worst case across all replicated
#'   treatments; the two axes are minimised independently, so they may come from
#'   different treatments. Only the grid axes are measured: a span is a
#'   distance, and a non-grid spatial factor such as block or site has no
#'   ordering to measure along.
#' - **Efficiency:** the A-efficiency factor, a row--column model metric (see
#'   [calculate_efficiency_factor()]); the heaviest of these metrics to
#'   compute, hence opt-in.
#' - **Neighbour balance:** how often treatment pairs end up side by side,
#'   using rook adjacency (left--right and up--down, not diagonal). Reported in
#'   two parts, because they mean opposite things. *Self-adjacency* counts
#'   treatments placed beside another plot of themselves; zero is the desirable
#'   outcome and is what the optimiser works towards. *Pair balance* covers the
#'   distinct treatment pairs: the min, max and variance of their adjacency
#'   counts, and how many never end up adjacent at all. Every possible pair is
#'   counted, so pairs that never neighbour register as zero rather than being
#'   omitted.
#'
#' @returns A list of class `"summary.design"`:
#' - **hierarchical** - `TRUE` for a multi-level (e.g. split-plot) design.
#' - **layout** - `n_plots`, `nrow`, `ncol`, `row_column`, `col_column`,
#'   `has_grid` (`TRUE` when the design is reportable as a single grid), and
#'   `grid_reason` (why not, or `NA`). `nrow`/`ncol` count the rows and columns
#'   the design *occupies*, so a design with a gap in its coordinates (a missing
#'   plot, or a removed buffer) reports fewer than the coordinates span. Both are
#'   `NA` unless `has_grid`.
#' - **levels** - character vector of level names (e.g. `"wp"`/`"sp"`; a single
#'   name for a simple design).
#' - **per_level** - one element per level (named by `levels`), each a list with:
#'   - `swap`, `n_treatments`, `treatments`.
#'   - `replication` - `counts`, `min`, `mean`, `max`, `equal`, `distribution`.
#'   - `spatial_factors` - named vector of the number of levels of each spatial factor.
#'   - `evaluation` - `replicate_span`, `connectedness`, `concurrence`,
#'     `block_spread`, `efficiency`, `neighbour`; each a list with an
#'     `available` flag and either its value(s) or a `reason` it wasn't
#'     computed.
#'   - `score` - `initial`, `final`, `optimal` (`NA` when not applicable), `components`.
#'   - `optim` - `objective`, `start_temp`, `cooling_rate`,
#'     `iterations_requested`, `iterations_run`, `stopped_early`.
#' - **score** - the overall optimised score (summed across levels for a
#'   hierarchical design).
#' - **seed** - the seed used for reproducibility.
#' - **flags** - `hit_iteration_cap`, `unequal_replication`, `disconnected`.
#' - **call** - the captured `speed()` call.
#'
#' @seealso [print.design()], [speed()]
#'
#' @examples
#' df <- data.frame(
#'   row = rep(1:4, times = 3),
#'   col = rep(1:3, each = 4),
#'   treatment = rep(LETTERS[1:3], 4)
#' )
#' design <- speed(df, swap = "treatment", swap_within = "1",
#'                 spatial_factors = ~ row + col, iterations = 100, seed = 1)
#' summary(design)
#'
#' # Opt in to the (heavier) A-efficiency factor
#' summary(design, efficiency = TRUE)
#'
#' @export
summary.design <- function(
  object,
  efficiency = FALSE,
  connectedness = NULL,
  concurrence = NULL,
  neighbour = NULL,
  ...
) {
  meta <- object$metadata
  if (is.null(meta)) {
    stop(
      "This design has no `metadata`; it may predate the summary() method. ",
      "Re-run speed() to produce a summarisable design.",
      call. = FALSE
    )
  }
  # Buffers (from add_buffers()) are a field-layout convenience, not part of
  # the statistical design, so they're excluded before any computation.
  df <- .drop_buffer_rows(object$design_df, meta)
  hierarchical <- is.list(object$treatments)
  rc <- meta$row_column %||% "row"
  cc <- meta$col_column %||% "col"
  levels <- meta$levels %||% names(meta$per_level)

  want_neighbour <- is.null(neighbour) || isTRUE(neighbour)

  # The one coordinate validation for the whole summary: `grid` is either a
  # `grid_index()` list or the reason there isn't one.
  #
  # `has_grid` means "reportable as one grid", not merely "row/col columns
  # exist": duplicate coordinates span several grids of differing shapes.
  grid <- tryCatch(
    grid_indices(df, row_column = rc, col_column = cc, by = meta$grid_by),
    speed_grid_error = function(e) return(e$reason)
  )
  # A multi-environment trial occupies several grids that share a treatment set
  # and never share an edge, so no single `nrow` x `ncol` describes it.
  n_grids <- if (is.character(grid)) NA_integer_ else length(grid)
  has_grid <- !is.character(grid) && n_grids == 1L
  layout <- list(
    n_plots = nrow(df),
    nrow = if (has_grid) length(unique(df[[rc]])) else NA_integer_,
    ncol = if (has_grid) length(unique(df[[cc]])) else NA_integer_,
    row_column = rc,
    col_column = cc,
    has_grid = has_grid,
    n_grids = n_grids,
    grid_by = meta$grid_by,
    grid_reason = if (has_grid) {
      NA_character_
    } else if (is.character(grid)) {
      grid
    } else {
      sprintf("%d grids, grouped by `%s`", n_grids, meta$grid_by)
    }
  )

  per_level <- lapply(levels, function(lv) {
    pm <- meta$per_level[[lv]]
    swap <- pm$swap
    trts <- if (hierarchical) object$treatments[[lv]] else object$treatments
    sf <- pm$spatial_cols
    block <- .design_block_factor(df, sf, rc, cc)

    # Plot counts per treatment. For a simple design this is the replication;
    # for a nested level it is plots-per-treatment (replication x sub-units).
    counts <- as.integer(table(df[[swap]]))
    replication <- list(
      counts = counts,
      min = min(counts),
      mean = mean(counts),
      max = max(counts),
      equal = length(unique(counts)) == 1,
      distribution = table(counts)
    )

    sf_levels <- vapply(
      sf,
      function(s) return(length(unique(df[[s]]))),
      integer(1)
    )
    names(sf_levels) <- sf

    # Score components captured during the run (see speed_hierarchical); NULL
    # for custom objectives that don't return one.
    trace <- if (hierarchical) object$scores[[lv]] else object$scores
    initial <- if (length(trace)) trace[[1]] else NA_real_
    final <- pm$final_score %||% NA_real_

    run <- if (hierarchical) {
      length(object$scores[[lv]])
    } else {
      object$iterations_run
    }
    stopped <- if (hierarchical) {
      isTRUE(object$stopped_early[[lv]])
    } else {
      isTRUE(object$stopped_early)
    }

    # --- Evaluation metrics ---
    evaluation <- list(
      replicate_span = .replicate_spans(df, swap, rc, cc, grid),
      connectedness = if (isFALSE(connectedness)) {
        list(
          available = FALSE,
          reason = "not requested (connectedness = FALSE)"
        )
      } else {
        # NULL (auto) skips very large designs; explicit TRUE forces the fit.
        .design_connectedness(
          df,
          swap,
          block,
          sf,
          force = isTRUE(connectedness)
        )
      },
      concurrence = if (is.null(block)) {
        list(available = FALSE, reason = "no block factor")
      } else if (isFALSE(concurrence)) {
        list(available = FALSE, reason = "not requested (concurrence = FALSE)")
      } else {
        # NULL (auto) skips complete blocks; explicit TRUE forces them too.
        .design_concurrence(df, swap, block, force = isTRUE(concurrence))
      },
      # Not gated on `concurrence`: it answers a different question, and stays
      # informative for complete blocks where concurrence is skipped.
      block_spread = if (is.null(block)) {
        list(available = FALSE, reason = "no block factor")
      } else {
        .block_spread(df, swap, block)
      },
      efficiency = if (isTRUE(efficiency)) {
        .efficiency_factor(df, swap, rc, cc, grid)
      } else {
        list(
          available = FALSE,
          reason = "not requested (set efficiency = TRUE)"
        )
      },
      # A design that cannot be gridded reports `grid`'s reason rather than
      # erroring.
      neighbour = if (!want_neighbour) {
        list(available = FALSE, reason = "not requested (neighbour = FALSE)")
      } else {
        .neighbour_balance(df, swap, rc, cc, grid)
      }
    )

    return(list(
      swap = swap,
      n_treatments = length(trts),
      treatments = trts,
      replication = replication,
      spatial_factors = sf_levels,
      evaluation = evaluation,
      score = list(
        initial = initial,
        final = final,
        optimal = pm$optimal_score %||% NA_real_,
        components = pm$final_components
      ),
      optim = list(
        objective = .objective_name(pm$obj_function),
        start_temp = pm$start_temp,
        cooling_rate = pm$cooling_rate,
        iterations_requested = pm$iterations,
        iterations_run = run,
        stopped_early = stopped
      )
    ))
  })
  names(per_level) <- levels

  # Flags.
  hit_cap <- vapply(
    per_level,
    function(p) return(!p$optim$stopped_early),
    logical(1)
  )
  unequal <- vapply(
    per_level,
    function(p) return(!p$replication$equal),
    logical(1)
  )
  disconnected <- vapply(
    per_level,
    function(p) {
      cn <- p$evaluation$connectedness
      return(isTRUE(cn$available) && isTRUE(!cn$connected))
    },
    logical(1)
  )
  flags <- list(
    hit_iteration_cap = names(hit_cap)[hit_cap],
    unequal_replication = any(unequal),
    disconnected = names(disconnected)[disconnected]
  )

  return(structure(
    list(
      hierarchical = hierarchical,
      layout = layout,
      levels = levels,
      per_level = per_level,
      score = object$score,
      seed = object$seed,
      flags = flags,
      call = meta$call
    ),
    class = "summary.design"
  ))
}

#' Identify a known objective function by name
#'
#' Compares `fn` by identity against the package's exported objective functions
#' so the summary can report a readable name (and detect the neighbour-balance
#' objective). Returns `"custom"` for anything unrecognised.
#'
#' @param fn A function.
#' @return A length-one character string.
#' @keywords internal
.objective_name <- function(fn) {
  if (!is.function(fn)) {
    return("unknown")
  }
  known <- list(
    objective_function = objective_function,
    objective_function_factorial = objective_function_factorial,
    objective_function_piepho = objective_function_piepho
  )
  for (nm in names(known)) {
    if (identical(fn, known[[nm]])) return(nm)
  }
  return("custom")
}

#' Print method for design summaries
#'
#' @param x A `"summary.design"` object from [summary.design()].
#' @param ... Unused; for S3 compatibility.
#'
#' @return `x` invisibly.
#'
#' @export
print.summary.design <- function(x, ...) {
  pad <- 14
  lab <- function(s) return(formatC(s, width = -pad))
  indent <- strrep(" ", pad)
  fmt_int <- function(n) {
    return(format(n, big.mark = ",", scientific = FALSE, trim = TRUE))
  }
  fmt_num <- function(n) {
    return(format(round(n, 4), big.mark = ",", trim = TRUE))
  }
  section <- function(title) {
    return(cat(
      "\n",
      crayon::bold(title),
      "\n",
      strrep("-", nchar(title)),
      "\n",
      sep = ""
    ))
  }

  cat(crayon::bold("Design Summary"), "\n", sep = "")
  cat("==============\n")

  # --- Flags (only when something is worth flagging) ---
  flag_lines <- character()
  if (length(x$flags$hit_iteration_cap)) {
    where <- if (x$hierarchical) {
      paste0(" (", paste(x$flags$hit_iteration_cap, collapse = ", "), ")")
    } else {
      ""
    }
    flag_lines <- c(
      flag_lines,
      paste0("! Ran to iteration cap - may not have converged", where)
    )
  }
  if (length(x$flags$disconnected)) {
    where <- if (x$hierarchical) {
      paste0(" (", paste(x$flags$disconnected, collapse = ", "), ")")
    } else {
      ""
    }
    flag_lines <- c(flag_lines, paste0("! DISCONNECTED design", where))
  }
  if (isTRUE(x$flags$unequal_replication)) {
    flag_lines <- c(flag_lines, "! Unequal replication")
  }
  if (length(flag_lines)) {
    section("Flags")
    # Every flag raised here is a warning, so all are magenta; a positive flag
    # would be green.
    for (fl in flag_lines) {
      cat(crayon::magenta(fl), "\n", sep = "")
    }
  }

  # --- Structure ---
  section("Structure")
  lo <- x$layout
  if (isTRUE(lo$has_grid)) {
    cat(
      lab("Layout:"),
      sprintf(
        "%d rows x %d cols (%s plots)",
        lo$nrow,
        lo$ncol,
        fmt_int(lo$n_plots)
      ),
      "\n",
      sep = ""
    )
  } else {
    cat(
      lab("Layout:"),
      sprintf("%s plots", fmt_int(lo$n_plots)),
      "\n",
      sep = ""
    )
  }
  for (lv in x$levels) {
    .print_level_structure(x, lv, lab, indent, fmt_num)
  }

  # --- Optimisation ---
  section("Optimisation")
  cat(lab("Seed:"), x$seed, "\n", sep = "")
  # Only meaningful to show the total separately when there's more than one level.
  if (x$hierarchical) {
    cat(lab("Total score:"), fmt_num(x$score), "\n", sep = "")
  }
  for (lv in x$levels) {
    .print_level_optim(x, lv, lab, indent, fmt_int, fmt_num)
  }

  # --- Evaluation ---
  section("Evaluation")
  for (lv in x$levels) {
    .print_level_evaluation(x, lv, lab, fmt_num)
  }

  return(invisible(x))
}

#' Print one level's evaluation block (connectedness, concurrence, spans, ...)
#'
#' @keywords internal
.print_level_evaluation <- function(x, lv, lab, fmt_num) {
  e <- x$per_level[[lv]]$evaluation
  if (x$hierarchical) {
    cat("\n[", lv, "]\n", sep = "")
  }

  # Connectedness
  cn <- e$connectedness
  if (isTRUE(cn$available)) {
    state <- if (isTRUE(cn$connected)) "connected" else "DISCONNECTED"
    cat(
      lab("Connected:"),
      sprintf("%s - %s [%s]", state, cn$message, cn$method),
      "\n",
      sep = ""
    )
  } else {
    cat(lab("Connected:"), cn$reason, "\n", sep = "")
  }

  # Concurrence
  cc <- e$concurrence
  if (isTRUE(cc$available)) {
    constant <- if (cc$lambda_constant) " (constant)" else ""
    zero <- if (cc$n_zero_pairs > 0) {
      sprintf(", %d zero-concurrence pair(s)", cc$n_zero_pairs)
    } else {
      ""
    }
    cat(
      lab("Concurrence:"),
      sprintf(
        "min %d, max %d%s%s [block: %s]",
        cc$lambda_min,
        cc$lambda_max,
        constant,
        zero,
        cc$block
      ),
      "\n",
      sep = ""
    )
  } else {
    cat(lab("Concurrence:"), cc$reason, "\n", sep = "")
  }

  # Replicate spread across blocks
  bs <- e$block_spread
  if (isTRUE(bs$available)) {
    spread <- if (bs$min_blocks == bs$max_blocks) {
      sprintf("each treatment in %d of %d blocks", bs$min_blocks, bs$n_blocks)
    } else {
      sprintf(
        "treatments in %d-%d of %d blocks",
        bs$min_blocks,
        bs$max_blocks,
        bs$n_blocks
      )
    }
    dup <- if (bs$n_within_block_reps > 0) {
      sprintf(", %d replicated within a block", bs$n_within_block_reps)
    } else {
      ""
    }
    cat(
      lab("Blk. spread:"),
      sprintf("%s%s [block: %s]", spread, dup, bs$block),
      "\n",
      sep = ""
    )
  } else {
    cat(lab("Blk. spread:"), bs$reason, "\n", sep = "")
  }

  # Replicate span
  rs <- e$replicate_span
  if (isTRUE(rs$available) && rs$n_replicated > 0) {
    cat(
      lab("Repl. span:"),
      sprintf(
        "worst-case %s (%s), %s (%s) across %d replicated treatment(s)",
        fmt_num(rs$min_row_span),
        x$layout$row_column,
        fmt_num(rs$min_col_span),
        x$layout$col_column,
        rs$n_replicated
      ),
      "\n",
      sep = ""
    )
  } else if (isTRUE(rs$available)) {
    cat(lab("Repl. span:"), "n/a (no replicated treatments)\n", sep = "")
  } else {
    cat(lab("Repl. span:"), rs$reason, "\n", sep = "")
  }

  # Efficiency. A multi-grid design reports one value per grid and no total:
  # there is no meaningful way to combine them (see `.efficiency_factor()`).
  ef <- e$efficiency
  if (!is.null(ef$per_grid)) {
    cat(
      lab("Efficiency:"),
      sprintf(
        "per %s (A-efficiency, row-column model)\n",
        if (is.null(ef$grid_by)) "grid" else paste0("`", ef$grid_by, "`")
      ),
      sep = ""
    )
    for (nm in names(ef$per_grid)) {
      one <- ef$per_grid[[nm]]
      cat(
        "    ",
        format(nm, width = max(nchar(names(ef$per_grid)))),
        "  ",
        if (isTRUE(one$available)) fmt_num(one$value) else one$reason,
        "\n",
        sep = ""
      )
    }
  } else if (isTRUE(ef$available)) {
    cat(
      lab("Efficiency:"),
      fmt_num(ef$value),
      " (A-efficiency, row-column model)\n",
      sep = ""
    )
  } else {
    cat(lab("Efficiency:"), ef$reason, "\n", sep = "")
  }

  # Neighbour balance. Self-adjacency gets its own row: zero is the desirable
  # outcome, so a non-zero count is highlighted the way a warning flag is.
  nb <- e$neighbour
  if (isTRUE(nb$available)) {
    self <- if (nb$self_adjacent > 0) {
      crayon::magenta(sprintf(
        "%d like-treatment adjacencies",
        nb$self_adjacent
      ))
    } else {
      "none"
    }
    cat(lab("Self-adj.:"), self, "\n", sep = "")

    zero <- if (nb$n_zero_pairs > 0) {
      sprintf(", %d never adjacent", nb$n_zero_pairs)
    } else {
      ""
    }
    cat(
      lab("Neighbour:"),
      sprintf(
        "min %d, max %d over %d pairs (variance %s)%s",
        nb$min_pair_count,
        nb$max_pair_count,
        nb$n_pairs,
        fmt_num(nb$pair_var),
        zero
      ),
      "\n",
      sep = ""
    )
  } else {
    cat(lab("Neighbour:"), nb$reason, "\n", sep = "")
  }
  return(invisible(NULL))
}

#' Print one level's structure block (treatments, replication, spatial factors)
#'
#' @keywords internal
.print_level_structure <- function(x, lv, lab, indent, fmt_num) {
  p <- x$per_level[[lv]]
  rep <- p$replication
  if (x$hierarchical) {
    cat("\n[", lv, "]\n", sep = "")
  }

  rep_label <- if (x$hierarchical) "Plots/trt:" else "Replication:"
  if (rep$equal) {
    rep_str <- sprintf("%d each", rep$min)
  } else {
    rep_str <- sprintf(
      "min/mean/max %d / %s / %d",
      rep$min,
      fmt_num(rep$mean),
      rep$max
    )
  }

  cat(lab("Treatments:"), p$n_treatments, "\n", sep = "")
  cat(lab(rep_label), rep_str, "\n", sep = "")
  sf <- p$spatial_factors
  if (length(sf)) {
    cat(
      lab("Spatial:"),
      paste(sprintf("%s (%d)", names(sf), sf), collapse = ", "),
      "\n",
      sep = ""
    )
  }
  return(invisible(NULL))
}

#' Drop buffer plots from a design data frame
#'
#' `add_buffers()` appends rows with the treatment column(s) set to `"buffer"`.
#' Buffers are a practical field-layout convenience, not part of the statistical
#' design, so `print()` and `summary()` exclude them from every computation.
#' Removes the buffer rows and the now-unused `"buffer"` factor level. A no-op
#' when there is no metadata or no buffers.
#'
#' @param df A design data frame.
#' @param meta The design's `metadata` (for the per-level swap columns).
#' @return `df` with any buffer rows removed.
#' @keywords internal
.drop_buffer_rows <- function(df, meta) {
  if (is.null(meta$per_level)) {
    return(df)
  }
  swap_cols <- unique(vapply(
    meta$per_level,
    function(p) return(p$swap),
    character(1)
  ))
  buffer_rows <- logical(nrow(df))
  for (s in swap_cols) {
    buffer_rows <- buffer_rows | as.character(df[[s]]) == "buffer"
  }
  if (!any(buffer_rows)) {
    return(df)
  }
  df <- df[!buffer_rows, , drop = FALSE]
  for (s in swap_cols) {
    if (is.factor(df[[s]])) df[[s]] <- droplevels(df[[s]])
  }
  return(df)
}

#' Replicate spatial spans
#'
#' For each treatment, the minimum Manhattan separation between its replicate
#' plots along rows and along columns (`+ 1` so the span counts plots
#' inclusively), plus the worst-case (minimum) span across replicated
#' treatments. Adapted from a colleague's `sommario.duplicates.span.doe`. A
#' small worst-case span flags replicates that sit close together.
#'
#' Measured only along the two grid axes (`rc`/`cc`, resolved from
#' `grid_factors` by [infer_row_col()]), not the design's other spatial
#' factors. A span is a distance, and only the grid axes are ordered: the
#' separation between block 1 and block 3 is not 2. For non-grid factors the
#' equivalent question ("do a treatment's replicates land in the same block?")
#' is a count, and is answered by the concurrence matrix diagonal instead.
#'
#' @param df Design data frame.
#' @param swap Treatment column name.
#' @param rc,cc Row and column column names.
#' @param grid A [grid_index()] list, or a character reason there is no grid.
#' @keywords internal
.replicate_spans <- function(df, swap, rc, cc, grid) {
  # Spans are distances within one grid; across grids they are meaningless
  # (two sites' row 3 are not one plot apart), so refuse rather than pool.
  if (is.character(grid)) {
    return(list(available = FALSE, reason = grid))
  }
  if (length(grid) > 1L) {
    return(list(
      available = FALSE,
      reason = sprintf(
        "design spans %d grids (grouped by `%s`)",
        length(grid),
        attr(grid, "by")
      )
    ))
  }
  span1 <- function(x) {
    if (length(x) < 2) {
      return(NA_real_)
    }
    return(min(stats::dist(x, "manhattan")) + 1)
  }
  rows <- as_numeric_factor(df[[rc]])
  cols <- as_numeric_factor(df[[cc]])
  trt <- df[[swap]]
  row_spans <- tapply(rows, trt, span1)
  col_spans <- tapply(cols, trt, span1)
  has_reps <- !is.na(row_spans)
  return(list(
    available = TRUE,
    row_spans = row_spans,
    col_spans = col_spans,
    min_row_span = if (any(has_reps)) {
      min(row_spans, na.rm = TRUE)
    } else {
      NA_real_
    },
    min_col_span = if (any(has_reps)) {
      min(col_spans, na.rm = TRUE)
    } else {
      NA_real_
    },
    n_replicated = sum(has_reps)
  ))
}

#' Detect a block-type factor for one level of a design
#'
#' Among *that level's* spatial factors that are not the row or column factor,
#' prefer one named like `block`; otherwise take the first such factor.
#' Failing that, fall back to a column literally named `block`. The chosen factor
#' is surfaced in the concurrence output (`[block: ...]`), so the choice is
#' visible to the user.
#'
#' Resolved per level (rather than once from the union of every level's spatial
#' factors) so that a hierarchical design whose levels are blocked by different
#' factors doesn't have one level's block column applied to another.
#'
#' @param spatial_cols Character vector of this level's spatial factor columns.
#' @keywords internal
.design_block_factor <- function(df, spatial_cols, rc, cc) {
  cand <- setdiff(spatial_cols, c(rc, cc))
  cand <- cand[cand %in% names(df)]
  block_like <- cand[grepl("block", cand, ignore.case = TRUE)]
  if (length(block_like)) {
    return(block_like[[1]])
  }
  if (length(cand)) {
    return(cand[[1]])
  }
  if ("block" %in% names(df)) {
    return("block")
  }
  return(NULL)
}

#' Design connectedness (base R, no lme4)
#'
#' A design is connected if every treatment contrast is estimable after
#' adjusting for the factors the design is stratified by - its spatial factors
#' (row, col, ...) **and** any block factor. We fit `lm(dummy ~ <nuisance> +
#' treatment)` with `treatment` last, so that any confounding aliases the
#' treatment coefficients (which we count) rather than the nuisance ones. Zero
#' aliased treatment coefficients implies treatment is fully estimable.
#'
#' The response is a dummy: estimability is a rank property of the design matrix,
#' independent of the data. Counting aliasing only among treatment terms avoids
#' false positives when nuisance factors are themselves collinear (e.g. a block
#' factor that coincides with rows in a resolvable design).
#'
#' @param spatial_cols Character vector of the level's spatial factor columns.
#' @param force Fit the model even for very large designs (where it is skipped
#'   by default because the dense `lm` fit is expensive).
#' @keywords internal
.design_connectedness <- function(
  df,
  swap,
  block,
  spatial_cols,
  force = FALSE
) {
  n_trt <- length(unique(df[[swap]]))
  if (n_trt < 2) {
    return(list(available = FALSE, reason = "needs >= 2 treatments"))
  }
  nuisance <- setdiff(
    intersect(unique(c(spatial_cols, block)), names(df)),
    swap
  )
  if (length(nuisance) == 0) {
    return(list(
      available = TRUE,
      method = "none",
      connected = TRUE,
      n_aliased = 0L,
      message = "no blocking structure (trivially connected)"
    ))
  }
  # Guard against expensive fits: the dense model matrix has ~p columns and the
  # QR is O(n * p^2). Skip (unless forced) when that is large enough to be slow.
  p <- 1 +
    sum(vapply(
      nuisance,
      function(f) return(length(unique(df[[f]])) - 1L),
      integer(1)
    )) +
    (n_trt - 1)
  if (!force && as.double(nrow(df)) * p^2 > 1e9) {
    return(list(
      available = FALSE,
      reason = "large design - set connectedness = TRUE to compute"
    ))
  }
  d <- df[, c(nuisance, swap)]
  d[] <- lapply(d, factor)
  d[["..y.."]] <- 0 # dummy response; estimability is a rank property
  fit <- stats::lm(stats::reformulate(c(nuisance, swap), "..y.."), data = d)
  co <- stats::coef(fit)
  n_aliased <- sum(is.na(co) & startsWith(names(co), swap))
  model <- paste(nuisance, collapse = " + ")
  return(list(
    available = TRUE,
    method = sprintf("model (%s)", model),
    connected = n_aliased == 0,
    n_aliased = n_aliased,
    message = if (n_aliased == 0) {
      sprintf("treatment estimable given %s", model)
    } else {
      sprintf(
        "%d treatment contrast(s) not estimable given %s",
        n_aliased,
        model
      )
    }
  ))
}

#' Treatment concurrences within blocks
#'
#' From the treatment-by-block incidence `M`, the concurrence matrix is
#' `C = M M'`: off-diagonals are pairwise concurrences (how often two treatments
#' share a block), the diagonal is replication.
#'
#' Concurrences only carry information for *incomplete* blocks (block size <
#' number of treatments). For complete blocks (RCBD, split-plot, ...) every pair
#' co-occurs in every block, so every concurrence equals the replication - it
#' merely restates the design and is skipped unless `force = TRUE`.
#'
#' @param force Compute even when blocks are complete.
#' @keywords internal
.design_concurrence <- function(df, swap, block, force = FALSE) {
  if (is.null(block)) {
    return(list(available = FALSE, reason = "no block factor"))
  }
  M <- table(df[[swap]], df[[block]])
  complete <- !any(M == 0) # every treatment present in every block
  if (complete && !force) {
    return(list(
      available = FALSE,
      complete = TRUE,
      reason = "complete blocks - not informative"
    ))
  }
  C <- M %*% t(M)
  lambda <- C[upper.tri(C)]
  return(list(
    available = TRUE,
    block = block,
    complete = complete,
    lambda_min = min(lambda),
    lambda_max = max(lambda),
    lambda_constant = length(unique(lambda)) == 1,
    n_zero_pairs = sum(lambda == 0)
  ))
}

#' Replicate spread across blocks
#'
#' How many distinct blocks each treatment's replicates are spread over, and how
#' many treatments land more than once in a single block. This is the block
#' equivalent of `.replicate_spans()`: a block factor is nominal, so there is no
#' distance to measure along, but "how many different blocks do a treatment's
#' replicates reach" is still well defined. It reads the same treatment-by-block
#' incidence `M` as [.design_concurrence()] - the per-treatment figures come
#' from `M`'s rows, where concurrence uses the off-diagonals of `M M'`.
#'
#' Computed whenever a block factor is present, independent of the `concurrence`
#' argument: unlike concurrence it remains informative for complete blocks,
#' where it confirms every treatment reaches every block.
#'
#' @param block Block factor column name.
#' @keywords internal
.block_spread <- function(df, swap, block) {
  M <- table(df[[swap]], df[[block]])
  occupied <- rowSums(M > 0) # distinct blocks reached by each treatment
  return(list(
    available = TRUE,
    block = block,
    n_blocks = ncol(M),
    min_blocks = min(occupied),
    max_blocks = max(occupied),
    n_within_block_reps = sum(apply(M, 1, max) > 1)
  ))
}

#' A-efficiency factor (opt-in wrapper)
#'
#' Thin guarded wrapper over [calculate_efficiency_factor()] (a row--column model
#' metric), using the design's resolved row/column columns. Returns `NA` with a
#' reason rather than erroring when its assumptions are not met.
#'
#' @param rc,cc Row and column column names.
#' @param grid A [grid_index()] list, or a character reason there is no grid.
#' @keywords internal
.efficiency_factor <- function(df, swap, rc, cc, grid) {
  # Not just a guard against erroring: on duplicate coordinates
  # calculate_efficiency_factor() pools the grids and returns an impossible >1.
  if (is.character(grid)) {
    return(list(available = FALSE, reason = grid))
  }
  if (length(unique(df[[swap]])) < 3) {
    return(list(available = FALSE, reason = "requires >= 3 treatments"))
  }

  one <- function(sub) {
    ef <- tryCatch(
      eval(bquote(calculate_efficiency_factor(
        sub,
        .(as.name(swap)),
        row_column = rc,
        col_column = cc
      ))),
      # A rank failure is a property of the design, so it carries its own
      # reason; anything else is unexpected and gets the generic one.
      speed_efficiency_error = function(e) return(e$reason),
      error = function(e) return(NULL)
    )
    if (is.character(ef)) {
      return(list(available = FALSE, reason = ef))
    }
    if (is.null(ef) || !is.finite(ef)) {
      return(list(
        available = FALSE,
        reason = "could not be computed for this design"
      ))
    }
    return(list(available = TRUE, value = ef))
  }

  if (length(grid) == 1L) {
    return(one(df))
  }

  # One value per grid, never summed or averaged: averaging gives a different
  # quantity from the combined analysis, which is not identified at design time
  # anyway. Each grid is gated on its own rank, so one unreplicated site reports
  # its reason without withholding the others.
  per_grid <- lapply(grid, function(g) return(one(df[g$rows, , drop = FALSE])))
  return(list(
    available = any(vapply(per_grid, function(x) x$available, logical(1))),
    per_grid = per_grid,
    grid_by = attr(grid, "by")
  ))
}

#' Neighbour-balance diagnostics
#'
#' Builds the treatment grid and counts how often each treatment pair ends up
#' adjacent (rook adjacency only), via
#' [create_pair_mapping()]/[calculate_nb()]. Pairs never observed as neighbours
#' are filled in as zero - `calculate_nb()`'s own table omits them rather than
#' recording a zero.
#'
#' Self-pairs (a treatment beside another plot of itself) are reported
#' separately from distinct treatment pairs, because they mean opposite things:
#' zero self-adjacency is the desirable outcome the optimiser works towards,
#' whereas a distinct pair that never neighbours is an imbalance. Lumping them
#' together hides self-adjacency behind the same `min 0` as the harmless case.
#'
#' The grid comes from [build_design_matrix()], which places each plot at its own
#' `rc`/`cc` coordinates, so the counts describe the layout whatever order `df`
#' is in. Coordinates are read as-is, so plots separated by a buffer row or
#' column (`add_buffers()` offsets and scales them) keep that separation and are
#' not counted as neighbours.
#'
#' A design that cannot be placed on one grid is reported as unavailable rather
#' than propagating [grid_index()]'s error out of `summary()`.
#'
#' @param rc,cc Row and column column names.
#' @param grid A [grid_index()] list, reused as the [build_design_matrix()]
#'   index, or a character reason there is no grid.
#' @keywords internal
.neighbour_balance <- function(df, swap, rc, cc, grid) {
  if (is.character(grid)) {
    return(list(available = FALSE, reason = grid))
  }
  # One pair mapping for the whole design, so a pair absent from one site counts
  # as zero rather than being dropped. Counts sum across grids: no edge crosses
  # a grid boundary.
  pair_mapping <- create_pair_mapping(df[[swap]])
  all_pairs <- unique(pair_mapping)
  counts <- setNames(rep(0L, length(all_pairs)), all_pairs)
  for (g in grid) {
    dm <- build_design_matrix(
      df[g$rows, , drop = FALSE],
      swap,
      row_column = rc,
      col_column = cc,
      index = g$index
    )
    nb <- calculate_nb(dm, pair_mapping)
    counts[names(nb$nb)] <- counts[names(nb$nb)] + nb$nb
  }

  # create_pair_mapping() keys are "trt1,trt2"; a self-pair repeats the level.
  parts <- strsplit(names(counts), ",", fixed = TRUE)
  is_self <- vapply(parts, function(p) return(p[[1]] == p[[2]]), logical(1))
  pairs <- counts[!is_self]

  return(list(
    available = TRUE,
    self_adjacent = sum(counts[is_self]),
    n_pairs = length(pairs),
    min_pair_count = min(pairs),
    max_pair_count = max(pairs),
    pair_var = stats::var(pairs),
    n_zero_pairs = sum(pairs == 0)
  ))
}

#' Print one level's optimisation block (objective, score components, schedule)
#'
#' @keywords internal
.print_level_optim <- function(x, lv, lab, indent, fmt_int, fmt_num) {
  p <- x$per_level[[lv]]
  s <- p$score
  o <- p$optim
  if (x$hierarchical) {
    cat("\n[", lv, "]\n", sep = "")
  }

  cat(lab("Objective:"), o$objective, "\n", sep = "")
  cat(
    lab("Score:"),
    sprintf(
      "%s  (initial %s -> final %s)",
      fmt_num(s$final),
      fmt_num(s$initial),
      fmt_num(s$final)
    ),
    "\n",
    sep = ""
  )
  # Additive decomposition of the score, when the objective exposes one (custom
  # objectives may omit it).
  comp <- s$components
  if (!is.null(comp) && length(comp)) {
    w <- max(nchar(names(comp)))
    for (nm in names(comp)) {
      cat(
        indent,
        formatC(nm, width = -(w + 2)),
        fmt_num(comp[[nm]]),
        "\n",
        sep = ""
      )
    }
  }

  if (!is.na(s$optimal %||% NA_real_)) {
    reached <- if (s$final <= s$optimal + 1e-9) {
      crayon::green("(reached)")
    } else {
      "(not reached)"
    }
    cat(lab("Optimal:"), fmt_num(s$optimal), "  ", reached, "\n", sep = "")
  }

  # Whether the run converged (stopped early) or hit the iteration cap is the
  # single most useful signal here, so it's colour-highlighted either way.
  stop_reason <- if (o$stopped_early) {
    crayon::green("(stopped early)")
  } else {
    crayon::bold(crayon::magenta("(ran to cap)"))
  }
  iter <- paste(
    sprintf(
      "%s / %s",
      fmt_int(o$iterations_run),
      fmt_int(o$iterations_requested)
    ),
    stop_reason
  )
  cat(lab("Iterations:"), iter, "\n", sep = "")
  cat(
    lab("Temperature:"),
    sprintf(
      "start %s, cooling %s",
      fmt_num(o$start_temp),
      fmt_num(o$cooling_rate)
    ),
    "\n",
    sep = ""
  )
  return(invisible(NULL))
}
