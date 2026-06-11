#' Summarise a speed design
#'
#' Produces a richer, statistically meaningful evaluation of a design than
#' [print()][print.design()]. Where `print()` is a compact output,
#' `summary()` decomposes the optimised score and reports structural and
#' evaluation metrics that let you interrogate and defend a design.
#'
#' The returned object is a list of class `"summary.design"`; it can be assigned
#' and queried programmatically (e.g. `s <- summary(d); s$per_level[[1]]$score`).
#' Printing it is handled by [print.summary.design()].
#'
#' @param object A `"design"` object returned by [speed()].
#' @param efficiency Logical (default `FALSE`); if `TRUE`, compute the
#'   A-efficiency factor (a row--column model metric and the heaviest to
#'   compute). Returns `NA` with a reason when its assumptions are not met
#'   (columns named `row` and `col`, and at least 3 treatments).
#' @param connectedness `NULL` (default) checks whether treatments are estimable
#'   (connected), but skips the check for very large designs where the model fit
#'   would be expensive; `TRUE` forces it regardless of size; `FALSE` skips it.
#'   The check fits `lm(~ <spatial factors + block> + treatment)` and looks for
#'   aliased treatment contrasts.
#' @param concurrence `NULL` (default) computes within-block treatment
#'   concurrences only when an *incomplete* block factor is present (they are
#'   uninformative for complete blocks such as RCBD/split-plot); `TRUE` forces
#'   them even for complete blocks, `FALSE` skips them.
#' @param neighbour `NULL` (default) reports neighbour-balance diagnostics when
#'   the design was optimised with `objective_function_piepho`; override with
#'   `TRUE`/`FALSE`.
#' @param ... Unused; for S3 compatibility.
#'
#' @returns A list of class `"summary.design"` with elements including
#'   `hierarchical`, `layout`, `levels`, `per_level` (structure + optimisation
#'   per level), `score`, `seed`, `flags` and the recorded `call`.
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
summary.design <- function(object,
                           efficiency    = FALSE,
                           connectedness = NULL,
                           concurrence   = NULL,
                           neighbour     = NULL,
                           ...) {
  meta <- object$metadata
  if (is.null(meta)) {
    stop("This design has no `metadata`; it may predate the summary() method. ",
         "Re-run speed() to produce a summarisable design.", call. = FALSE)
  }
  # Exclude buffer plots (add_buffers() appends rows with the treatment columns
  # set to "buffer"). Buffers are a practical convenience for laying the design
  # out in the field, not part of the statistical design, so they take no part
  # in the summary and are not reported. The stored score components are
  # unaffected: they were captured during optimisation, before any buffers.
  df <- .drop_buffer_rows(object$design_df, meta)
  hierarchical <- is.list(object$treatments)
  rc <- meta$row_column %||% "row"
  cc <- meta$col_column %||% "col"
  levels <- meta$levels %||% names(meta$per_level)

  # Resolve evaluation toggles. concurrence auto-on only when an *incomplete*
  # block factor exists (it is uninformative for complete blocks); neighbour
  # auto-on when any level used the neighbour-balance objective.
  block <- .design_block_factor(df, meta, rc, cc)
  want_neighbour <- if (is.null(neighbour)) {
    any(vapply(meta$per_level,
               function(p) identical(p$obj_function, objective_function_piepho),
               logical(1)))
  } else {
    isTRUE(neighbour)
  }

  has_grid <- all(c(rc, cc) %in% names(df))
  layout <- list(
    n_plots    = nrow(df),
    nrow       = if (has_grid) length(unique(df[[rc]])) else NA_integer_,
    ncol       = if (has_grid) length(unique(df[[cc]])) else NA_integer_,
    row_column = rc,
    col_column = cc,
    has_grid   = has_grid
  )

  per_level <- lapply(levels, function(lv) {
    pm <- meta$per_level[[lv]]
    swap <- pm$swap
    trts <- if (hierarchical) object$treatments[[lv]] else object$treatments
    sf <- pm$spatial_cols

    # Plot counts per treatment. For a simple design this is the replication;
    # for a nested level it is plots-per-treatment (replication x sub-units).
    counts <- as.integer(table(df[[swap]]))
    replication <- list(
      counts       = counts,
      min          = min(counts),
      mean         = mean(counts),
      max          = max(counts),
      equal        = length(unique(counts)) == 1,
      distribution = table(counts)
    )

    sf_levels <- vapply(sf, function(s) length(unique(df[[s]])), integer(1))
    names(sf_levels) <- sf

    # Score components captured during the run (see speed_hierarchical): a named
    # numeric vector of the additive pieces that sum to the final score, faithful
    # to the objective and arguments actually used. NULL for custom objectives
    # that do not return `components`.
    trace   <- if (hierarchical) object$scores[[lv]] else object$scores
    initial <- if (length(trace)) trace[[1]] else NA_real_
    final   <- pm$final_score %||% NA_real_

    run     <- if (hierarchical) length(object$scores[[lv]]) else object$iterations_run
    stopped <- if (hierarchical) isTRUE(object$stopped_early[[lv]]) else isTRUE(object$stopped_early)

    # --- Evaluation metrics (Phase 4) ---
    evaluation <- list(
      replicate_span = .replicate_spans(df, swap, rc, cc),
      connectedness  = if (isFALSE(connectedness)) {
        list(available = FALSE, reason = "not requested (connectedness = FALSE)")
      } else {
        # NULL (auto) skips very large designs; explicit TRUE forces the fit.
        .design_connectedness(df, swap, block, sf, force = isTRUE(connectedness))
      },
      concurrence = if (is.null(block)) {
        list(available = FALSE, reason = "no block factor")
      } else if (isFALSE(concurrence)) {
        list(available = FALSE, reason = "not requested (concurrence = FALSE)")
      } else {
        # NULL (auto) skips complete blocks; explicit TRUE forces them too.
        .design_concurrence(df, swap, block, force = isTRUE(concurrence))
      },
      efficiency = if (isTRUE(efficiency)) {
        .efficiency_factor(df, swap, rc, cc)
      } else {
        list(available = FALSE, reason = "not requested (set efficiency = TRUE)")
      },
      neighbour = if (want_neighbour) {
        .neighbour_balance(df, swap, rc, cc)
      } else {
        NULL
      }
    )

    list(
      swap            = swap,
      n_treatments    = length(trts),
      treatments      = trts,
      replication     = replication,
      spatial_factors = sf_levels,
      evaluation      = evaluation,
      score = list(
        initial    = initial,
        final      = final,
        components = pm$final_components
      ),
      optim = list(
        objective            = .objective_name(pm$obj_function),
        start_temp           = pm$start_temp,
        cooling_rate         = pm$cooling_rate,
        iterations_requested = pm$iterations,
        iterations_run       = run,
        stopped_early        = stopped
      )
    )
  })
  names(per_level) <- levels

  # Flags.
  hit_cap <- vapply(per_level, function(p) !p$optim$stopped_early, logical(1))
  unequal <- vapply(per_level, function(p) !p$replication$equal, logical(1))
  disconnected <- vapply(per_level, function(p) {
    cn <- p$evaluation$connectedness
    isTRUE(cn$available) && isTRUE(!cn$connected)
  }, logical(1))
  flags <- list(
    hit_iteration_cap   = names(hit_cap)[hit_cap],
    unequal_replication = any(unequal),
    disconnected        = names(disconnected)[disconnected]
  )

  structure(
    list(
      hierarchical = hierarchical,
      layout       = layout,
      levels       = levels,
      per_level    = per_level,
      score        = object$score,
      seed         = object$seed,
      flags        = flags,
      call         = meta$call
    ),
    class = "summary.design"
  )
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
  if (!is.function(fn)) return("unknown")
  known <- list(
    objective_function            = objective_function,
    objective_function_factorial  = objective_function_factorial,
    objective_function_piepho     = objective_function_piepho
  )
  for (nm in names(known)) {
    if (identical(fn, known[[nm]])) return(nm)
  }
  "custom"
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
  lab <- function(s) formatC(s, width = -pad)
  indent <- strrep(" ", pad)
  fmt_int <- function(n) format(n, big.mark = ",", scientific = FALSE, trim = TRUE)
  fmt_num <- function(n) format(round(n, 4), big.mark = ",", trim = TRUE)
  section <- function(title) cat("\n", title, "\n", strrep("-", nchar(title)), "\n", sep = "")

  cat("Design Summary\n")
  cat("==============\n")

  # --- Flags (only when something is worth flagging) ---
  flag_lines <- character()
  if (length(x$flags$hit_iteration_cap)) {
    where <- if (x$hierarchical) {
      paste0(" (", paste(x$flags$hit_iteration_cap, collapse = ", "), ")")
    } else {
      ""
    }
    flag_lines <- c(flag_lines,
                    paste0("! Ran to iteration cap - may not have converged", where))
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
    flag_lines <- c(flag_lines, "! Unequal replication (expected for p-rep designs)")
  }
  if (length(flag_lines)) {
    section("Flags")
    for (fl in flag_lines) cat(fl, "\n", sep = "")
  }

  # --- Structure ---
  section("Structure")
  lo <- x$layout
  if (isTRUE(lo$has_grid)) {
    cat(lab("Layout:"),
        sprintf("%d rows x %d cols (%s plots)", lo$nrow, lo$ncol, fmt_int(lo$n_plots)),
        "\n", sep = "")
  } else {
    cat(lab("Layout:"), sprintf("%s plots", fmt_int(lo$n_plots)), "\n", sep = "")
  }
  for (lv in x$levels) .print_level_structure(x, lv, lab, indent, fmt_num)

  # --- Optimisation ---
  section("Optimisation")
  cat(lab("Seed:"), x$seed, "\n", sep = "")
  # The overall score is the sum of per-level scores; only meaningful to show
  # separately when there is more than one level.
  if (x$hierarchical) cat(lab("Total score:"), fmt_num(x$score), "\n", sep = "")
  for (lv in x$levels) .print_level_optim(x, lv, lab, indent, fmt_int, fmt_num)

  # --- Evaluation ---
  section("Evaluation")
  for (lv in x$levels) .print_level_evaluation(x, lv, lab, fmt_num)

  invisible(x)
}

#' Print one level's evaluation block (connectedness, concurrence, spans, ...)
#'
#' @keywords internal
.print_level_evaluation <- function(x, lv, lab, fmt_num) {
  e <- x$per_level[[lv]]$evaluation
  if (x$hierarchical) cat("\n[", lv, "]\n", sep = "")

  # Connectedness
  cn <- e$connectedness
  if (isTRUE(cn$available)) {
    state <- if (isTRUE(cn$connected)) "connected" else "DISCONNECTED"
    cat(lab("Connected:"), sprintf("%s - %s [%s]", state, cn$message, cn$method),
        "\n", sep = "")
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
    cat(lab("Concurrence:"),
        sprintf("lambda %d-%d%s%s [block: %s]",
                cc$lambda_min, cc$lambda_max, constant, zero, cc$block),
        "\n", sep = "")
  } else {
    cat(lab("Concurrence:"), cc$reason, "\n", sep = "")
  }

  # Replicate span
  rs <- e$replicate_span
  if (isTRUE(rs$available) && rs$n_replicated > 0) {
    cat(lab("Repl. span:"),
        sprintf("worst-case %s (row), %s (col) across %d replicated treatment(s)",
                fmt_num(rs$min_row_span), fmt_num(rs$min_col_span), rs$n_replicated),
        "\n", sep = "")
  } else if (isTRUE(rs$available)) {
    cat(lab("Repl. span:"), "n/a (no replicated treatments)\n", sep = "")
  } else {
    cat(lab("Repl. span:"), rs$reason, "\n", sep = "")
  }

  # Efficiency
  ef <- e$efficiency
  if (isTRUE(ef$available)) {
    cat(lab("Efficiency:"), fmt_num(ef$value), " (A-efficiency, row-column model)\n",
        sep = "")
  } else {
    cat(lab("Efficiency:"), ef$reason, "\n", sep = "")
  }

  # Neighbour balance (only when computed)
  nb <- e$neighbour
  if (!is.null(nb) && isTRUE(nb$available)) {
    cat(lab("Neighbour:"),
        sprintf("pair-count variance %s, most-repeated pair x%d",
                fmt_num(nb$nb_var), nb$max_pair_count),
        "\n", sep = "")
  }
}

#' Print one level's structure block (treatments, replication, spatial factors)
#'
#' @keywords internal
.print_level_structure <- function(x, lv, lab, indent, fmt_num) {
  p <- x$per_level[[lv]]
  rep <- p$replication
  if (x$hierarchical) cat("\n[", lv, "]\n", sep = "")

  rep_label <- if (x$hierarchical) "Plots/trt:" else "Replication:"
  if (rep$equal) {
    rep_str <- sprintf("%d each", rep$min)
  } else {
    rep_str <- sprintf("min/mean/max %d / %s / %d",
                       rep$min, fmt_num(rep$mean), rep$max)
  }

  cat(lab("Treatments:"), p$n_treatments, "\n", sep = "")
  cat(lab(rep_label), rep_str, "\n", sep = "")
  sf <- p$spatial_factors
  if (length(sf)) {
    cat(lab("Spatial:"),
        paste(sprintf("%s (%d)", names(sf), sf), collapse = ", "), "\n", sep = "")
  }
}

# ---------------------------------------------------------------------------
# Phase 4 evaluation helpers (base R, no new dependencies). Each returns a list
# with an `available` flag; when FALSE it carries a `reason` string so the
# printer can show a one-line note instead of a value.
# ---------------------------------------------------------------------------

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
  if (is.null(meta) || is.null(meta$per_level)) return(df)
  swap_cols <- unique(vapply(meta$per_level, function(p) p$swap, character(1)))
  swap_cols <- swap_cols[swap_cols %in% names(df)]
  if (!length(swap_cols)) return(df)
  buffer_rows <- logical(nrow(df))
  for (s in swap_cols) buffer_rows <- buffer_rows | as.character(df[[s]]) == "buffer"
  if (!any(buffer_rows)) return(df)
  df <- df[!buffer_rows, , drop = FALSE]
  for (s in swap_cols) if (is.factor(df[[s]])) df[[s]] <- droplevels(df[[s]])
  df
}

#' Replicate spatial spans
#'
#' For each treatment, the minimum Manhattan separation between its replicate
#' plots along rows and along columns (`+ 1` so the span counts plots
#' inclusively), plus the worst-case (minimum) span across replicated
#' treatments. Adapted from a colleague's `sommario.duplicates.span.doe`. A
#' small worst-case span flags replicates that sit close together.
#'
#' @param df Design data frame.
#' @param swap Treatment column name.
#' @param rc,cc Row and column column names.
#' @keywords internal
.replicate_spans <- function(df, swap, rc, cc) {
  if (!all(c(rc, cc) %in% names(df))) {
    return(list(available = FALSE, reason = "no row/column factors"))
  }
  span1 <- function(x) {
    if (length(x) < 2) NA_real_ else min(stats::dist(x, "manhattan")) + 1
  }
  rows <- as_numeric_factor(df[[rc]])
  cols <- as_numeric_factor(df[[cc]])
  trt  <- df[[swap]]
  row_spans <- tapply(rows, trt, span1)
  col_spans <- tapply(cols, trt, span1)
  has_reps <- !is.na(row_spans)
  list(
    available    = TRUE,
    row_spans    = row_spans,
    col_spans    = col_spans,
    min_row_span = if (any(has_reps)) min(row_spans, na.rm = TRUE) else NA_real_,
    min_col_span = if (any(has_reps)) min(col_spans, na.rm = TRUE) else NA_real_,
    n_replicated = sum(has_reps)
  )
}

#' Detect a block-type factor in a design
#'
#' Among the spatial factors (across levels) that are not the row or column
#' factor, prefer one named like `block`; otherwise take the first such factor.
#' Failing that, fall back to a column literally named `block`. The chosen factor
#' is surfaced in the concurrence output (`[block: ...]`), so the choice is
#' visible to the user.
#'
#' @keywords internal
.design_block_factor <- function(df, meta, rc, cc) {
  sc <- unique(unlist(lapply(meta$per_level, function(p) p$spatial_cols)))
  cand <- setdiff(sc, c(rc, cc))
  cand <- cand[cand %in% names(df)]
  block_like <- cand[grepl("block", cand, ignore.case = TRUE)]
  if (length(block_like)) return(block_like[[1]])
  if (length(cand)) return(cand[[1]])
  if ("block" %in% names(df)) return("block")
  NULL
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
.design_connectedness <- function(df, swap, block, spatial_cols, force = FALSE) {
  n_trt <- length(unique(df[[swap]]))
  if (n_trt < 2) {
    return(list(available = FALSE, reason = "needs >= 2 treatments"))
  }
  nuisance <- setdiff(intersect(unique(c(spatial_cols, block)), names(df)), swap)
  if (length(nuisance) == 0) {
    return(list(
      available = TRUE, method = "none", connected = TRUE, n_aliased = 0L,
      message = "no blocking structure (trivially connected)"
    ))
  }
  # Guard against expensive fits: the dense model matrix has ~p columns and the
  # QR is O(n * p^2). Skip (unless forced) when that is large enough to be slow.
  p <- 1 + sum(vapply(nuisance, function(f) length(unique(df[[f]])) - 1L, integer(1))) +
    (n_trt - 1)
  if (!force && as.double(nrow(df)) * p^2 > 1e9) {
    return(list(available = FALSE,
                reason = "large design - set connectedness = TRUE to compute"))
  }
  d <- df[, c(nuisance, swap)]
  d[] <- lapply(d, factor)
  d[["..y.."]] <- 0  # dummy response; estimability is a rank property
  fit <- stats::lm(stats::reformulate(c(nuisance, swap), "..y.."), data = d)
  co <- stats::coef(fit)
  n_aliased <- sum(is.na(co) & startsWith(names(co), swap))
  model <- paste(nuisance, collapse = " + ")
  list(
    available = TRUE, method = sprintf("model (%s)", model),
    connected = n_aliased == 0, n_aliased = n_aliased,
    message = if (n_aliased == 0) {
      sprintf("treatment estimable given %s", model)
    } else {
      sprintf("%d treatment contrast(s) not estimable given %s", n_aliased, model)
    }
  )
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
  if (is.null(block)) return(list(available = FALSE, reason = "no block factor"))
  M <- table(df[[swap]], df[[block]])
  complete <- !any(M == 0)  # every treatment present in every block
  if (complete && !force) {
    return(list(available = FALSE, complete = TRUE,
                reason = "complete blocks - not informative"))
  }
  C <- M %*% t(M)
  lambda <- C[upper.tri(C)]
  list(
    available       = TRUE,
    block           = block,
    complete        = complete,
    lambda_min      = min(lambda),
    lambda_max      = max(lambda),
    lambda_constant = length(unique(lambda)) == 1,
    n_zero_pairs    = sum(lambda == 0)
  )
}

#' A-efficiency factor (opt-in wrapper)
#'
#' Thin guarded wrapper over [calculate_efficiency_factor()] (a row--column model
#' metric), using the design's resolved row/column columns. Returns `NA` with a
#' reason rather than erroring when its assumptions are not met.
#'
#' @param rc,cc Row and column column names.
#' @keywords internal
.efficiency_factor <- function(df, swap, rc, cc) {
  if (!all(c(rc, cc) %in% names(df))) {
    return(list(available = FALSE, reason = "requires a row/column grid"))
  }
  if (length(unique(df[[swap]])) < 3) {
    return(list(available = FALSE, reason = "requires >= 3 treatments"))
  }
  ef <- tryCatch(
    eval(bquote(calculate_efficiency_factor(df, .(as.name(swap)),
                                            row_column = rc, col_column = cc))),
    error = function(e) NULL
  )
  if (is.null(ef) || !is.finite(ef)) {
    return(list(available = FALSE, reason = "could not be computed for this design"))
  }
  list(available = TRUE, value = ef)
}

#' Neighbour-balance diagnostics
#'
#' Builds the treatment grid and reports the variance of adjacent-pair counts
#' (the quantity `objective_function_piepho` minimises via `nb_score`) and the
#' most frequent adjacent pair count.
#'
#' @keywords internal
.neighbour_balance <- function(df, swap, rc, cc) {
  if (!all(c(rc, cc) %in% names(df))) {
    return(list(available = FALSE, reason = "no row/column factors"))
  }
  dm <- matrix(
    df[[swap]],
    nrow = max(as_numeric_factor(df[[rc]]), na.rm = TRUE),
    ncol = max(as_numeric_factor(df[[cc]]), na.rm = TRUE)
  )
  nb <- calculate_nb(dm)
  list(available = TRUE, nb_var = nb$var, max_pair_count = nb$max_nb)
}

#' Print one level's optimisation block (objective, score components, schedule)
#'
#' @keywords internal
.print_level_optim <- function(x, lv, lab, indent, fmt_int, fmt_num) {
  p <- x$per_level[[lv]]
  s <- p$score
  o <- p$optim
  if (x$hierarchical) cat("\n[", lv, "]\n", sep = "")

  cat(lab("Objective:"), o$objective, "\n", sep = "")
  cat(lab("Score:"),
      sprintf("%s  (initial %s -> final %s)",
              fmt_num(s$final), fmt_num(s$initial), fmt_num(s$final)),
      "\n", sep = "")
  # Faithful additive decomposition of the score, when the objective exposes one
  # (the components sum to the final score). Custom objectives may omit it.
  comp <- s$components
  if (!is.null(comp) && length(comp)) {
    w <- max(nchar(names(comp)))
    for (nm in names(comp)) {
      cat(indent, formatC(nm, width = -(w + 2)), fmt_num(comp[[nm]]), "\n", sep = "")
    }
  }
  iter <- sprintf("%s / %s", fmt_int(o$iterations_run), fmt_int(o$iterations_requested))
  if (!o$stopped_early) iter <- paste0(iter, " (ran to cap)")
  cat(lab("Iterations:"), iter, "\n", sep = "")
  cat(lab("Temperature:"),
      sprintf("start %s, cooling %s", fmt_num(o$start_temp), fmt_num(o$cooling_rate)),
      "\n", sep = "")
}
