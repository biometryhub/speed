#' Summarise a speed design
#'
#' Produces a richer, statistically meaningful evaluation of a design than
#' [print()][print.design()]. Where `print()` is a compact identity card,
#' `summary()` decomposes the optimised score and reports structural and
#' evaluation metrics that let you interrogate and defend a design.
#'
#' The returned object is a list of class `"summary.design"`; it can be assigned
#' and queried programmatically (e.g. `s <- summary(d); s$per_level$score`).
#' Printing it is handled by [print.summary.design()].
#'
#' @param object A `"design"` object returned by [speed()].
#' @param efficiency Logical; compute the A-efficiency factor (a row--column
#'   model metric). Off by default as it is the heaviest metric. *(Computed in a
#'   later phase.)*
#' @param connectedness Logical; assess whether the design is connected.
#'   *(Computed in a later phase.)*
#' @param concurrence `NULL` (default) computes treatment concurrences when a
#'   block factor is present, otherwise skips. *(Computed in a later phase.)*
#' @param neighbour `NULL` (default) reports neighbour-balance diagnostics when
#'   the design was optimised with a neighbour-balance objective; override with
#'   `TRUE`/`FALSE`. *(Computed in a later phase.)*
#' @param ... Unused; for S3 compatibility.
#'
#' @returns A list of class `"summary.design"` with elements including
#'   `hierarchical`, `layout`, `levels`, `per_level` (structure + optimisation
#'   per level), `score`, `seed`, `flags` and the recorded `call`.
#'
#' @seealso [print.design()], [speed()]
#'
#' @export
summary.design <- function(object,
                           efficiency    = FALSE,
                           connectedness = TRUE,
                           concurrence   = NULL,
                           neighbour     = NULL,
                           ...) {
  meta <- object$metadata
  if (is.null(meta)) {
    stop("This design has no `metadata`; it may predate the summary() method. ",
         "Re-run speed() to produce a summarisable design.", call. = FALSE)
  }
  df <- object$design_df
  hierarchical <- is.list(object$treatments)
  rc <- meta$row_column %||% "row"
  cc <- meta$col_column %||% "col"
  levels <- meta$levels %||% names(meta$per_level)

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

    # Spatial diagnostics, always meaningful regardless of the objective used.
    # Recomputed with default ring settings; the headline score is the stored
    # `final` (which honours whatever objective/ring args were used).
    adj_raw <- calculate_adjacency_score(df, swap, row_column = rc, col_column = cc)
    bal_raw <- calculate_balance_score(df, swap, sf)

    trace   <- if (hierarchical) object$scores[[lv]] else object$scores
    initial <- if (length(trace)) trace[[1]] else NA_real_
    final   <- pm$final_score %||% NA_real_

    run     <- if (hierarchical) length(object$scores[[lv]]) else object$iterations_run
    stopped <- if (hierarchical) isTRUE(object$stopped_early[[lv]]) else isTRUE(object$stopped_early)

    list(
      swap            = swap,
      n_treatments    = length(trts),
      treatments      = trts,
      replication     = replication,
      spatial_factors = sf_levels,
      score = list(
        adjacency        = adj_raw,
        adj_weight       = pm$adj_weight,
        adj_contribution = pm$adj_weight * adj_raw,
        balance          = bal_raw,
        bal_weight       = pm$bal_weight,
        bal_contribution = pm$bal_weight * bal_raw,
        initial          = initial,
        final            = final
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

  # Flags (subset; disconnected design is added with the connectedness metric).
  hit_cap <- vapply(per_level, function(p) !p$optim$stopped_early, logical(1))
  unequal <- vapply(per_level, function(p) !p$replication$equal, logical(1))
  flags <- list(
    hit_iteration_cap   = names(hit_cap)[hit_cap],
    unequal_replication = any(unequal)
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
      call         = meta$call,
      settings     = list(
        efficiency    = efficiency,
        connectedness = connectedness,
        concurrence   = concurrence,
        neighbour     = neighbour
      )
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

  invisible(x)
}

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
  cat(indent, sprintf("adjacency  %s x %s = %s",
                      fmt_num(s$adjacency), fmt_num(s$adj_weight),
                      fmt_num(s$adj_contribution)), "\n", sep = "")
  cat(indent, sprintf("balance    %s x %s = %s",
                      fmt_num(s$balance), fmt_num(s$bal_weight),
                      fmt_num(s$bal_contribution)), "\n", sep = "")
  iter <- sprintf("%s / %s", fmt_int(o$iterations_run), fmt_int(o$iterations_requested))
  if (!o$stopped_early) iter <- paste0(iter, " (ran to cap)")
  cat(lab("Iterations:"), iter, "\n", sep = "")
  cat(lab("Temperature:"),
      sprintf("start %s, cooling %s", fmt_num(o$start_temp), fmt_num(o$cooling_rate)),
      "\n", sep = "")
}
