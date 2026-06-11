# Tests for summary.design / print.summary.design (Phase 3: Structure +
# Optimisation + flags). Evaluation metrics are covered separately.

simple_design <- function(iterations = 200, seed = 42) {
  d <- data.frame(
    row = rep(1:4, times = 3),
    col = rep(1:3, each = 4),
    treatment = rep(LETTERS[1:3], 4)
  )
  speed(d, swap = "treatment", swap_within = "1", spatial_factors = ~ row + col,
        iterations = iterations, seed = seed, quiet = TRUE)
}

split_plot_design <- function(seed = 42) {
  d <- data.frame(
    row = rep(1:6, each = 4),
    col = rep(1:4, times = 6),
    wholeplot_treatment = rep(LETTERS[1:3], each = 8),
    subplot_treatment = rep(letters[1:4], 6),
    block = rep(1:2, each = 12)
  )
  speed(d,
        swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
        swap_within = list(wp = "block", sp = "wholeplot_treatment"),
        spatial_factors = ~ row + col,
        iterations = list(wp = 100, sp = 100), seed = seed, quiet = TRUE)
}

prep_design <- function(seed = 1) {
  d <- data.frame(
    row = rep(1:4, each = 4),
    col = rep(1:4, times = 4),
    treatment = c(LETTERS[1:8], rep(c("chk1", "chk2"), 4))
  )
  speed(d, swap = "treatment", spatial_factors = ~ row + col,
        iterations = 50, seed = seed, quiet = TRUE)
}

test_that("summary.design returns a summary.design with the expected shape", {
  s <- summary(simple_design())

  expect_s3_class(s, "summary.design")
  expect_false(s$hierarchical)
  expect_named(s, c("hierarchical", "layout", "levels", "per_level", "score",
                    "seed", "flags", "call"))

  # Layout
  expect_equal(s$layout$n_plots, 12)
  expect_equal(s$layout$nrow, 4)
  expect_equal(s$layout$ncol, 3)
  expect_true(s$layout$has_grid)

  # One level, with structure + optimisation sub-lists
  expect_length(s$per_level, 1)
  lvl <- s$per_level[[1]]
  expect_equal(lvl$n_treatments, 3)
  expect_setequal(lvl$treatments, c("A", "B", "C"))
  expect_true(lvl$replication$equal)
  expect_equal(lvl$replication$min, 4)
  expect_equal(lvl$replication$max, 4)
  expect_named(lvl$spatial_factors, c("row", "col"))
  expect_equal(lvl$optim$objective, "objective_function")
})

test_that("summary score components are programmatically accessible", {
  s <- summary(simple_design())
  sc <- s$per_level[[1]]$score

  expect_named(sc, c("initial", "final", "components"))
  # The default objective decomposes into adjacency + balance.
  expect_named(sc$components, c("adjacency", "balance"))
  # The components sum to the final score (faithful decomposition).
  expect_equal(sum(sc$components), sc$final)
  # The level's final score matches the overall design score for a simple design.
  expect_equal(sc$final, s$score)
})

test_that("score components are faithful for non-default objectives (piepho)", {
  d <- data.frame(row = rep(1:5, each = 5), col = rep(1:5, times = 5),
                  treatment = rep(LETTERS[1:5], 5))
  r <- speed(d, swap = "treatment", spatial_factors = ~ row + col,
             obj_function = objective_function_piepho, iterations = 500, seed = 1,
             quiet = TRUE)
  sc <- summary(r)$per_level[[1]]$score
  # Piepho exposes four additive components that sum to its score - the bug this
  # fixes was the old adjacency+balance recompute not matching the piepho score.
  expect_named(sc$components,
               c("neighbour_balance", "even_distribution", "balance", "adjacency"))
  expect_equal(sum(sc$components), sc$final)
})

test_that("custom objectives without components degrade gracefully", {
  custom <- function(layout_df, swap, spatial_cols, adj_weight = 1, bal_weight = 1, ...) {
    list(score = calculate_balance_score(layout_df, swap, spatial_cols))
  }
  d <- data.frame(row = rep(1:4, times = 3), col = rep(1:3, each = 4),
                  treatment = rep(LETTERS[1:3], 4))
  r <- speed(d, swap = "treatment", swap_within = "1", spatial_factors = ~ row + col,
             obj_function = custom, iterations = 100, seed = 1, quiet = TRUE)
  sc <- summary(r)$per_level[[1]]$score
  expect_null(sc$components)
  # Still prints without error (no decomposition shown).
  expect_no_error(capture_output(print(summary(r))))
})

test_that("summary handles hierarchical designs with per-level metrics", {
  s <- summary(split_plot_design())

  expect_true(s$hierarchical)
  expect_equal(s$levels, c("wp", "sp"))
  expect_length(s$per_level, 2)
  expect_equal(s$per_level$wp$n_treatments, 3)
  expect_equal(s$per_level$sp$n_treatments, 4)

  # Overall score is the sum of the per-level final scores.
  per_level_total <- s$per_level$wp$score$final + s$per_level$sp$score$final
  expect_equal(s$score, per_level_total)
})

test_that("flags fire for unequal replication and hitting the iteration cap", {
  s <- summary(prep_design())

  expect_true(s$flags$unequal_replication)
  expect_false(s$per_level[[1]]$replication$equal)
  expect_equal(s$per_level[[1]]$replication$min, 1)  # single-rep entries
  expect_equal(s$per_level[[1]]$replication$max, 4)  # checks
  # 50 iterations with no early stop -> hit the cap.
  expect_true(length(s$flags$hit_iteration_cap) >= 1)
})

test_that("print.summary.design shows the expected sections", {
  out <- capture_output(print(summary(simple_design())))

  expect_match(out, "Design Summary")
  expect_match(out, "Structure")
  expect_match(out, "Optimisation")
  expect_match(out, "Layout:.*plots")
  expect_match(out, "Replication:")
  expect_match(out, "Objective:.*objective_function")
  expect_match(out, "adjacency")
  expect_match(out, "balance")
  # Iterations report run / total plus an explicit convergence note.
  expect_match(out, "Iterations:.*/.*\\((stopped early|ran to cap)\\)")
})

test_that("print.summary.design shows per-level blocks and a total for hierarchical", {
  out <- capture_output(print(summary(split_plot_design())))

  expect_match(out, "\\[wp\\]")
  expect_match(out, "\\[sp\\]")
  expect_match(out, "Total score:")
  expect_match(out, "Plots/trt:")  # hierarchical replication label
})

test_that("print.summary.design returns the object invisibly", {
  s <- summary(simple_design())
  expect_identical(print(s), s)
})

test_that(".objective_name maps known objectives and falls back to custom", {
  expect_equal(.objective_name(objective_function), "objective_function")
  expect_equal(.objective_name(objective_function_factorial),
               "objective_function_factorial")
  expect_equal(.objective_name(objective_function_piepho),
               "objective_function_piepho")
  expect_equal(.objective_name(function(...) NULL), "custom")
  expect_equal(.objective_name("not a function"), "unknown")
})

test_that("summary errors clearly when metadata is absent", {
  d <- simple_design()
  d$metadata <- NULL
  expect_error(summary(d), "no `metadata`")
})

test_that("buffers are excluded from summary() and print() entirely", {
  r  <- simple_design()
  rb <- add_buffers(r, "edge")
  # The buffered design carries extra "buffer" rows...
  expect_gt(nrow(rb$design_df), nrow(r$design_df))

  s <- summary(rb)
  # ...but the summary reports only the experimental units, identical to the
  # unbuffered design.
  expect_equal(s$layout$n_plots, nrow(r$design_df))
  expect_equal(s$layout$nrow, 4)
  expect_equal(s$layout$ncol, 3)
  expect_true(s$per_level[[1]]$replication$equal)
  expect_equal(s$per_level[[1]]$replication$max, 4)
  expect_equal(s$per_level[[1]]$n_treatments, 3)

  # Buffers are never mentioned, and there is no buffer-count field.
  expect_false("n_buffers" %in% names(s$layout))
  expect_false(any(grepl("buffer", capture_output(print(s)), ignore.case = TRUE)))
  expect_false(any(grepl("buffer", capture_output(print(rb)), ignore.case = TRUE)))
})

# --- Phase 4: evaluation metrics ------------------------------------------

block_design <- function(seed = 7) {
  d <- data.frame(
    row = rep(1:4, each = 6),
    col = rep(1:6, times = 4),
    block = rep(1:4, each = 6),
    treatment = rep(LETTERS[1:6], 4)
  )
  speed(d, swap = "treatment", swap_within = "block",
        spatial_factors = ~ row + col + block, iterations = 300, seed = seed,
        quiet = TRUE)
}

test_that("replicate spans are computed per level", {
  rs <- summary(simple_design())$per_level[[1]]$evaluation$replicate_span
  expect_true(rs$available)
  expect_equal(rs$n_replicated, 3)
  expect_true(is.finite(rs$min_row_span))
  expect_true(is.finite(rs$min_col_span))
  # Spans are at least 2 (a replicate cannot be 0 apart; +1 inclusive).
  expect_gte(rs$min_row_span, 2)
})

test_that("connectedness uses the model path for grid designs", {
  cn <- summary(simple_design())$per_level[[1]]$evaluation$connectedness
  expect_true(cn$available)
  expect_match(cn$method, "model")
  expect_true(cn$connected)
})

test_that(".design_connectedness flags non-estimable treatments via the model", {
  # treatment fully confounded with row -> not estimable given row + col
  dconf <- data.frame(row = rep(1:4, each = 3), col = rep(1:3, times = 4),
                      treatment = rep(LETTERS[1:4], each = 3))
  cn <- .design_connectedness(dconf, "treatment", NULL, c("row", "col"))
  expect_false(cn$connected)
  expect_gt(cn$n_aliased, 0)

  # two variety groups that never share a block -> disconnected given block
  dg <- data.frame(block = c(1, 1, 2, 2, 3, 3, 4, 4),
                   treatment = c("A", "B", "A", "B", "C", "D", "C", "D"))
  disc <- .design_connectedness(dg, "treatment", "block", character(0))
  expect_match(disc$method, "block")
  expect_false(disc$connected)
  expect_gt(disc$n_aliased, 0)

  # a connected block structure
  dg2 <- data.frame(block = c(1, 1, 2, 2, 3, 3),
                    treatment = c("A", "B", "B", "C", "C", "A"))
  conn <- .design_connectedness(dg2, "treatment", "block", character(0))
  expect_true(conn$connected)
  expect_equal(conn$n_aliased, 0)
})

test_that("connectedness skips very large designs unless forced, and FALSE skips entirely", {
  # A large layout: O(n * p^2) lm would be expensive, so the auto path skips it
  # (this returns before any model is fitted, so the test stays fast).
  big <- data.frame(
    row = rep(1:200, each = 200),
    col = rep(1:200, times = 200),
    treatment = factor(rep(seq_len(1000), length.out = 40000))
  )
  skipped <- .design_connectedness(big, "treatment", NULL, c("row", "col"), force = FALSE)
  expect_false(skipped$available)
  expect_match(skipped$reason, "large design")

  # Small designs still compute on the auto (NULL) path.
  auto <- summary(simple_design())$per_level[[1]]$evaluation$connectedness
  expect_true(auto$available)

  # connectedness = FALSE skips entirely.
  off <- summary(simple_design(), connectedness = FALSE)$per_level[[1]]$evaluation$connectedness
  expect_false(off$available)
  expect_match(off$reason, "connectedness = FALSE")
})

test_that("connectedness does not false-positive when block is collinear with rows", {
  # BIBD laid out one block per row: block aliases row, but treatment is fully
  # estimable. Counting aliasing only among treatment terms must keep it connected.
  bibd <- data.frame(
    row   = rep(1:6, each = 2),
    block = rep(1:6, each = 2),
    col   = rep(1:2, times = 6),
    treatment = c("A", "B", "A", "C", "A", "D", "B", "C", "B", "D", "C", "D")
  )
  cn <- .design_connectedness(bibd, "treatment", "block", c("row", "col"))
  expect_true(cn$connected)
  expect_equal(cn$n_aliased, 0)
})

test_that("concurrence is computed for incomplete blocks, skipped for complete", {
  # Incomplete BIBD: 4 treatments in 6 blocks of size 2, every pair once.
  inc <- data.frame(
    block = rep(1:6, each = 2),
    treatment = c("A", "B", "A", "C", "A", "D", "B", "C", "B", "D", "C", "D")
  )
  cc <- .design_concurrence(inc, "treatment", "block")
  expect_true(cc$available)
  expect_false(cc$complete)
  expect_true(cc$lambda_constant)   # BIBD: lambda = 1 for every pair
  expect_equal(cc$lambda_max, 1)

  # Complete blocks (RCBD): every treatment in every block -> uninformative.
  comp <- data.frame(block = rep(1:3, each = 4),
                     treatment = rep(c("A", "B", "C", "D"), 3))
  cc2 <- .design_concurrence(comp, "treatment", "block")
  expect_false(cc2$available)
  expect_true(cc2$complete)

  # ...unless forced.
  cc3 <- .design_concurrence(comp, "treatment", "block", force = TRUE)
  expect_true(cc3$available)
  expect_true(cc3$complete)
})

test_that("complete-block designs auto-skip concurrence in summary()", {
  # block_design() is an RCBD (each block holds every treatment once).
  cc <- summary(block_design())$per_level[[1]]$evaluation$concurrence
  expect_false(cc$available)
  expect_true(isTRUE(cc$complete))

  # Forcing it on computes it anyway.
  cc_forced <- summary(block_design(), concurrence = TRUE)$per_level[[1]]$evaluation$concurrence
  expect_true(cc_forced$available)
})

test_that(".design_block_factor prefers a block-named factor over others", {
  df <- data.frame(row = 1, col = 1, rep = 1, block = 1, treatment = "A")
  # spatial factors listed with 'rep' before 'block' - block should still win.
  meta <- list(per_level = list(l1 = list(spatial_cols = c("row", "col", "rep", "block"))))
  expect_equal(.design_block_factor(df, meta, "row", "col"), "block")

  # No block-named factor: falls back to the first non-row/col spatial factor.
  meta2 <- list(per_level = list(l1 = list(spatial_cols = c("row", "col", "rep"))))
  expect_equal(.design_block_factor(df, meta2, "row", "col"), "rep")
})

test_that("concurrence is skipped for designs without a block", {
  cc <- summary(simple_design())$per_level[[1]]$evaluation$concurrence
  expect_false(cc$available)
  expect_match(cc$reason, "no block factor")
})

test_that("efficiency is opt-in and guarded", {
  off <- summary(simple_design())$per_level[[1]]$evaluation$efficiency
  expect_false(off$available)
  expect_match(off$reason, "efficiency = TRUE")

  on <- summary(simple_design(), efficiency = TRUE)$per_level[[1]]$evaluation$efficiency
  expect_true(on$available)
  expect_true(is.finite(on$value))

  # Guard: < 3 treatments returns NA with a reason rather than erroring.
  two <- .efficiency_factor(
    data.frame(row = rep(1:2, 2), col = rep(1:2, each = 2),
               treatment = rep(c("A", "B"), 2)),
    "treatment", "row", "col"
  )
  expect_false(two$available)
})

test_that("neighbour balance auto-detects the piepho objective", {
  d <- data.frame(row = rep(1:4, times = 3), col = rep(1:3, each = 4),
                  treatment = rep(LETTERS[1:3], 4))
  r <- speed(d, swap = "treatment", swap_within = "1", spatial_factors = ~ row + col,
             obj_function = objective_function_piepho, iterations = 200, seed = 42,
             quiet = TRUE)
  nb <- summary(r)$per_level[[1]]$evaluation$neighbour
  expect_false(is.null(nb))
  expect_true(nb$available)
  expect_true(is.finite(nb$nb_var))

  # Default (objective_function) does not compute neighbour balance.
  expect_null(summary(simple_design())$per_level[[1]]$evaluation$neighbour)
})

test_that("the disconnected flag fires and prints", {
  # Build a summary then inject a disconnected verdict to exercise the flag path.
  s <- summary(simple_design())
  s$per_level[[1]]$evaluation$connectedness <-
    list(available = TRUE, connected = FALSE, method = "model (row + col)",
         message = "2 aliased coefficient(s)")
  s$flags$disconnected <- names(s$per_level)
  out <- capture_output(print(s))
  expect_match(out, "DISCONNECTED")
})

test_that("the Evaluation section prints expected metrics", {
  out <- capture_output(print(summary(block_design())))
  expect_match(out, "Evaluation")
  expect_match(out, "Connected:")
  expect_match(out, "Repl. span:")
  # Complete blocks -> concurrence shown as a skip note, no lambda.
  expect_match(out, "Concurrence:.*complete blocks")

  # Forcing concurrence prints the lambda line.
  out_forced <- capture_output(print(summary(block_design(), concurrence = TRUE)))
  expect_match(out_forced, "Concurrence:.*lambda")
})
