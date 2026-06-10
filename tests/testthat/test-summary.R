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
                    "seed", "flags", "call", "settings"))

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

  expect_named(sc, c("adjacency", "adj_weight", "adj_contribution",
                     "balance", "bal_weight", "bal_contribution",
                     "initial", "final"))
  # Weighted components sum to the recomputed final score (default objective).
  expect_equal(sc$adj_contribution + sc$bal_contribution, sc$final)
  # The level's final score matches the overall design score for a simple design.
  expect_equal(sc$final, s$score)
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
  expect_match(out, "Iterations:.*/")
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

test_that(".design_connectedness detects confounding (model) and components (graph)", {
  # treatment perfectly aliased with row -> not estimable
  dconf <- data.frame(row = rep(1:4, each = 3), col = rep(1:3, times = 4),
                      treatment = rep(LETTERS[1:4], each = 3))
  cn <- .design_connectedness(dconf, "treatment", NULL, "row", "col")
  expect_false(cn$connected)
  expect_gt(cn$n_aliased, 0)

  # block-graph path: two disjoint treatment groups
  dg <- data.frame(block = c(1, 1, 2, 2, 3, 3, 4, 4),
                   treatment = c("A", "B", "A", "B", "C", "D", "C", "D"))
  disc <- .design_connectedness(dg, "treatment", "block", NA, NA)
  expect_match(disc$method, "block graph")
  expect_false(disc$connected)
  expect_equal(disc$n_components, 2)

  dg2 <- data.frame(block = c(1, 1, 2, 2, 3, 3),
                    treatment = c("A", "B", "B", "C", "C", "A"))
  conn <- .design_connectedness(dg2, "treatment", "block", NA, NA)
  expect_true(conn$connected)
  expect_equal(conn$n_components, 1)
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
    "treatment"
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
