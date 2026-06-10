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
