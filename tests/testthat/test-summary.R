# Tests for summary.design / print.summary.design (Phase 3: Structure +
# Optimisation + flags). Evaluation metrics are covered separately.

# The `grid` argument the evaluation helpers take: a grid_indices() list - one
# entry per grid - or the reason there is no grid at all. summary.design() builds
# it inline, once per call; tests calling a helper directly need the same value.
grid_or_reason <- function(df, rc = "row", cc = "col", by = NULL) {
  return(tryCatch(
    grid_indices(df, row_column = rc, col_column = cc, by = by),
    speed_grid_error = function(e) return(e$reason)
  ))
}

simple_design <- function(iterations = 200, seed = 42) {
  d <- data.frame(
    row = rep(1:4, times = 3),
    col = rep(1:3, each = 4),
    treatment = rep(LETTERS[1:3], 4)
  )
  speed(
    d,
    swap = "treatment",
    swap_within = "1",
    spatial_factors = ~ row + col,
    iterations = iterations,
    seed = seed,
    quiet = TRUE
  )
}

split_plot_design <- function(seed = 42) {
  d <- data.frame(
    row = rep(1:6, each = 4),
    col = rep(1:4, times = 6),
    wholeplot_treatment = rep(LETTERS[1:3], each = 8),
    subplot_treatment = rep(letters[1:4], 6),
    block = rep(1:2, each = 12)
  )
  speed(
    d,
    swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
    swap_within = list(wp = "block", sp = "wholeplot_treatment"),
    spatial_factors = ~ row + col,
    iterations = list(wp = 100, sp = 100),
    seed = seed,
    quiet = TRUE
  )
}

prep_design <- function(seed = 1) {
  d <- data.frame(
    row = rep(1:4, each = 4),
    col = rep(1:4, times = 4),
    treatment = c(LETTERS[1:8], rep(c("chk1", "chk2"), 4))
  )
  speed(
    d,
    swap = "treatment",
    spatial_factors = ~ row + col,
    iterations = 50,
    seed = seed,
    quiet = TRUE
  )
}

test_that("summary.design returns a summary.design with the expected shape", {
  s <- summary(simple_design())

  expect_s3_class(s, "summary.design")
  expect_false(s$hierarchical)
  expect_named(
    s,
    c(
      "hierarchical",
      "layout",
      "levels",
      "per_level",
      "score",
      "seed",
      "flags",
      "call"
    )
  )

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
  d <- data.frame(
    row = rep(1:5, each = 5),
    col = rep(1:5, times = 5),
    treatment = rep(LETTERS[1:5], 5)
  )
  r <- speed(
    d,
    swap = "treatment",
    spatial_factors = ~ row + col,
    obj_function = objective_function_piepho,
    iterations = 500,
    seed = 1,
    quiet = TRUE
  )
  sc <- summary(r)$per_level[[1]]$score
  # Piepho exposes four additive components, which must sum to its score.
  expect_named(
    sc$components,
    c("neighbour_balance", "even_distribution", "balance", "adjacency")
  )
  expect_equal(sum(sc$components), sc$final)
})

test_that("custom objectives without components degrade gracefully", {
  custom <- function(
    layout_df,
    swap,
    spatial_cols,
    adj_weight = 1,
    bal_weight = 1,
    ...
  ) {
    list(score = calculate_balance_score(layout_df, swap, spatial_cols))
  }
  d <- data.frame(
    row = rep(1:4, times = 3),
    col = rep(1:3, each = 4),
    treatment = rep(LETTERS[1:3], 4)
  )
  r <- speed(
    d,
    swap = "treatment",
    swap_within = "1",
    spatial_factors = ~ row + col,
    obj_function = custom,
    iterations = 100,
    seed = 1,
    quiet = TRUE
  )
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
  expect_equal(s$per_level[[1]]$replication$min, 1) # single-rep entries
  expect_equal(s$per_level[[1]]$replication$max, 4) # checks
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
  expect_match(out, "Plots/trt:") # hierarchical replication label
})

test_that("print.summary.design returns the object invisibly", {
  s <- summary(simple_design())
  expect_identical(print(s), s)
})

test_that("print.summary.design colours section headings and convergence status", {
  s <- summary(simple_design(iterations = 5000)) # enough to stop early

  # Colour is off by default in non-interactive test runs; strip it either way
  # so the plain-text content is unaffected regardless of terminal support.
  out_plain <- capture_output(print(s))
  expect_match(out_plain, "Structure")
  expect_match(out_plain, "stopped early")

  # testthat's reproducible-output setup forces `cli.num_colors = 1`, which
  # crayon checks ahead of `crayon.enabled` - override both to actually force
  # colour on for this test.
  withr::with_options(list(crayon.enabled = TRUE, cli.num_colors = 8), {
    out <- capture_output(print(s))
  })
  expect_match(out, "\033\\[1mStructure\033\\[22m")
  expect_match(out, "\033\\[32m\\(stopped early\\)\033\\[39m")

  cap <- summary(prep_design()) # 50 iterations, no early stop -> hits the cap
  # testthat's reproducible-output setup forces `cli.num_colors = 1`, which
  # crayon checks ahead of `crayon.enabled` - override both to actually force
  # colour on for this test.
  withr::with_options(list(crayon.enabled = TRUE, cli.num_colors = 8), {
    out_cap <- capture_output(print(cap))
  })
  expect_match(
    out_cap,
    "\033\\[1m\033\\[35m\\(ran to cap\\)\033\\[39m\033\\[22m"
  )
})

test_that(".objective_name maps known objectives and falls back to custom", {
  expect_equal(.objective_name(objective_function), "objective_function")
  expect_equal(
    .objective_name(objective_function_factorial),
    "objective_function_factorial"
  )
  expect_equal(
    .objective_name(objective_function_piepho),
    "objective_function_piepho"
  )
  expect_equal(.objective_name(function(...) NULL), "custom")
  expect_equal(.objective_name("not a function"), "unknown")
})

test_that("summary errors clearly when metadata is absent", {
  d <- simple_design()
  d$metadata <- NULL
  expect_error(summary(d), "no `metadata`")
})

test_that("buffers are excluded from summary() and print() entirely", {
  r <- simple_design()
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
  expect_false(any(grepl(
    "buffer",
    capture_output(print(s)),
    ignore.case = TRUE
  )))
  expect_false(any(grepl(
    "buffer",
    capture_output(print(rb)),
    ignore.case = TRUE
  )))
})

test_that("neighbour balance is unaffected by buffers (KNOWN_ISSUES.md #1a)", {
  # add_buffers("edge") shifts row/col by 1. The grid is built from the
  # coordinates, so the offset just leaves an empty leading row and column,
  # which contribute no pairs. "edge" only offsets - it inserts no gap between
  # the design's own plots - so the counts must match the unbuffered design
  # exactly. (A "row" or "block" buffer does insert a gap, and there the counts
  # are expected to differ: plots either side of a buffer are not neighbours.)
  d <- data.frame(
    row = rep(1:4, times = 3),
    col = rep(1:3, each = 4),
    treatment = rep(LETTERS[1:3], 4)
  )
  r <- speed(
    d,
    swap = "treatment",
    swap_within = "1",
    spatial_factors = ~ row + col,
    obj_function = objective_function_piepho,
    iterations = 100,
    seed = 1,
    quiet = TRUE
  )
  rb <- add_buffers(r, "edge")

  nb_unbuffered <- summary(r)$per_level[[1]]$evaluation$neighbour
  expect_no_warning(
    nb_buffered <- summary(rb)$per_level[[1]]$evaluation$neighbour
  )
  expect_true(nb_buffered$available)
  expect_equal(nb_buffered, nb_unbuffered)
})

# --- Phase 4: evaluation metrics ------------------------------------------

block_design <- function(seed = 7) {
  d <- data.frame(
    row = rep(1:4, each = 6),
    col = rep(1:6, times = 4),
    block = rep(1:4, each = 6),
    treatment = rep(LETTERS[1:6], 4)
  )
  speed(
    d,
    swap = "treatment",
    swap_within = "block",
    spatial_factors = ~ row + col + block,
    iterations = 300,
    seed = seed,
    quiet = TRUE
  )
}

test_that("block spread reports how many blocks each treatment reaches", {
  # Complete blocks: concurrence is skipped as uninformative, but block spread
  # still confirms every treatment reaches every block - that is why it is not
  # gated on the `concurrence` argument.
  s <- summary(block_design())
  bs <- s$per_level[[1]]$evaluation$block_spread

  expect_named(
    bs,
    c(
      "available",
      "block",
      "n_blocks",
      "min_blocks",
      "max_blocks",
      "n_within_block_reps"
    )
  )
  expect_true(bs$available)
  expect_equal(bs$block, "block")
  expect_equal(bs$n_blocks, 4)
  expect_equal(bs$min_blocks, 4) # 6 treatments x 4 complete blocks
  expect_equal(bs$max_blocks, 4)
  expect_equal(bs$n_within_block_reps, 0)

  expect_false(s$per_level[[1]]$evaluation$concurrence$available)
  out <- capture_output(print(s))
  expect_match(out, "Blk\\. spread:\\s+each treatment in 4 of 4 blocks")
})

test_that("block spread detects treatments replicated within a block", {
  # Each treatment appears twice in each of 2 blocks.
  d <- data.frame(
    row = rep(1:4, each = 3),
    col = rep(1:3, times = 4),
    block = rep(1:2, each = 6),
    treatment = rep(rep(LETTERS[1:3], each = 2), 2)
  )
  r <- speed(
    d,
    swap = "treatment",
    swap_within = "block",
    spatial_factors = ~ row + col + block,
    iterations = 300,
    seed = 2,
    quiet = TRUE
  )
  bs <- summary(r)$per_level[[1]]$evaluation$block_spread

  expect_equal(bs$n_blocks, 2)
  expect_equal(bs$min_blocks, 2)
  expect_equal(bs$n_within_block_reps, 3) # all three treatments doubled up
  expect_match(
    capture_output(print(summary(r))),
    "Blk\\. spread:.*3 replicated within a block"
  )
})

test_that("block spread reports a reason when there is no block factor", {
  bs <- summary(simple_design())$per_level[[1]]$evaluation$block_spread
  expect_false(bs$available)
  expect_equal(bs$reason, "no block factor")
})

test_that("block spread reports unequal reach across incomplete blocks", {
  # 3 blocks of 2; A is in every block, B and C in one each.
  d <- data.frame(
    row = rep(1:3, each = 2),
    col = rep(1:2, times = 3),
    block = rep(1:3, each = 2),
    treatment = c("A", "B", "A", "C", "A", "B")
  )
  r <- speed(
    d,
    swap = "treatment",
    swap_within = "block",
    spatial_factors = ~ row + col + block,
    iterations = 100,
    seed = 1,
    quiet = TRUE
  )
  bs <- summary(r)$per_level[[1]]$evaluation$block_spread

  expect_equal(bs$n_blocks, 3)
  expect_equal(bs$min_blocks, 1) # C reaches only one block
  expect_equal(bs$max_blocks, 3) # A reaches all three
  expect_match(
    capture_output(print(summary(r))),
    "Blk\\. spread:\\s+treatments in 1-3 of 3 blocks"
  )
})

test_that("replicate spans are computed per level", {
  rs <- summary(simple_design())$per_level[[1]]$evaluation$replicate_span
  expect_true(rs$available)
  expect_equal(rs$n_replicated, 3)
  expect_true(is.finite(rs$min_row_span))
  expect_true(is.finite(rs$min_col_span))
  # Spans are at least 2 (a replicate cannot be 0 apart; +1 inclusive).
  expect_gte(rs$min_row_span, 2)
})

test_that("replicate spans are labelled with the resolved grid columns", {
  # grid_factors renames the axes; the printed labels must follow rather than
  # being hard-coded to "row"/"col".
  d <- data.frame(
    range = rep(1:4, times = 3),
    plot = rep(1:3, each = 4),
    treatment = rep(LETTERS[1:3], 4)
  )
  r <- speed(
    d,
    swap = "treatment",
    spatial_factors = ~ range + plot,
    grid_factors = list(dim1 = "range", dim2 = "plot"),
    iterations = 200,
    seed = 1,
    quiet = TRUE
  )
  out <- capture_output(print(summary(r)))
  expect_match(
    out,
    "Repl\\. span:\\s+worst-case \\d+ \\(range\\), \\d+ \\(plot\\)"
  )
})

test_that("connectedness uses the model path for grid designs", {
  cn <- summary(simple_design())$per_level[[1]]$evaluation$connectedness
  expect_true(cn$available)
  expect_match(cn$method, "model")
  expect_true(cn$connected)
})

test_that(".design_connectedness flags non-estimable treatments via the model", {
  # treatment fully confounded with row -> not estimable given row + col
  dconf <- data.frame(
    row = rep(1:4, each = 3),
    col = rep(1:3, times = 4),
    treatment = rep(LETTERS[1:4], each = 3)
  )
  cn <- .design_connectedness(dconf, "treatment", NULL, c("row", "col"))
  expect_false(cn$connected)
  expect_gt(cn$n_aliased, 0)

  # two variety groups that never share a block -> disconnected given block
  dg <- data.frame(
    block = c(1, 1, 2, 2, 3, 3, 4, 4),
    treatment = c("A", "B", "A", "B", "C", "D", "C", "D")
  )
  disc <- .design_connectedness(dg, "treatment", "block", character(0))
  expect_match(disc$method, "block")
  expect_false(disc$connected)
  expect_gt(disc$n_aliased, 0)

  # a connected block structure
  dg2 <- data.frame(
    block = c(1, 1, 2, 2, 3, 3),
    treatment = c("A", "B", "B", "C", "C", "A")
  )
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
  skipped <- .design_connectedness(
    big,
    "treatment",
    NULL,
    c("row", "col"),
    force = FALSE
  )
  expect_false(skipped$available)
  expect_match(skipped$reason, "large design")

  # Small designs still compute on the auto (NULL) path.
  auto <- summary(simple_design())$per_level[[1]]$evaluation$connectedness
  expect_true(auto$available)

  # connectedness = FALSE skips entirely.
  off <- summary(simple_design(), connectedness = FALSE)$per_level[[
    1
  ]]$evaluation$connectedness
  expect_false(off$available)
  expect_match(off$reason, "connectedness = FALSE")
})

test_that("connectedness does not false-positive when block is collinear with rows", {
  # BIBD laid out one block per row: block aliases row, but treatment is fully
  # estimable. Counting aliasing only among treatment terms must keep it connected.
  bibd <- data.frame(
    row = rep(1:6, each = 2),
    block = rep(1:6, each = 2),
    col = rep(1:2, times = 6),
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
  expect_true(cc$lambda_constant) # BIBD: lambda = 1 for every pair
  expect_equal(cc$lambda_max, 1)

  # Complete blocks (RCBD): every treatment in every block -> uninformative.
  comp <- data.frame(
    block = rep(1:3, each = 4),
    treatment = rep(c("A", "B", "C", "D"), 3)
  )
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
  cc_forced <- summary(block_design(), concurrence = TRUE)$per_level[[
    1
  ]]$evaluation$concurrence
  expect_true(cc_forced$available)
})

test_that(".design_block_factor prefers a block-named factor over others", {
  df <- data.frame(row = 1, col = 1, rep = 1, block = 1, treatment = "A")
  # spatial factors listed with 'rep' before 'block' - block should still win.
  expect_equal(
    .design_block_factor(df, c("row", "col", "rep", "block"), "row", "col"),
    "block"
  )

  # No block-named factor: falls back to the first non-row/col spatial factor.
  expect_equal(
    .design_block_factor(df, c("row", "col", "rep"), "row", "col"),
    "rep"
  )
})

test_that(".design_block_factor is resolved per level, not shared across levels", {
  # wp is blocked by 'block'; sp has no block-like spatial factor of its own and
  # should fall back to the literal "block" column rather than silently reusing
  # wp's resolved factor for a level it doesn't describe.
  df <- data.frame(row = 1, col = 1, block = 1, rep = 1, treatment = "A")
  expect_equal(
    .design_block_factor(df, c("row", "col", "block"), "row", "col"),
    "block"
  )
  expect_equal(
    .design_block_factor(df, c("row", "col", "rep"), "row", "col"),
    "rep"
  )
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

  on <- summary(simple_design(), efficiency = TRUE)$per_level[[
    1
  ]]$evaluation$efficiency
  expect_true(on$available)
  expect_true(is.finite(on$value))

  # Guard: < 3 treatments returns NA with a reason rather than erroring.
  d2 <- data.frame(
    row = rep(1:2, 2),
    col = rep(1:2, each = 2),
    treatment = rep(c("A", "B"), 2)
  )
  two <- .efficiency_factor(d2, "treatment", "row", "col", grid_or_reason(d2))
  expect_false(two$available)
})

test_that("neighbour balance is reported for any grid design, whatever the objective", {
  d <- data.frame(
    row = rep(1:4, times = 3),
    col = rep(1:3, each = 4),
    treatment = rep(LETTERS[1:3], 4)
  )
  r <- speed(
    d,
    swap = "treatment",
    swap_within = "1",
    spatial_factors = ~ row + col,
    obj_function = objective_function_piepho,
    iterations = 200,
    seed = 42,
    quiet = TRUE
  )
  nb <- summary(r)$per_level[[1]]$evaluation$neighbour
  expect_true(nb$available)
  expect_true(is.finite(nb$pair_var))

  # Also on by default for the default objective.
  nb_default <- summary(simple_design())$per_level[[1]]$evaluation$neighbour
  expect_true(nb_default$available)

  # Opt out with neighbour = FALSE.
  nb_off <- summary(simple_design(), neighbour = FALSE)$per_level[[
    1
  ]]$evaluation$neighbour
  expect_false(nb_off$available)
  expect_match(nb_off$reason, "neighbour = FALSE")
})

test_that("neighbour balance separates self-adjacency from distinct-pair counts", {
  # 3 treatments x 4 replicates each on a 4x3 grid gives 6 possible pairs:
  # 3 self-pairs (AA, BB, CC) and 3 distinct pairs (AB, AC, BC). Self-adjacency
  # is reported on its own because zero is the desirable outcome, whereas a
  # distinct pair never neighbouring is an imbalance. calculate_nb()'s own table
  # silently omits pairs it never sees rather than recording a zero, so
  # .neighbour_balance() must fill them in for the counts to be over the full
  # pair universe.
  d <- data.frame(
    row = rep(1:4, times = 3),
    col = rep(1:3, each = 4),
    treatment = rep(LETTERS[1:3], 4)
  )
  r <- speed(
    d,
    swap = "treatment",
    swap_within = "1",
    spatial_factors = ~ row + col,
    obj_function = objective_function_piepho,
    iterations = 200,
    seed = 42,
    quiet = TRUE
  )
  nb <- summary(r)$per_level[[1]]$evaluation$neighbour

  expect_named(
    nb,
    c(
      "available",
      "self_adjacent",
      "n_pairs",
      "min_pair_count",
      "max_pair_count",
      "pair_var",
      "n_zero_pairs"
    )
  )
  expect_equal(nb$n_pairs, 3) # AB, AC, BC - self-pairs excluded
  expect_true(nb$min_pair_count <= nb$max_pair_count)
  expect_true(nb$min_pair_count >= 0)
  expect_true(nb$self_adjacent >= 0)

  # Cross-check by walking the (row, col) coordinates directly. Deliberately NOT
  # matrix(treatment, nrow, ncol): reshaping is what the implementation must
  # avoid, so an expectation built that way would only confirm itself.
  coords <- r$design_df
  rr <- as.numeric(as.character(coords$row))
  cc <- as.numeric(as.character(coords$col))
  trt <- as.character(coords$treatment)
  at <- function(i, j) {
    k <- which(rr == i & cc == j)
    if (length(k) == 1) trt[k] else NA_character_
  }
  pm_levels <- sort(unique(trt))
  counts <- list()
  for (k in seq_along(trt)) {
    for (nb_ij in list(c(rr[k], cc[k] + 1), c(rr[k] + 1, cc[k]))) {
      other <- at(nb_ij[1], nb_ij[2])
      if (!is.na(other)) {
        key <- paste(sort(c(trt[k], other)), collapse = ",")
        prev <- counts[[key]]
        counts[[key]] <- if (is.null(prev)) 1L else prev + 1L
      }
    }
  }
  self_keys <- paste(pm_levels, pm_levels, sep = ",")
  all_pairs <- combn(pm_levels, 2, function(p) paste(p, collapse = ","))
  get <- function(k) if (is.null(counts[[k]])) 0L else counts[[k]]
  self_total <- sum(vapply(self_keys, get, integer(1)))
  pair_counts <- vapply(all_pairs, get, integer(1))

  expect_equal(nb$self_adjacent, self_total)
  expect_equal(nb$min_pair_count, min(pair_counts))
  expect_equal(nb$max_pair_count, max(pair_counts))
  expect_equal(nb$pair_var, var(pair_counts))
  expect_equal(nb$n_zero_pairs, sum(pair_counts == 0))

  # The design is 4x3, so a column-major reshape would disagree - which is the
  # whole point of the coordinate-based grid.
  expect_equal(nb$self_adjacent, calculate_adjacency_score(coords, "treatment"))
})

test_that("a non-zero self-adjacency count is highlighted in the printed output", {
  withr::with_options(list(crayon.enabled = TRUE, cli.num_colors = 8), {
    s <- summary(simple_design())
    s$per_level[[1]]$evaluation$neighbour$self_adjacent <- 4L
    out <- capture_output(print(s))
    expect_match(
      out,
      "Self-adj\\.:\\s+\033\\[35m4 like-treatment adjacencies\033\\[39m"
    )

    s$per_level[[1]]$evaluation$neighbour$self_adjacent <- 0L
    expect_match(capture_output(print(s)), "Self-adj\\.:\\s+none")
  })
})

test_that("the disconnected flag fires and prints", {
  # Build a summary then inject a disconnected verdict to exercise the flag path.
  s <- summary(simple_design())
  s$per_level[[1]]$evaluation$connectedness <-
    list(
      available = TRUE,
      connected = FALSE,
      method = "model (row + col)",
      message = "2 aliased coefficient(s)"
    )
  s$flags$disconnected <- names(s$per_level)
  out <- capture_output(print(s))
  expect_match(out, "DISCONNECTED")
})

test_that("the Evaluation section prints expected metrics", {
  out <- capture_output(print(summary(block_design())))
  expect_match(out, "Evaluation")
  expect_match(out, "Connected:")
  expect_match(out, "Repl. span:")
  # Complete blocks -> concurrence shown as a skip note, no counts.
  expect_match(out, "Concurrence:.*complete blocks")

  # Forcing concurrence prints the min/max concurrence counts.
  out_forced <- capture_output(print(summary(
    block_design(),
    concurrence = TRUE
  )))
  expect_match(out_forced, "Concurrence:\\s+min \\d+, max \\d+")
})

test_that("concurrence = FALSE skips the check even when a block factor exists", {
  # Distinct from the "no block factor" skip: the block is present, the user
  # opted out.
  cc <- summary(block_design(), concurrence = FALSE)$per_level[[
    1
  ]]$evaluation$concurrence

  expect_false(cc$available)
  expect_equal(cc$reason, "not requested (concurrence = FALSE)")
  expect_match(
    capture_output(print(summary(block_design(), concurrence = FALSE))),
    "Concurrence:\\s+not requested \\(concurrence = FALSE\\)"
  )
})

test_that("every opted-out metric prints its reason rather than a value", {
  s <- summary(
    block_design(),
    connectedness = FALSE,
    concurrence = FALSE,
    neighbour = FALSE
  )
  out <- capture_output(print(s))

  expect_match(out, "Connected:\\s+not requested \\(connectedness = FALSE\\)")
  expect_match(out, "Neighbour:\\s+not requested \\(neighbour = FALSE\\)")
})

test_that("an opted-in efficiency factor is printed with its value", {
  out <- capture_output(print(summary(simple_design(), efficiency = TRUE)))
  expect_match(
    out,
    "Efficiency:\\s+[0-9.]+ \\(A-efficiency, row-column model\\)"
  )
})

test_that("a fully unreplicated design reports no replicate spans", {
  # 12 treatments on 12 plots: spans are available but there is nothing to
  # measure, which is distinct from spans being unavailable.
  d <- data.frame(
    row = rep(1:4, times = 3),
    col = rep(1:3, each = 4),
    treatment = LETTERS[1:12]
  )
  r <- speed(
    d,
    swap = "treatment",
    spatial_factors = ~ row + col,
    iterations = 50,
    seed = 1,
    quiet = TRUE
  )
  rs <- summary(r)$per_level[[1]]$evaluation$replicate_span

  expect_true(rs$available)
  expect_equal(rs$n_replicated, 0)
  expect_true(is.na(rs$min_row_span))
  expect_true(is.na(rs$min_col_span))
  expect_match(
    capture_output(print(summary(r))),
    "Repl\\. span:\\s+n/a \\(no replicated treatments\\)"
  )
})

test_that("designs without a row/column grid summarise and print without a grid", {
  # No inferrable row/col and no grid_factors: the grid-dependent metrics report
  # a reason, and the layout line drops the rows x cols part.
  d <- data.frame(
    a = rep(1:4, times = 3),
    b = rep(1:3, each = 4),
    treatment = rep(LETTERS[1:3], 4)
  )
  expect_warning(
    r <- speed(
      d,
      swap = "treatment",
      spatial_factors = ~ a + b,
      iterations = 50,
      seed = 1,
      quiet = TRUE
    ),
    "Cannot infer row"
  )
  s <- summary(r)

  expect_false(s$layout$has_grid)
  expect_true(is.na(s$layout$nrow))
  expect_true(is.na(s$layout$ncol))

  e <- s$per_level[[1]]$evaluation
  expect_false(e$neighbour$available)
  expect_equal(e$neighbour$reason, "no row/column factors")
  expect_false(e$replicate_span$available)
  expect_equal(e$replicate_span$reason, "no row/column factors")

  out <- capture_output(print(s))
  expect_match(out, "Layout:\\s+12 plots")
  expect_no_match(out, "rows x")
  expect_match(out, "Repl\\. span:\\s+no row/column factors")
  expect_match(out, "Neighbour:\\s+no row/column factors")
})

test_that("grid_index() conditions carry the reason summary() reports", {
  # summary() reports `reason` in place of a metric it cannot compute, so every
  # condition class must supply one.
  ok <- data.frame(row = c(1, 1, 2, 2), col = c(1, 2, 1, 2), treatment = "A")
  expect_false(is.character(grid_or_reason(ok)))

  # A genuine hole (missing plot) is still a single grid - coordinates stay
  # unique, so the metrics must not refuse it.
  expect_false(is.character(grid_or_reason(ok[-2, ])))

  # Factor coordinates whose level order is lexical are still fine.
  lex <- data.frame(
    row = factor(c(1, 2, 10)),
    col = factor(c(1, 1, 1)),
    treatment = "A"
  )
  expect_false(is.character(grid_or_reason(lex)))

  expect_equal(grid_or_reason(ok, "nope"), "no row/column factors")
  expect_match(
    grid_or_reason(data.frame(row = c("A", "B"), col = c(1, 1))),
    "labels are not numeric"
  )
  expect_match(
    grid_or_reason(data.frame(row = c(0, 1), col = c(1, 1))),
    "not positive whole numbers"
  )
  expect_match(
    grid_or_reason(ok[c(1, 1, 2), ]),
    "duplicate .row./.col. coordinates"
  )
})

test_that("multi-site (MET) designs summarise instead of erroring", {
  # initialise_design_df(designs = ) reuses row/col per site, so a MET design has
  # duplicate coordinates and cannot be placed on one grid. Every grid metric is
  # wrong for it (they pool sites that share no edge), so all must report a
  # reason. Before this gate, summary() errored outright.
  met <- initialise_design_df(
    items = rep(LETTERS[1:3], 6),
    designs = list(
      a = list(nrows = 3, ncols = 2),
      b = list(nrows = 3, ncols = 4)
    )
  )
  expect_true(anyDuplicated(met[c("row", "col")]) > 0)

  r <- speed(
    met,
    swap = "treatment",
    swap_within = "site",
    spatial_factors = ~ row + col + site,
    optimise_params = optim_params(adj_weight = 0),
    iterations = 50,
    seed = 1,
    quiet = TRUE
  )
  s <- expect_no_error(summary(r, efficiency = TRUE))

  # No single nrow x ncol describes two grids of different shapes.
  expect_false(s$layout$has_grid)
  expect_match(s$layout$grid_reason, "multi-site")
  expect_true(is.na(s$layout$nrow))
  expect_equal(s$layout$n_plots, 18)

  e <- s$per_level[[1]]$evaluation
  for (metric in c("neighbour", "replicate_span", "efficiency")) {
    expect_false(e[[metric]]$available, info = metric)
    expect_match(e[[metric]]$reason, "duplicate", info = metric)
  }

  # Non-grid metrics still work - only the grid ones are withheld.
  expect_equal(s$per_level[[1]]$n_treatments, 3)

  out <- capture_output(print(s))
  expect_match(out, "Layout:\\s+18 plots")
  expect_no_match(out, "rows x")
  expect_match(out, "Neighbour:\\s+duplicate")
})

test_that("efficiency is withheld rather than reported above 1 for MET designs", {
  # An efficiency factor is a property of one experiment's information matrix,
  # so a multi-site frame has no single answer. Nothing about the arithmetic
  # stops it being computed - duplicate coordinates just pool the sites into one
  # row/column model - so the refusal has to be explicit. Before it, this
  # returned a value above 1, impossible for an efficiency factor.
  met <- initialise_design_df(
    items = rep(LETTERS[1:3], 6),
    designs = list(
      a = list(nrows = 3, ncols = 2),
      b = list(nrows = 3, ncols = 4)
    )
  )
  expect_error(
    calculate_efficiency_factor(met, treatment),
    class = "speed_grid_duplicate"
  )
  gate <- .efficiency_factor(
    met,
    "treatment",
    "row",
    "col",
    grid_or_reason(met)
  )
  expect_false(gate$available)
  expect_match(gate$reason, "duplicate")
})

test_that("a MET design reports one efficiency per site and never a pooled one", {
  set.seed(1)
  d <- rbind(
    data.frame(
      site = "a",
      expand.grid(row = 1:4, col = 1:3),
      treatment = sample(rep(LETTERS[1:6], 2))
    ),
    data.frame(
      site = "b",
      expand.grid(row = 1:4, col = 1:3),
      treatment = sample(rep(LETTERS[1:6], 2))
    )
  )
  r <- speed(
    d,
    swap = "treatment",
    swap_within = "site",
    spatial_factors = ~ row + col + site,
    grid_factors = list(dim1 = "row", dim2 = "col", by = "site"),
    iterations = 100,
    seed = 42,
    quiet = TRUE
  )
  s <- summary(r, efficiency = TRUE)

  # The grouping is recorded, not guessed from the column name.
  expect_equal(r$metadata$grid_by, "site")
  expect_false(s$layout$has_grid)
  expect_equal(s$layout$n_grids, 2L)
  expect_match(s$layout$grid_reason, "2 grids")
  # nrow/ncol stay NA: no single shape describes a design spanning two grids.
  expect_true(is.na(s$layout$nrow))

  ef <- s$per_level[[1]]$evaluation$efficiency
  expect_named(ef$per_grid, c("a", "b"))
  expect_equal(ef$grid_by, "site")
  expect_null(ef$value) # no pooled value, ever
  for (one in ef$per_grid) {
    expect_true(one$available)
    expect_lte(one$value, 1)
  }
  # Each site's value is that site scored on its own.
  expect_equal(
    ef$per_grid$a$value,
    calculate_efficiency_factor(
      r$design_df[r$design_df$site == "a", ],
      treatment
    )
  )

  out <- capture_output(print(s))
  expect_match(out, "per `site`")

  # Spans pool sites if computed naively - two sites' row 3 are not one plot
  # apart - so they are withheld with a reason instead.
  spans <- s$per_level[[1]]$evaluation$replicate_span
  expect_false(spans$available)
  expect_match(spans$reason, "spans 2 grids")
})

test_that("a site that cannot support an efficiency factor reports only for itself", {
  # Site b puts each treatment in one column, so its treatment contrasts are
  # confounded with the column effects. That must not withhold site a's value.
  d <- rbind(
    data.frame(
      site = "a",
      expand.grid(row = 1:4, col = 1:3),
      treatment = c("A", "B", "C", "D", "C", "D", "A", "B", "B", "A", "D", "C")
    ),
    data.frame(
      site = "b",
      expand.grid(row = 1:4, col = 1:3),
      treatment = rep(c("A", "B", "C"), each = 4)
    )
  )
  grid <- grid_or_reason(d, by = "site")
  ef <- .efficiency_factor(d, "treatment", "row", "col", grid)

  expect_true(ef$per_grid$a$available)
  expect_false(ef$per_grid$b$available)
  expect_match(ef$per_grid$b$reason, "not estimable")
  # `available` is TRUE overall because at least one site reported.
  expect_true(ef$available)
})

test_that("neighbour balance sums across grids rather than pooling them", {
  set.seed(3)
  d <- rbind(
    data.frame(
      site = "a",
      expand.grid(row = 1:4, col = 1:3),
      treatment = sample(rep(LETTERS[1:6], 2))
    ),
    data.frame(
      site = "b",
      expand.grid(row = 1:4, col = 1:3),
      treatment = sample(rep(LETTERS[1:6], 2))
    )
  )
  both <- .neighbour_balance(
    d,
    "treatment",
    "row",
    "col",
    grid_or_reason(d, by = "site")
  )
  expect_true(both$available)

  # The self-adjacency count is an edge count, so it is exactly the sum of the
  # per-site counts - no edge crosses a site boundary.
  per_site <- vapply(
    split(d, d$site),
    function(s) {
      return(.neighbour_balance(
        s,
        "treatment",
        "row",
        "col",
        grid_or_reason(s)
      )$self_adjacent)
    },
    numeric(1)
  )
  expect_equal(both$self_adjacent, sum(per_site))
})

test_that("non-numeric row/col labels are reported, not coerced silently", {
  d <- data.frame(
    row = rep(c("A", "B", "C"), each = 4),
    col = rep(c("w", "x", "y", "z"), 3),
    treatment = rep(LETTERS[1:4], 3)
  )
  r <- speed(
    d,
    swap = "treatment",
    spatial_factors = ~ row + col,
    optimise_params = optim_params(adj_weight = 0),
    iterations = 50,
    seed = 1,
    quiet = TRUE
  )

  # Previously this errored from build_design_matrix(), and .replicate_spans()
  # leaked two "NAs introduced by coercion" warnings on the way.
  s <- expect_no_warning(expect_no_error(summary(r)))

  expect_false(s$layout$has_grid)
  e <- s$per_level[[1]]$evaluation
  expect_match(e$neighbour$reason, "labels are not numeric")
  expect_match(e$replicate_span$reason, "labels are not numeric")
})

test_that("split-plot designs keep their grid metrics at every level", {
  # The gate must not withhold metrics from a legitimate hierarchical design:
  # a split-plot shares one grid, and both levels' plots have unique
  # coordinates, so nothing here spans multiple grids.
  s <- summary(split_plot_design(), efficiency = TRUE)

  expect_true(s$layout$has_grid)
  expect_true(is.na(s$layout$grid_reason))
  expect_equal(c(s$layout$nrow, s$layout$ncol), c(6, 4))

  expect_named(s$per_level, c("wp", "sp"))
  for (lv in c("wp", "sp")) {
    e <- s$per_level[[lv]]$evaluation
    expect_true(e$neighbour$available, info = lv)
    expect_true(e$replicate_span$available, info = lv)
  }
})

test_that("the disconnected flag names the affected levels for hierarchical designs", {
  s <- summary(split_plot_design())
  s$flags$disconnected <- c("wp", "sp")

  expect_match(capture_output(print(s)), "DISCONNECTED design \\(wp, sp\\)")
})

test_that(".drop_buffer_rows is a no-op without per-level metadata", {
  df <- data.frame(row = 1:3, col = 1:3, treatment = c("A", "B", "C"))
  expect_identical(.drop_buffer_rows(df, list()), df)
})

test_that("buffer removal drops the unused factor level from a factor swap column", {
  # add_buffers() adds a "buffer" level; once the rows go, the level must go too
  # or downstream table()/matrix() work counts a treatment that isn't there.
  d <- data.frame(
    row = rep(1:4, times = 3),
    col = rep(1:3, each = 4),
    treatment = factor(rep(LETTERS[1:3], 4))
  )
  r <- speed(
    d,
    swap = "treatment",
    spatial_factors = ~ row + col,
    iterations = 50,
    seed = 1,
    quiet = TRUE
  )
  rb <- add_buffers(r, "edge")
  expect_true(is.factor(rb$design_df$treatment))
  expect_true("buffer" %in% levels(rb$design_df$treatment))

  dropped <- .drop_buffer_rows(rb$design_df, rb$metadata)
  expect_setequal(levels(dropped$treatment), c("A", "B", "C"))
  expect_equal(nrow(dropped), 12)

  # Replication counts come from table(), so a stray level would show up as 0.
  s <- summary(rb)
  expect_equal(s$per_level[[1]]$n_treatments, 3)
  expect_true(s$per_level[[1]]$replication$equal)
  expect_equal(s$per_level[[1]]$replication$min, 4)
})

test_that("evaluation helpers return a reason instead of erroring on unmet assumptions", {
  no_grid <- data.frame(a = 1:3, treatment = c("A", "B", "C"))

  ng <- grid_or_reason(no_grid)
  rs <- .replicate_spans(no_grid, "treatment", "row", "col", ng)
  expect_false(rs$available)
  expect_equal(rs$reason, "no row/column factors")

  ef <- .efficiency_factor(no_grid, "treatment", "row", "col", ng)
  expect_false(ef$available)
  expect_equal(ef$reason, "no row/column factors")

  # A contrast needs two treatments to be a contrast.
  one <- .design_connectedness(
    data.frame(treatment = rep("A", 3)),
    "treatment",
    NULL,
    character(0)
  )
  expect_false(one$available)
  expect_equal(one$reason, "needs >= 2 treatments")

  # Guarded against directly, even though summary() never reaches it.
  cc <- .design_concurrence(
    data.frame(treatment = c("A", "B")),
    "treatment",
    NULL
  )
  expect_false(cc$available)
  expect_equal(cc$reason, "no block factor")
})

test_that(".design_connectedness is trivially connected with no nuisance factors", {
  cn <- .design_connectedness(
    data.frame(treatment = c("A", "B", "C")),
    "treatment",
    NULL,
    character(0)
  )

  expect_true(cn$available)
  expect_equal(cn$method, "none")
  expect_true(cn$connected)
  expect_equal(cn$n_aliased, 0L)
  expect_match(cn$message, "trivially connected")
})

test_that(".efficiency_factor reports a reason when the computation fails", {
  # The wrapper must absorb an error from the underlying metric rather than
  # propagate it. Mocked rather than provoked with a degenerate design, because a
  # design degenerate enough to fail the metric (e.g. a 1x1 grid, three plots
  # sharing one coordinate) is rejected by grid_index() first, leaving this
  # backstop untested.
  d <- data.frame(
    row = rep(1:2, 3),
    col = rep(1:3, each = 2),
    treatment = rep(c("A", "B", "C"), 2)
  )
  grid <- grid_or_reason(d)
  expect_false(is.character(grid))

  local_mocked_bindings(
    calculate_efficiency_factor = function(...) stop("cannot compute")
  )
  ef <- .efficiency_factor(d, "treatment", "row", "col", grid)

  expect_false(ef$available)
  expect_equal(ef$reason, "could not be computed for this design")
})

test_that("neighbour balance reads coordinates, not the data frame order", {
  # A 3x8 design optimised to a genuine zero self-adjacency. A column-major
  # reshape of a row-major frame scrambles it and invents adjacencies, so this
  # is the regression test for that class of bug: the reported figure must
  # agree with the objective's own adjacency score for the same design.
  d <- speed(
    initialise_design_df(items = rep(LETTERS[1:6], 4), nrows = 3, ncols = 8),
    swap = "treatment",
    iterations = 3000,
    seed = 11,
    quiet = TRUE
  )
  nb <- summary(d)$per_level[[1]]$evaluation$neighbour

  expect_equal(nb$self_adjacent, 0)
  expect_equal(
    nb$self_adjacent,
    calculate_adjacency_score(d$design_df, "treatment")
  )

  # Row order must not matter: the same design with its rows reversed is the
  # same field layout, and must report the same neighbour balance.
  reversed <- d
  reversed$design_df <- d$design_df[rev(seq_len(nrow(d$design_df))), ]
  expect_equal(
    summary(reversed)$per_level[[1]]$evaluation$neighbour,
    nb
  )
})
