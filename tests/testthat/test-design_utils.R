test_that("generate_multi_swap_neighbour only exchanges equally replicated treatments", {
  # A and B have three plots each, C has one, so C cannot be exchanged with anything
  # without changing the replication of the design
  design <- data.frame(
    block = factor(rep(1, 7)),
    treatment = factor(c("A", "A", "A", "B", "B", "B", "C"))
  )

  set.seed(42)
  for (i in 1:50) {
    result <- generate_multi_swap_neighbour(
      design,
      "treatment",
      "block",
      1,
      FALSE
    )

    expect_equal(as.integer(table(result$design$treatment)), c(3L, 3L, 1L))
    expect_equal(as.character(result$design$treatment[7]), "C")
  }
})

test_that("generate_multi_swap_neighbour makes no swap when no replication is shared", {
  design <- data.frame(
    block = factor(rep(1, 6)),
    treatment = factor(c("A", "B", "B", "C", "C", "C"))
  )

  set.seed(42)
  for (i in 1:20) {
    result <- generate_multi_swap_neighbour(
      design,
      "treatment",
      "block",
      1,
      FALSE
    )

    expect_equal(result$design, design)
  }
})

test_that("generate_multi_swap_neighbour swaps as before when replication is equal", {
  design <- data.frame(
    block = factor(rep(1, 6)),
    treatment = factor(c("A", "A", "B", "B", "C", "C"))
  )

  set.seed(1)
  result <- generate_multi_swap_neighbour(
    design,
    "treatment",
    "block",
    1,
    FALSE
  )

  expect_false(identical(
    as.character(result$design$treatment),
    as.character(design$treatment)
  ))
  expect_equal(as.integer(table(result$design$treatment)), c(2L, 2L, 2L))
})

test_that("generate_multi_swap_neighbour restricts the pool per group, not per design", {
  # Group 1 is unbalanced and group 2 is not; the swap in group 2 must still happen
  design <- data.frame(
    block = factor(rep(1:2, each = 4)),
    treatment = factor(c("A", "A", "A", "B", "C", "C", "D", "D"))
  )

  set.seed(3)
  result <- generate_multi_swap_neighbour(design, "treatment", "block", 1, TRUE)

  expect_equal(as.integer(table(result$design$treatment)), c(3L, 1L, 2L, 2L))
  # Group 1 holds no exchangeable pair, so it is left alone
  expect_equal(
    as.character(result$design$treatment[1:4]),
    c("A", "A", "A", "B")
  )
  # Group 2 is balanced, so C and D are exchanged
  expect_equal(
    as.character(result$design$treatment[5:8]),
    c("D", "D", "C", "C")
  )
})

# `block` and `site` cut across each other, so swaps at the block level can leave a
# site unbalanced part way through the search even though the input passes
# `.verify_swap_all_replication()`
cross_cutting_df <- function() {
  return(data.frame(
    row = rep(1:6, times = 2),
    col = rep(1:2, each = 6),
    block = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
    site = c("a", "a", "a", "b", "b", "b", "a", "a", "a", "b", "b", "b"),
    lines = c("X", "X", "Z", "Y", "Y", "Z", "Y", "Y", "Z", "X", "X", "Z"),
    stringsAsFactors = FALSE
  ))
}

test_that("swap_all preserves replication when levels have cross-cutting groups", {
  df <- cross_cutting_df()

  for (seed in 1:5) {
    # Some seeds leave a site with no exchangeable pair, which warns; that is covered
    # separately below
    result <- suppressWarnings(speed(
      df,
      swap = "lines",
      optimise = list(
        lvl1 = list(swap_within = "block", swap_all = TRUE),
        lvl2 = list(swap_within = "site", swap_all = TRUE)
      ),
      iterations = 30,
      seed = seed,
      quiet = TRUE
    ))

    expect_equal(as.integer(table(result$design_df$lines)), c(4L, 4L, 4L))
  }
})

test_that("swappable_groups separates unequal replication from other blockers", {
  design <- data.frame(
    # g1: A/B/C replicated 3/2/1, so `swap_all` can exchange no pair
    # g2: equal replication, exchangeable
    # g3: a single treatment, unswappable but unremarkable
    block = factor(rep(c("g1", "g2", "g3"), each = 6)),
    treatment = factor(c(
      "A", "A", "A", "B", "B", "C",
      "A", "A", "B", "B", "C", "C",
      "A", "A", "A", "A", "A", "A"
    ))
  )

  all_swap <- swappable_groups(design, "treatment", "block", swap_all = TRUE)
  expect_equal(all_swap$swappable, "g2")
  expect_equal(all_swap$unequal_replication, "g1")

  # Without `swap_all` a single pair of plots moves, so replication is irrelevant
  # and only the single-treatment group is stuck
  single <- swappable_groups(design, "treatment", "block", swap_all = FALSE)
  expect_equal(single$swappable, c("g1", "g2"))
  expect_length(single$unequal_replication, 0)
})

test_that("swappable_groups counts a level with no plots as unswappable", {
  # A factor carrying a level the data no longer uses, e.g. a subset of a MET
  design <- data.frame(
    site = factor(rep(c("a", "b"), each = 4), levels = c("a", "b", "c")),
    treatment = factor(c("A", "A", "B", "B", "A", "A", "B", "B"))
  )

  result <- swappable_groups(design, "treatment", "site", swap_all = TRUE)
  expect_equal(result$swappable, c("a", "b"))
  expect_length(result$unequal_replication, 0)
})

test_that("speed() warns when a swap group is left frozen mid-search", {
  # A level 1 swap can leave a site with no two treatments sharing a replication
  # count. The search cannot move anything there, which should be reported rather
  # than returned silently.
  df <- cross_cutting_df()

  expect_warning(
    speed(
      df,
      swap = "lines",
      optimise = list(
        lvl1 = list(swap_within = "block", swap_all = TRUE),
        lvl2 = list(swap_within = "site", swap_all = TRUE)
      ),
      iterations = 30,
      seed = 4,
      quiet = TRUE
    ),
    "No treatments could be swapped at level 'lvl2' within 'site'",
    fixed = TRUE
  )
})

test_that("speed() stops a level once every swap group is frozen", {
  # As above, but the level 1 swaps leave both sites frozen. Nothing can move at
  # level 2 from then on, so it should give up rather than run out its iterations.
  df <- cross_cutting_df()

  run <- function(d) {
    return(suppressWarnings(speed(
      d,
      swap = "lines",
      optimise = list(
        lvl1 = list(swap_within = "block", swap_all = TRUE, iterations = 20),
        lvl2 = list(swap_within = "site", swap_all = TRUE, iterations = 500)
      ),
      early_stop_iterations = 500,
      optimise_params = optim_params(stop_at_optimal = FALSE),
      seed = 2,
      quiet = TRUE
    )))
  }

  result <- run(df)
  expect_true(result$stopped_early[["lvl2"]])
  # Settled before the first swap is proposed, so only the starting score is kept
  expect_length(result$scores$lvl2, 1)
  expect_equal(result$metadata$per_level$lvl2$stop_reason, "frozen")

  # Neither an unused factor level, e.g. `site` subset from a larger trial, nor
  # a site holding a single treatment gives the level anything else to move
  unused_level <- df
  unused_level$site <- factor(df$site, levels = c("a", "b", "c"))
  expect_length(run(unused_level)$scores$lvl2, 1)

  single_treatment <- rbind(
    df,
    data.frame(
      row = rep(7:8, times = 2), col = rep(1:2, each = 2),
      block = 3, site = "c", lines = "W",
      stringsAsFactors = FALSE
    )
  )
  expect_length(run(single_treatment)$scores$lvl2, 1)

  # A frozen level says so unless silenced
  expect_output(
    suppressWarnings(speed(
      df,
      swap = "lines",
      optimise = list(
        lvl1 = list(swap_within = "block", swap_all = TRUE, iterations = 20),
        lvl2 = list(swap_within = "site", swap_all = TRUE, iterations = 500)
      ),
      early_stop_iterations = 500,
      optimise_params = optim_params(stop_at_optimal = FALSE),
      seed = 2,
      quiet = FALSE
    )),
    "No swaps possible for level lvl2"
  )
})

test_that(".warn_unequal_replication names one group and counts the rest", {
  expect_silent(.warn_unequal_replication(character(0), "lvl1", "site"))

  expect_warning(
    .warn_unequal_replication("a", "lvl1", "site"),
    "at level 'lvl1' within 'site' group a, because",
    fixed = TRUE
  )

  expect_warning(
    .warn_unequal_replication(c("a", "b", "c"), "lvl1", "site"),
    "group a, and in 2 other group(s), because",
    fixed = TRUE
  )
})

test_that("a level searches from the best design found, not the last accepted", {
  # Annealing usually ends above its own best, so the last accepted design and
  # `best_design` - the one returned - differ. A level starting from the former
  # optimises and diagnoses a design the caller never sees: here the returned
  # design leaves both sites with no exchangeable pair, which went unreported.
  df <- cross_cutting_df()
  df$plot_id <- sprintf("P%02d", seq_len(nrow(df)))

  run <- function() {
    return(speed(
      df,
      swap = "lines",
      optimise = list(
        lvl1 = list(swap_within = "block", swap_all = TRUE),
        lvl2 = list(swap_within = "site", swap_all = TRUE)
      ),
      linked_cols = "plot_id",
      iterations = 30,
      seed = 1,
      quiet = TRUE
    ))
  }

  expect_warning(
    run(),
    "No treatments could be swapped at level 'lvl2' within 'site'",
    fixed = TRUE
  )

  result <- suppressWarnings(run())
  expect_equal(result$metadata$per_level$lvl2$stop_reason, "frozen")

  # The design handed back is the one the level was reported on
  final <- result$design_df
  final$site <- as.factor(final$site)
  groups <- swappable_groups(final, "lines", "site", swap_all = TRUE)
  expect_length(groups$swappable, 0)
  expect_equal(groups$unequal_replication, c("a", "b"))
})

test_that("swapped_items are treatment labels, not factor codes", {
  # The swap column is a factor throughout the search, and a factor put into a
  # character vector contributes its integer codes. `objective_function_piepho()`
  # matches `swapped_items` against the design matrix, so codes silently mask
  # every plot out of its incremental update.
  design <- data.frame(
    row = 1:8,
    col = 1,
    block = factor(rep(c("g1", "g2"), each = 4)),
    treatment = factor(c("A", "A", "B", "B", "C", "C", "D", "D"))
  )

  for (swap_all in c(FALSE, TRUE)) {
    withr::with_seed(3, {
      result <- generate_neighbour(
        design,
        "treatment",
        "block",
        swap_all = swap_all,
        swap_all_blocks = TRUE
      )
    })

    swapped <- result$swapped_items[nzchar(result$swapped_items)]
    expect_true(all(swapped %in% levels(design$treatment)))
  }
})

test_that("a level ending on a score plateau returns a random point on it", {
  # A whole plot spanning a full row makes its like-adjacencies irreducible, so
  # every arrangement without like-neighbouring rows ties, the input included.
  # The input is as valid an answer as any other tied design, so the test is not
  # that it is avoided but that it is not privileged: were the best design only
  # replaced on a strict improvement, every seed would return the input.
  df <- data.frame(
    row = rep(1:12, each = 4),
    col = rep(1:4, times = 12),
    block = rep(1:4, each = 12),
    wholeplot_treatment = rep(rep(LETTERS[1:3], each = 4), times = 4)
  )

  run <- function(seed) {
    return(speed(
      df,
      swap = "wholeplot_treatment",
      swap_within = "block",
      iterations = 2000,
      early_stop_iterations = 1000,
      swap_all = TRUE,
      seed = seed,
      quiet = TRUE
    ))
  }

  results <- lapply(1:4, run)

  # Every run sits on the plateau it started on
  for (result in results) {
    expect_equal(result$score, result$scores[1])
    # A tie is not an improvement, so it must not keep the level running
    expect_equal(result$metadata$per_level[[1]]$stop_reason, "no_improvement")
  }

  # ... but the seed decides which tied arrangement comes back
  layouts <- lapply(results, function(r) return(r$design_df$wholeplot_treatment))
  expect_gt(length(unique(layouts)), 1)
})

test_that("speed() records why each level stopped", {
  df <- data.frame(
    row = rep(1:6, times = 2),
    col = rep(1:2, each = 6),
    block = rep(c(1, 2), each = 6),
    trt = rep(c("A", "B", "C"), times = 4)
  )

  optimal <- speed(df, swap = "trt", swap_within = "block", seed = 1, quiet = TRUE)
  expect_equal(optimal$metadata$per_level[[1]]$stop_reason, "optimal")

  # No lower bound to stop at, and too few iterations to run out of improvements
  capped <- speed(
    df,
    swap = "trt",
    swap_within = "block",
    iterations = 5,
    optimise_params = optim_params(stop_at_optimal = FALSE),
    seed = 1,
    quiet = TRUE
  )
  expect_equal(capped$metadata$per_level[[1]]$stop_reason, "iterations")
  expect_length(capped$scores, 5)

  no_improvement <- speed(
    df,
    swap = "trt",
    swap_within = "block",
    iterations = 200,
    early_stop_iterations = 2,
    optimise_params = optim_params(stop_at_optimal = FALSE),
    seed = 1,
    quiet = TRUE
  )
  expect_equal(
    no_improvement$metadata$per_level[[1]]$stop_reason,
    "no_improvement"
  )
})

test_that("speed() calls a score of zero optimal without a derived lower bound", {
  df <- data.frame(
    row = rep(1:4, times = 5),
    col = rep(1:5, each = 4),
    treatment = rep(LETTERS[1:4], 5),
    stringsAsFactors = FALSE
  )

  # A custom objective, so no lower bound can be derived and `optimal_score` is
  # `NA`. Reaching zero is still optimal rather than merely out of improvements.
  obj_runs <- function(layout_df, swap, spatial_cols, ...) {
    trt <- as.character(layout_df[[swap]])
    return(list(score = as.numeric(sum(trt[-1] == trt[-length(trt)]))))
  }

  result <- speed(
    df,
    swap = "treatment",
    obj_function = obj_runs,
    iterations = 5000,
    seed = 3,
    quiet = TRUE
  )

  expect_equal(result$score, 0)
  expect_true(is.na(result$metadata$per_level[[1]]$optimal_score))
  expect_equal(result$metadata$per_level[[1]]$stop_reason, "optimal")
})

test_that("speed() does not warn when every group can swap", {
  df <- data.frame(
    row = rep(1:6, times = 2),
    col = rep(1:2, each = 6),
    block = rep(c(1, 2), each = 6),
    trt = rep(c("A", "B", "C"), times = 4)
  )

  expect_no_warning(
    speed(
      df,
      swap = "trt",
      swap_within = "block",
      swap_all = TRUE,
      iterations = 30,
      seed = 1,
      quiet = TRUE
    )
  )
})
