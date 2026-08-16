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

test_that("swap_all preserves replication when levels have cross-cutting groups", {
  # `block` and `site` cut across each other, so swaps at the block level can leave a
  # site unbalanced part way through the search even though the input passes
  # `.verify_swap_all_replication()`
  df <- data.frame(
    row = rep(1:6, times = 2),
    col = rep(1:2, each = 6),
    block = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
    site = c("a", "a", "a", "b", "b", "b", "a", "a", "a", "b", "b", "b"),
    lines = c("X", "X", "Z", "Y", "Y", "Z", "Y", "Y", "Z", "X", "X", "Z"),
    stringsAsFactors = FALSE
  )

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
  # `block` and `site` cut across each other, so a level 1 swap can leave a site with
  # no two treatments sharing a replication count. The search cannot move anything
  # there, which should be reported rather than returned silently.
  df <- data.frame(
    row = rep(1:6, times = 2),
    col = rep(1:2, each = 6),
    block = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
    site = c("a", "a", "a", "b", "b", "b", "a", "a", "a", "b", "b", "b"),
    lines = c("X", "X", "Z", "Y", "Y", "Z", "Y", "Y", "Z", "X", "X", "Z"),
    stringsAsFactors = FALSE
  )

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
    "No treatments could be swapped at level `lvl2` within `site`",
    fixed = TRUE
  )
})

test_that("speed() stops a level once every swap group is frozen", {
  # As above, but the level 1 swaps leave both sites frozen. Nothing can move at
  # level 2 from then on, so it should give up rather than run out its iterations.
  df <- data.frame(
    row = rep(1:6, times = 2),
    col = rep(1:2, each = 6),
    block = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
    site = c("a", "a", "a", "b", "b", "b", "a", "a", "a", "b", "b", "b"),
    lines = c("X", "X", "Z", "Y", "Y", "Z", "Y", "Y", "Z", "X", "X", "Z"),
    stringsAsFactors = FALSE
  )

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
