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

test_that("generate_multi_swap_neighbour reports groups it could not swap in", {
  # A/B/C replicated 3/2/1, so no two treatments can be exchanged
  design <- data.frame(
    block = factor(rep("g1", 6)),
    treatment = factor(c("A", "A", "A", "B", "B", "C"))
  )

  result <- generate_multi_swap_neighbour(
    design,
    "treatment",
    "block",
    1,
    FALSE
  )

  expect_equal(result$frozen, "g1")
  expect_equal(result$design, design)
})

test_that("generate_multi_swap_neighbour reports no frozen groups when swaps are possible", {
  design <- data.frame(
    block = factor(rep("g1", 6)),
    treatment = factor(c("A", "A", "B", "B", "C", "C"))
  )

  result <- generate_multi_swap_neighbour(
    design,
    "treatment",
    "block",
    1,
    FALSE
  )

  expect_length(result$frozen, 0)
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

  result <- suppressWarnings(speed(
    df,
    swap = "lines",
    optimise = list(
      lvl1 = list(swap_within = "block", swap_all = TRUE, iterations = 20),
      lvl2 = list(swap_within = "site", swap_all = TRUE, iterations = 500)
    ),
    early_stop_iterations = 500,
    optimise_params = optim_params(stop_at_optimal = FALSE),
    seed = 2,
    quiet = TRUE
  ))

  expect_true(result$stopped_early[["lvl2"]])
  expect_lt(length(result$scores$lvl2), 500)
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
