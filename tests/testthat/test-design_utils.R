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
    result <- speed(
      df,
      swap = "lines",
      optimise = list(
        lvl1 = list(swap_within = "block", swap_all = TRUE),
        lvl2 = list(swap_within = "site", swap_all = TRUE)
      ),
      iterations = 30,
      seed = seed,
      quiet = TRUE
    )

    expect_equal(as.integer(table(result$design_df$lines)), c(4L, 4L, 4L))
  }
})
