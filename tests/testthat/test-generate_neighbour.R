# The generators are called directly rather than through speed() so the group a
# swap lands in is deterministic, instead of depending on what sample() picks.

# Every group holds a single treatment, so no swap is ever possible.
uniform_groups <- data.frame(
  row = rep(1:4, each = 2),
  col = rep(1:2, 4),
  block = factor(rep(1:2, each = 4)),
  treatment = factor(rep(c("A", "B"), each = 4))
)

# Both treatments in both groups, so every group can swap.
mixed_groups <- data.frame(
  row = rep(1:4, each = 2),
  col = rep(1:2, 4),
  block = factor(rep(1:2, each = 4)),
  treatment = factor(rep(c("A", "B"), 4))
)

# A partially replicated group: A and B appear twice, C and D three times, so a
# replication class must be chosen before the pair is drawn.
prep_group <- data.frame(
  row = rep(1:5, each = 2),
  col = rep(1:2, 5),
  block = factor(rep(1, 10)),
  treatment = factor(c("A", "A", "B", "B", "C", "C", "C", "D", "D", "D"))
)

test_that("a single swap is abandoned when the group holds one treatment", {
  # No different treatment exists to substitute, so it must be a no-op rather
  # than writing a corrupted pair back into the design.
  res <- generate_single_swap_neighbour(
    uniform_groups,
    "treatment",
    "block",
    1,
    FALSE
  )

  expect_identical(res$design$treatment, uniform_groups$treatment)
  expect_true(all(res$swapped_items == ""))
})

test_that("a single swap still proceeds when the group has two treatments", {
  res <- generate_single_swap_neighbour(
    mixed_groups,
    "treatment",
    "block",
    1,
    FALSE
  )

  expect_equal(sum(res$swapped_items != ""), 2)
  # A swap rearranges the design; it never changes which treatments are present.
  expect_equal(table(res$design$treatment), table(mixed_groups$treatment))
})

test_that("swap_all_blocks swaps in every group, not just one", {
  all_blocks <- generate_multi_swap_neighbour(
    mixed_groups,
    "treatment",
    "block",
    1,
    TRUE
  )
  one_block <- generate_multi_swap_neighbour(
    mixed_groups,
    "treatment",
    "block",
    1,
    FALSE
  )

  # Two items recorded per swap, so both blocks give 4 against one block's 2.
  expect_equal(sum(all_blocks$swapped_items != ""), 4)
  expect_equal(sum(one_block$swapped_items != ""), 2)
  expect_equal(
    table(all_blocks$design$treatment),
    table(mixed_groups$treatment)
  )
})

test_that("a multi swap skips groups with fewer than two treatments", {
  res <- generate_multi_swap_neighbour(
    uniform_groups,
    "treatment",
    "block",
    1,
    TRUE
  )

  expect_identical(res$design$treatment, uniform_groups$treatment)
  expect_true(all(res$swapped_items == ""))
})

test_that("a multi swap only exchanges equally replicated treatments", {
  # A multi swap moves every plot of one treatment to another, so pairing
  # unequal replication would change the design.
  before <- table(prep_group$treatment)

  for (seed in 1:20) {
    set.seed(seed)
    res <- generate_multi_swap_neighbour(
      prep_group,
      "treatment",
      "block",
      1,
      TRUE
    )

    expect_equal(table(res$design$treatment), before)
  }
})

test_that("a multi swap draws from both replication classes", {
  levels_ <- levels(prep_group$treatment)
  pairs <- character(0)

  for (seed in 1:20) {
    set.seed(seed)
    res <- generate_multi_swap_neighbour(
      prep_group,
      "treatment",
      "block",
      1,
      TRUE
    )

    # `swapped_items` records factor codes rather than labels
    swapped <- res$swapped_items[res$swapped_items != ""]
    pairs <- c(pairs, paste(sort(levels_[as.integer(swapped)]), collapse = ""))
  }

  # A/B are the two-plot class and C/D the three-plot class; a pair spanning
  # them, such as "AC", would change the design's replication
  expect_setequal(pairs, c("AB", "CD"))
})
