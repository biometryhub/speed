test_that("neighbour generators return only the swaps actually made", {
  # A block whose plots all hold the same treatment offers no valid swap, so
  # nothing should be reported as swapped. The vectors are preallocated to the
  # requested swap_count, and returning them untrimmed leaves "" entries that
  # look like item names to an objective function's incremental path.
  design <- initialise_design_df(items = rep("a", 6), nrows = 2, ncols = 3)
  design$blk <- factor(1)

  single <- generate_single_swap_neighbour(design, "treatment", "blk", 3, TRUE)
  expect_length(single$swapped_items, 0)
  expect_identical(single$swapped_items, character(0))

  multi <- generate_multi_swap_neighbour(design, "treatment", "blk", 3, TRUE)
  expect_length(multi$swapped_items, 0)
  expect_identical(multi$swapped_items, character(0))
})

test_that("neighbour generators report two items per completed swap", {
  design <- initialise_design_df(
    items = rep(letters[1:3], 3),
    nrows = 3,
    ncols = 3
  )
  design$blk <- factor(1)

  set.seed(1)
  single <- generate_single_swap_neighbour(design, "treatment", "blk", 2, TRUE)
  expect_length(single$swapped_items, 4)
  expect_false(any(single$swapped_items == ""))
  expect_true(all(single$swapped_items %in% letters[1:3]))

  set.seed(1)
  multi <- generate_multi_swap_neighbour(design, "treatment", "blk", 2, TRUE)
  expect_length(multi$swapped_items, 4)
  expect_false(any(multi$swapped_items == ""))
})
