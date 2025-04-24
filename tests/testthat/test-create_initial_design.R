# test_that("create_initial_design works without blocks", {
#   treatments <- c(1, 2, 3)
#   design <- create_initial_design(3, 3, treatments)
#
#   expect_equal(dim(design), c(3, 3))
#   expect_true(all(design %in% treatments))
# })
#
# test_that("create_initial_design works with blocks with replacement", {
#   nrows <- 4
#   ncols <- 2
#   treatments <- 1:3
#   # 1 2
#   # 1 2
#   # 1 2
#   # 1 2
#   blocks <- matrix(c(1, 1, 1, 1, 2, 2, 2, 2), nrow = nrows, ncol = ncols)
#
#   # Block 1: 3 cells → sample with replacement since treatments = 2
#   # Block 2: 3 cells → same
#
#   design <- create_initial_design(nrows, ncols, treatments, blocks)
#
#   expect_equal(dim(design), dim(blocks))
#   expect_true(all(!is.na(design)))
#   expect_true(all(design %in% treatments))
# })
#
# test_that("create_initial_design works with blocks without replacement", {
#   nrows <- 4
#   ncols <- 2
#   treatments <- 1:2
#   n_treatments <- length(treatments)
#   # 1 3
#   # 1 3
#   # 2 4
#   # 2 4
#   blocks <- matrix(c(1, 1, 2, 2, 3, 3, 4, 4), nrow = nrows, ncol = ncols)
#
#   # Block 1: 3 cells → sample with replacement since treatments = 2
#   # Block 2: 3 cells → same
#
#   design <- create_initial_design(nrows, ncols, treatments, blocks)
#
#   expect_equal(dim(design), dim(blocks))
#   expect_true(all(!is.na(design)))
#   expect_true(all(design %in% treatments))
#   expect_equal(length(unique(design[blocks == 1])), n_treatments)
#   expect_equal(length(unique(design[blocks == 2])), n_treatments)
#   expect_equal(length(unique(design[blocks == 3])), n_treatments)
#   expect_equal(length(unique(design[blocks == 4])), n_treatments)
# })
