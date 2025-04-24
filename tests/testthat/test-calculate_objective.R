<<<<<<< HEAD
# test_that("calculate_objective works without blocks", {
#   # 1 1 1
#   # 2 3 3
#   # 2 3 3
#   design <- matrix(c(1, 2, 2, 1, 3, 3, 1, 3, 3), nrow = 3, ncol = 3)
#   treatments <- c(1, 2, 3)
#
#   # adjacency_score = 7, balance_score > 0
#   obj <- calculate_objective(design, treatments)
#   expect_gt(obj, 7) # because balance_score > 0 and both weights are 1
# })
#
# test_that("calculate_objective works with valid blocks", {
#   # 1 3 2
#   # 2 1 3
#   # 3 2 1
#   design <- matrix(c(1, 2, 3, 3, 1, 2, 2, 3, 1), nrow = 3, ncol = 3)
#   blocks <- matrix(c(1, 1, 1, 2, 2, 2, 3, 3, 3), nrow = 3, ncol = 3)
#   treatments <- c(1, 2, 3)
#
#   # adjacency_score = 0, balance_score = 0
#   obj <- calculate_objective(design, treatments, blocks)
#   expect_equal(obj, 0)
# })
#
# test_that("calculate_objective penalizes invalid blocks", {
#   # 1 1 1
#   # 2 3 3
#   # 2 3 3
#   design <- matrix(c(1, 2, 2, 1, 3, 3, 1, 3, 3), nrow = 3, ncol = 3)
#   blocks <- matrix(c(1, 1, 1, 2, 2, 2, 3, 3, 3), nrow = 3, ncol = 3)
#   treatments <- c(1, 2, 3)
#
#   obj <- calculate_objective(design, treatments, blocks)
#   expect_equal(obj, 1e9)
# })
#
# test_that("calculate_objective applies weights correctly", {
#   # 1 1 1
#   # 2 3 3
#   # 2 3 3
#   design <- matrix(c(1, 2, 2, 1, 3, 3, 1, 3, 3), nrow = 3, ncol = 3)
#   treatments <- c(1, 2, 3)
#
#   obj1 <- calculate_objective(design, treatments, adj_weight = 1, bal_weight = 0)
#   obj2 <- calculate_objective(design, treatments, adj_weight = 5, bal_weight = 1)
#   obj3 <- calculate_objective(design, treatments, adj_weight = 1, bal_weight = 5)
#
#   expect_equal(obj1, 7)
#   expect_gt(obj2, 35)
#   expect_gt(obj3, 7)
# })
=======
test_that("calculate_objective works without blocks", {
  # Create test data frame
  df <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    Treatment = c(1, 1, 1, 2, 3, 3, 2, 3, 3)
  )

  design <- matrix(df$Treatment, nrow = 3, ncol = 3)
  permute_var <- "Treatment"
  spatial_fac <- c("row", "col")

  # adjacency_score = 7, balance_score > 0
  obj <- calculate_objective(design, permute_var, df, spatial_fac)
  expect_gt(obj, 7) # because balance_score > 0 and both weights are 1
})

test_that("calculate_objective works with valid blocks", {
  # Create test data frame with blocks
  df <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    Treatment = c(1, 3, 2, 2, 1, 3, 3, 2, 1),
    Block = rep(1:3, each = 3)
  )

  design <- matrix(df$Treatment, nrow = 3, ncol = 3)
  blocks <- matrix(df$Block, nrow = 3, ncol = 3)
  permute_var <- "Treatment"
  spatial_fac <- c("row", "col")

  # adjacency_score = 0, balance_score = 0
  obj <- calculate_objective(design, permute_var, df, spatial_fac)
  expect_equal(obj, 0)
})

test_that("calculate_objective applies weights correctly", {
  df <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    Treatment = c(1, 1, 1, 2, 3, 3, 2, 3, 3)
  )

  design <- matrix(df$Treatment, nrow = 3, ncol = 3)
  permute_var <- "Treatment"
  spatial_fac <- c("row", "col")

  obj1 <- calculate_objective(design, permute_var, df, spatial_fac,
                            adj_weight = 1, bal_weight = 0)
  obj2 <- calculate_objective(design, permute_var, df, spatial_fac,
                            adj_weight = 5, bal_weight = 1)
  obj3 <- calculate_objective(design, permute_var, df, spatial_fac,
                            adj_weight = 1, bal_weight = 5)

  expect_equal(obj1, 7)
  expect_gt(obj2, 35)
  expect_gt(obj3, 7)
})

>>>>>>> origin/main
