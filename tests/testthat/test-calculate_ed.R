test_that("calculate_ed works for partial replications", {
  # fmt: skip
  design_matrix <- matrix(c(
    1, 1, 1, 4,
    2, 3, 3, 4,
    2, 3, 3, 5,
    6, 6, 5, 4
  ), ncol = 4, byrow = TRUE)

  # MST_i is the mean edge length of the tree, so it is comparable across
  # treatments with different numbers of replications (Piepho et al. 2018,
  # S3.2b). Treatment 3 has four replications in a tight 2x2 block and
  # treatment 4 has three spread down a column: the means separate them
  # (1 vs 1.5) where the totals would tie them both at 3.
  expected_msts <- c(
    "1" = 1, # (1,1) (1,2) (1,3): two edges of length 1
    "2" = 1, # (2,1) (3,1): one edge of length 1
    "3" = 1, # 2x2 block: three edges of length 1
    "4" = 1.5, # (1,4) (2,4) (4,4): edges of length 1 and 2
    "5" = sqrt(2), # (3,4) (4,3): one diagonal edge
    "6" = 1 # (4,1) (4,2): one edge of length 1
  )

  result <- calculate_ed(design_matrix)
  expect_equal(result$msts, expected_msts, tolerance = 1e-10)
  expect_equal(result$total_mst, sum(expected_msts), tolerance = 1e-10)
  expect_equal(
    result$inv_total_mst,
    sum(1 / expected_msts[expected_msts > 0]),
    tolerance = 1e-10
  )
})

test_that("the two MST implementations agree", {
  # calculate_ed picks between them on point count, so they must not diverge
  skip_if_not_installed("igraph")

  # 2 replications, collinear, an L, a square, and a scattered set
  point_sets <- list(
    cbind(1:2, c(1, 1)),
    cbind(1:3, c(1, 1, 1)),
    cbind(c(1, 1, 4), c(1, 4, 1)),
    cbind(c(1, 1, 3, 3), c(1, 3, 1, 3)),
    cbind(c(1, 4, 2, 7, 5), c(3, 1, 6, 2, 5))
  )

  for (xy in point_sets) {
    d <- as.matrix(dist(xy))
    expect_equal(.mst_mean_prim(d), .mst_mean_igraph(d))
  }
})

test_that("Prim's handles coincident points, which igraph does not", {
  # igraph::graph_from_adjacency_matrix() reads a weight of 0 as an absent edge,
  # so two points at the same position drop out of the tree entirely
  d <- as.matrix(dist(cbind(c(1, 1, 5), c(1, 1, 1))))

  expect_equal(.mst_mean_prim(d), 2) # edges of length 0 and 4
  skip_if_not_installed("igraph")
  expect_equal(.mst_mean_igraph(d), 4) # documents the limitation
})

test_that("calculate_ed gives unreplicated items an MST of zero", {
  design_matrix <- matrix(c(1, 1, 2, 3), nrow = 2)

  result <- calculate_ed(design_matrix)
  expect_equal(result$msts[["2"]], 0)
  expect_equal(result$msts[["3"]], 0)
  # ...and excludes them from inv_total_mst rather than dividing by zero
  expect_true(is.finite(result$inv_total_mst))
  expect_equal(result$inv_total_mst, 1 / result$msts[["1"]])
})
