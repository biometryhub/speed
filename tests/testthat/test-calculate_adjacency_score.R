test_that("calculate_adjacency_score works as expected", {
  # 1 2
  # 1 2
  # fmt: skip
  design <- data.frame(
    row = rep(1:3, times = 3),
    col = rep(1:3, each = 3),
    swap = rep(1:3, each = 3)
  )
  expect_equal(calculate_adjacency_score(design, "swap"), 6)

  # 1 1
  # 2 3
  design$swap <- c(1, 2, 1, 1, 3, 2, 1, 2, 3)
  expect_equal(calculate_adjacency_score(design, "swap"), 2)

  # 1 1 1
  # 2 3 3
  # 2 3 3
  design$swap <- c(1, 2, 2, 1, 3, 3, 1, 3, 3)
  expect_equal(calculate_adjacency_score(design, "swap"), 7)

  # 1 3 2
  # 2 1 3
  # 3 2 1
  design$swap <- c(1, 2, 3, 3, 1, 2, 2, 3, 1)
  expect_equal(calculate_adjacency_score(design, "swap"), 0)
})

test_that("ring_offsets enumerates the expected ring", {
  manhattan_1 <- ring_offsets(1, "manhattan")
  expect_equal(nrow(manhattan_1), 4)
  expect_setequal(asplit(manhattan_1, 1), list(-1:0, 1:0, 0:-1, 0:1))

  chebyshev_1 <- ring_offsets(1, "chebyshev")
  expect_equal(nrow(chebyshev_1), 8)
  expect_false(any(chebyshev_1[, 1] == 0 & chebyshev_1[, 2] == 0))

  expect_equal(nrow(ring_offsets(2, "manhattan")), 8)
  expect_equal(nrow(ring_offsets(2, "chebyshev")), 16)
})

test_that("shift_pad shifts and NA-pads correctly", {
  m <- matrix(1:9, nrow = 3, ncol = 3)

  # shift rows down by 1
  down <- shift_pad(m, dx = 0, dy = 1)
  expect_true(all(is.na(down[1, ])))
  expect_equal(down[2:3, ], m[1:2, ])

  # shift columns right by 1
  right <- shift_pad(m, dx = 1, dy = 0)
  expect_true(all(is.na(right[, 1])))
  expect_equal(right[, 2:3], m[, 1:2])

  # negative shift moves rows up
  up <- shift_pad(m, dx = 0, dy = -1)
  expect_true(all(is.na(up[3, ])))
  expect_equal(up[1:2, ], m[2:3, ])

  # custom fill value
  filled <- shift_pad(m, dx = 0, dy = 1, fill = 0)
  expect_equal(filled[1, ], rep(0, 3))
})

test_that("adjacency_score_vec scores per-cell matches", {
  # 1 1
  # 2 2
  m <- matrix(c(1, 2, 1, 2), nrow = 2, ncol = 2)
  score <- adjacency_score_vec(m, dists = 1, weights = 1, ring_type = "manhattan")
  expect_equal(score, matrix(1, 2, 2))

  # full match: every neighbour counts (4 interior, fewer at edges/corners)
  uniform <- matrix(1, 3, 3)
  score_uniform <- adjacency_score_vec(
    uniform,
    dists = 1,
    weights = 1,
    ring_type = "manhattan"
  )
  # corners: 2 neighbours, edges: 3, centre: 4
  expect_equal(score_uniform, matrix(c(2, 3, 2, 3, 4, 3, 2, 3, 2), nrow = 3, byrow = TRUE))
})

test_that("adjacency_score_vec applies per-ring weights", {
  # fmt: skip
  m <- matrix(
    c(
      1, 2, 1, 3,
      2, 1, 2, 1,
      1, 3, 1, 2,
      2, 1, 2, 1
    ),
    nrow = 4, byrow = TRUE
  )
  weighted <- adjacency_score_vec(
    m,
    dists = c(1, 2),
    weights = c(1, 10),
    ring_type = "chebyshev"
  )
  unweighted <- adjacency_score_vec(
    m,
    dists = c(1, 2),
    weights = c(1, 1),
    ring_type = "chebyshev"
  )
  ring_2_only <- weighted - unweighted

  # check if there are matches
  expect_true(any(ring_2_only > 0))
  # ring 2 contribution is scaled by (10 - 1) = 9 per matching neighbour
  expect_true(all(ring_2_only %% 9 == 0))
})

test_that("adjacency_score_vec rejects mismatched dists/weights", {
  expect_error(adjacency_score_vec(
    matrix(1, 2, 2),
    dists = c(1, 2),
    weights = 1
  ))
})
