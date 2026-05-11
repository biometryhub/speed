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

test_that("shift_pad returns all-fill when shift exceeds matrix bounds", {
  m <- matrix(1:2, nrow = 1, ncol = 2)
  expect_true(all(is.na(shift_pad(m, dx = 0, dy = 1))))
  expect_true(all(is.na(shift_pad(m, dx = 0, dy = -1))))
  expect_true(all(is.na(shift_pad(m, dx = 2, dy = 0))))
  expect_true(all(is.na(shift_pad(m, dx = -2, dy = 0))))
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

test_that("calculate_adjacency_score uses relationship for graded similarity", {
  # A B A
  # B A B
  # A B A  -> 12 A-B edges, no like-treatment adjacencies
  design <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    swap = c("A", "B", "A", "B", "A", "B", "A", "B", "A")
  )
  expect_equal(calculate_adjacency_score(design, "swap"), 0)

  rel <- prep_relationship(matrix(
    c(1.0, 0.3, 0.3, 1.0),
    nrow = 2,
    dimnames = list(c("A", "B"), c("A", "B"))
  ))
  expect_equal(
    calculate_adjacency_score(design, "swap", relationship = rel),
    12 * 0.3
  )
})

test_that("calculate_adjacency_score with identity relationship matches default", {
  # A A A
  # B B B
  # A A A  -> 6 like-treatment edges, 6 mixed edges
  design <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    swap = c("A", "A", "A", "B", "B", "B", "A", "A", "A")
  )
  rel_mat <- diag(2)
  dimnames(rel_mat) <- list(c("A", "B"), c("A", "B"))

  expect_equal(
    calculate_adjacency_score(design, "swap", relationship = prep_relationship(rel_mat)),
    calculate_adjacency_score(design, "swap")
  )
})

test_that("prep_relationship validates input", {
  expect_error(prep_relationship(list()), "numeric matrix")

  expect_error(
    prep_relationship(matrix("x", 2, 2, dimnames = list(c("A", "B"), c("A", "B")))),
    "numeric matrix"
  )

  expect_error(
    prep_relationship(matrix(0, nrow = 2, ncol = 2)),
    "rownames and colnames"
  )

  expect_error(
    prep_relationship(
      matrix(0, 1, 1, dimnames = list("A", "A")),
      treatments = c("A", "B")
    ),
    "missing entries for treatments"
  )
})

test_that("speed preps a raw relationship matrix once before the SA loop", {
  df <- data.frame(
    row = rep(1:3, times = 3),
    col = rep(1:3, each = 3),
    treatment = rep(c("A", "B", "C"), times = 3)
  )
  rel <- matrix(
    c(1, 0.5, 0.2, 0.5, 1, 0.5, 0.2, 0.5, 1),
    nrow = 3,
    dimnames = list(c("A", "B", "C"), c("A", "B", "C"))
  )

  # capture what reaches the objective function — must be the prepped form,
  # not the raw matrix the user passed
  seen <- new.env(parent = emptyenv())
  spy_obj <- function(layout_df, swap, spatial_cols, ...) {
    dots <- list(...)
    if (!is.null(dots$relationship)) {
      seen$relationship <- dots$relationship
    }
    objective_function(layout_df, swap, spatial_cols, ...)
  }

  result <- speed(
    df,
    swap = "treatment",
    iterations = 5,
    early_stop_iterations = 5,
    obj_function = spy_obj,
    relationship = rel,
    quiet = TRUE,
    seed = 1
  )

  expect_named(seen$relationship, c("flat", "row_levels", "col_levels", "n_row"))
  expect_s3_class(result, "design")
})

test_that("adjacency_score_vec respects relationship asymmetry per cell", {
  # A B
  # B A
  m <- matrix(c("A", "B", "B", "A"), nrow = 2, byrow = TRUE)
  rel <- prep_relationship(matrix(
    c(0, 0.1, 0.7, 0),
    nrow = 2,
    dimnames = list(c("A", "B"), c("A", "B"))
  ))
  # rel["A","B"] = 0.7, rel["B","A"] = 0.1 (column-major fill)
  # A cells see 2 B neighbours -> 2 * 0.7 = 1.4
  # B cells see 2 A neighbours -> 2 * 0.1 = 0.2
  score <- adjacency_score_vec(
    m,
    dists = 1,
    weights = 1,
    ring_type = "manhattan",
    relationship = rel
  )
  expect_equal(score, matrix(c(1.4, 0.2, 0.2, 1.4), nrow = 2, byrow = TRUE))
})
