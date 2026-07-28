# Regression tests against the published statistics in Piepho et al. (2018).
#
# Figure 1 (p. 1174) gives a complete row-column design together with its ED, NB
# and efficiency statistics, which makes it an exact oracle for the metrics in
# metrics.R. (v, k, s) = (25, 9, 11); treatment 2 has three replications and all
# others four. Reported there: f_A^RC = 0.864; (n0, n1, n2, n3) = (222, 67, 10, 1);
# MST_2 = 3.17; MST_6 = 2.27 (the smallest among treatments with four replications).
#
# The transcription is cross-checked by three statements in the paper's own prose:
# treatment 2 is the only one replicated three times, the sole treatment pair with
# three adjacencies is (1, 19), and the two diagonal self-adjacencies belong to
# treatments 6 and 18.

# fmt: skip
fig1 <- rbind(
  c(20, 18, 23, 13,  7,  9, 12, 16, 11, 19,  1),
  c( 5, 24, 22, 19,  1, 14,  3,  9, 18, 23, 10),
  c(22, 20, 12, 21, 14, 10,  8, 18, 13, 17,  4),
  c( 8, 15, 10, 11, 16, 19,  7,  4, 23, 14, 25),
  c( 3, 16, 21, 24, 25,  6, 14, 22, 15, 13, 20),
  c( 1, 19,  6,  7,  2, 17, 18, 25, 24, 21,  8),
  c(16,  5,  1,  4, 10, 11,  6, 17,  3, 12, 24),
  c(11, 12, 15, 22, 13,  2,  5,  6,  8, 25,  9),
  c(21,  2,  7,  9,  3, 20,  4, 23, 17,  5, 15)
)

test_that("Figure 1 transcription matches the paper's description", {
  expect_equal(dim(fig1), c(9L, 11L))
  reps <- table(fig1)
  expect_equal(sum(reps == 3), 1L)
  expect_equal(names(reps)[reps == 3], "2")
  expect_equal(sum(reps == 4), 24L)
})

test_that("calculate_ed reproduces the published MST_i values for Figure 1", {
  ed <- calculate_ed(fig1)

  # MST_i is the arithmetic *mean* MST edge length (Piepho et al. 2018, S3.2b).
  # Using the total instead would give 6.36 and 6.81 here.
  # Treatment 2 sits at (6,5), (8,6), (9,2); treatment 6 at (5,6), (6,3), (7,7), (8,8).
  expect_equal(ed$msts[["2"]], (sqrt(5) + sqrt(17)) / 2)
  expect_equal(ed$msts[["6"]], (sqrt(2) + sqrt(5) + sqrt(10)) / 3)

  # ...which are the published 3.17 and 2.27 at the precision printed (the paper
  # truncates rather than rounds: MST_2 is 3.1796)
  expect_equal(trunc(ed$msts[["2"]] * 100) / 100, 3.17)
  expect_equal(trunc(ed$msts[["6"]] * 100) / 100, 2.27)

  # The paper reports MST_6 as the smallest among the four-replication treatments
  reps <- vapply(
    names(ed$msts),
    function(t) sum(fig1 == as.numeric(t)),
    integer(1)
  )
  expect_equal(names(which.min(ed$msts[reps == 4])), "6")
})

test_that("calculate_nb reproduces the published n_h distribution for Figure 1", {
  # The paper counts adjacencies along rows only. Figure 1 has s = 11 columns
  # and k = 9 rows, so the default "auto" rule selects that on its own.
  expect_equal(nb_auto_directions(nrow(fig1), ncol(fig1)), "row")
  nb <- calculate_nb(fig1, directions = "row")
  expect_equal(calculate_nb(fig1)$nb, nb$nb)

  # n_h spans every distinct treatment pair, so it sums to v(v - 1)/2
  expect_length(nb$nb, 25 * 24 / 2)
  expect_equal(
    unname(table(nb$nb)[c("0", "1", "2", "3")]),
    c(222L, 67L, 10L, 1L),
    ignore_attr = TRUE
  )

  # The single pair with three adjacencies is named in the text
  expect_equal(names(nb$nb)[nb$nb == 3], "1,19")

  # Each row of Figure 1 is a binary block, so there are no self-adjacencies
  expect_equal(nb$self_adjacencies, 0L)
})

test_that("calculate_efficiency_factor reproduces the published f_A^RC for Figure 1", {
  design_df <- data.frame(
    row = as.vector(row(fig1)),
    col = as.vector(col(fig1)),
    treatment = factor(as.vector(fig1))
  )
  design_df <- design_df[order(design_df$row, design_df$col), ]

  expect_equal(
    calculate_efficiency_factor(design_df, "treatment"),
    0.864,
    tolerance = 1e-3
  )
})


# The cyclic Latin square is the classic failure case for neighbour balance: it
# is binary in both directions, yet only 4 of its 6 treatment pairs are ever
# adjacent. Before structural zeros were counted it scored var = 0, the best
# value the metric can return, making it a global optimum of the objective.

test_that("neighbour balance is not fooled by a cyclic Latin square", {
  cyclic <- t(sapply(0:3, function(i) (seq_len(4) + i - 1) %% 4 + 1))
  # fmt: skip
  balanced <- rbind(
    c(1, 2, 3, 4),
    c(2, 4, 1, 3),
    c(3, 1, 4, 2),
    c(4, 3, 2, 1)
  )

  nb_cyclic <- calculate_nb(cyclic, directions = "row")
  nb_balanced <- calculate_nb(balanced, directions = "row")

  # Both tabulate all 6 pairs, and both are binary
  expect_length(nb_cyclic$nb, 6L)
  expect_length(nb_balanced$nb, 6L)
  expect_equal(nb_cyclic$self_adjacencies, 0L)

  # The cyclic square leaves two pairs non-adjacent; the balanced one none
  expect_equal(sum(nb_cyclic$nb == 0), 2L)
  expect_equal(sum(nb_balanced$nb == 0), 0L)

  # It must therefore score strictly worse on both measures
  expect_gt(nb_cyclic$var, nb_balanced$var)
  expect_gt(nb_cyclic$s2, nb_balanced$s2)
  expect_equal(nb_balanced$var, 0)
  expect_equal(nb_balanced$s2, 6)
})

test_that("var and s2 agree as NB criteria for binary designs", {
  # For a binary design the number of adjacencies is fixed by the layout, so the
  # variance over all pairs is an affine function of Piepho's s2 score. This is
  # what makes counting structural zeros equivalent to the published criterion.
  squares <- list(
    t(sapply(0:3, function(i) (seq_len(4) + i - 1) %% 4 + 1)),
    rbind(c(1, 2, 3, 4), c(2, 4, 1, 3), c(3, 1, 4, 2), c(4, 3, 2, 1)),
    rbind(c(1, 2, 3, 4), c(2, 1, 4, 3), c(3, 4, 1, 2), c(4, 3, 2, 1)),
    rbind(c(1, 2, 3, 4), c(4, 3, 2, 1), c(2, 1, 4, 3), c(3, 4, 1, 2))
  )
  stats <- vapply(
    squares,
    function(m) {
      nb <- calculate_nb(m, directions = "row")
      c(var = nb$var, s2 = nb$s2, total = sum(nb$nb))
    },
    numeric(3)
  )

  # k * (s - 1) = 4 * 3 adjacencies in every case, none of them self-pairs
  expect_true(all(stats["total", ] == 12))
  expect_equal(cor(stats["var", ], stats["s2", ]), 1, tolerance = 1e-9)
})
