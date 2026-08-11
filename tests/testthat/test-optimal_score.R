# col: var(1, 1, 1, 2) * 4 = 1
simple_design <- function() {
  df <- expand.grid(row = 1:5, col = 1:4)
  df$treatment <- LETTERS[1:4]
  return(df)
}

row_col <- function() c("row", "col")

default_optimal_score <- function(df, ...) {
  .optimal_score(df, "treatment", row_col(), objective_function, ...)
}

expect_na <- function(obj) expect_true(is.na(obj))

test_that(".balance_score_min matches the score of an evenly split layout", {
  df <- expand.grid(row = 1:3, col = 1:3)
  df$treatment <- c(1, 2, 3, 3, 1, 2, 2, 3, 1)

  expect_equal(.balance_score_min(df, "treatment", row_col()), 0)
  expect_equal(
    .balance_score_min(df, "treatment", row_col()),
    calculate_balance_score(df, "treatment", row_col())
  )
})

test_that(".balance_score_min uses even variance per spatial factor", {
  df <- expand.grid(row = 1:3, col = 1:4)
  df$treatment <- c("A", "B", "C")

  # var(1, 1, 2) * 3 = 1
  expect_equal(.balance_score_min(df, "treatment", "row"), 1)
  expect_equal(.balance_score_min(df, "treatment", "col"), 0)
  expect_equal(.balance_score_min(df, "treatment", row_col()), 1)
})

test_that(".balance_score_min is the minimum over every arrangement", {
  reps <- c(A = 2, B = 2, C = 2, D = 2)
  df <- expand.grid(row = 1:2, col = 1:4)
  df$treatment <- names(reps)
  df$block <- rep(rep(1:2, each = 4))
  spatial_cols <- c(row_col(), "block")

  bound <- .balance_score_min(df, "treatment", spatial_cols)
  # row: var(0, 0, 1, 1) * 4 = 4/3, col: 0, block: 0
  expect_equal(bound, 4 / 3)

  # every distinct permutation
  arrangements <- list()
  assign_treatment <- function(assigned, k) {
    if (k > length(reps)) {
      arrangements[[length(arrangements) + 1]] <<- assigned
      return(invisible(NULL))
    }
    for (idx in combn(which(is.na(assigned)), reps[[k]], simplify = FALSE)) {
      next_assigned <- assigned
      next_assigned[idx] <- names(reps)[k]
      assign_treatment(next_assigned, k + 1)
    }
    return(invisible(NULL))
  }
  assign_treatment(rep(NA_character_, sum(reps)), 1)
  # 8!/2!/2!/2!/2!
  expect_length(arrangements, factorial(8) / 2^4)

  scores <- vapply(
    arrangements,
    function(arrangement) {
      df$treatment <- arrangement
      return(calculate_balance_score(df, "treatment", spatial_cols))
    },
    numeric(1)
  )
  expect_equal(min(scores), bound)
})

test_that(".balance_score_min handles a single treatment", {
  df <- expand.grid(row = 1:4, col = 1)
  df$treatment <- rep("A", 4)

  expect_equal(.balance_score_min(df, "treatment", row_col()), 0)
})

test_that(".optimal_score bounds the default objective", {
  df <- simple_design()

  expect_equal(default_optimal_score(df), 1)
  # the layout starts optimal
  df <- df[order(df$row, df$col), ]
  expect_equal(objective_function(df, "treatment", row_col())$score, 1)
})

test_that(".optimal_score applies bal_weight", {
  df <- simple_design()
  expect_equal(default_optimal_score(df), 1)
  expect_equal(default_optimal_score(df, bal_weight = 2), 2)
})

test_that(".optimal_score ignores ring/adj args", {
  df <- simple_design()
  expect_equal(default_optimal_score(df), 1)
  expect_equal(default_optimal_score(df, adj_weight = 3), 1)
  expect_equal(default_optimal_score(df, ring_weights = 2), 1)
  expect_equal(default_optimal_score(df, ring_weights = c(1, 2)), 1)
  expect_equal(default_optimal_score(df, ring_type = "chebyshev"), 1)
})

test_that(".optimal_score returns NA when no bound can be derived", {
  df <- simple_design()

  # custom objective function
  expect_na(default_optimal_score(df, obj_function = objective_function_piepho))

  # with relationship matrix
  rel_matrix <- prep_relationship(matrix(
    0.5,
    nrow = 4,
    ncol = 4,
    dimnames = list(LETTERS[1:4], LETTERS[1:4])
  ))
  expect_na(default_optimal_score(df, relationship = rel_matrix))

  # negative weights
  expect_na(default_optimal_score(df, ring_weights = c(1, -1)))
  expect_na(default_optimal_score(df, adj_weight = -1))
  expect_na(default_optimal_score(df, bal_weight = -1))

  # unknown weights
  expect_na(default_optimal_score(df, ring_weights = NA))
  expect_na(default_optimal_score(df, adj_weight = NA_real_))
  expect_na(default_optimal_score(df, bal_weight = NA_real_))
})

test_that(".balance_score_min bounds a layout with missing plots", {
  #      [,1] [,2] [,3]
  # [1,] NA   "B"  "C"
  # [2,] "A"  "B"  "C"
  # [3,] "A"  "B"  NA
  # [4,] "A"  NA   "C"
  df <- expand.grid(col = factor(1:3), row = factor(1:4))
  df$block <- factor(rep(1:2, each = 6))
  df$treatment <- factor(rep(LETTERS[1:3], 4))
  df$treatment[c(1, 9, 11)] <- NA
  spatial_cols <- c(row_col(), "block")

  bound <- .balance_score_min(df, "treatment", spatial_cols)
  # block: var(1, 2, 2) + var(2, 1, 1) = 2/3
  # row: var(1, 1, 0) * 3 + var(1, 1, 1) = 1
  expect_equal(bound, 5 / 3)

  movable <- which(!is.na(df$treatment))
  min_score <- Inf
  current <- df
  scored <- new.env(hash = TRUE, parent = emptyenv())
  set.seed(42)
  for (i in 1:400) {
    shuffled <- sample(df$treatment[movable])
    key <- paste(shuffled, collapse = "")
    if (is.null(scored[[key]])) {
      next
    }
    scored[[key]] <- TRUE

    current$treatment[movable] <- shuffled
    score <- calculate_balance_score(current, "treatment", spatial_cols)
    if (score < min_score) min_score <- score
  }
  expect_gte(min_score, bound)
})
