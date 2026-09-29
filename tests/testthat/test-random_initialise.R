# `random_initialise()` takes the per-level list `speed()` resolves its arguments
# into, and a design whose optimised columns are already factors
optimise_level <- function(swap, swap_within, swap_all = FALSE, n = 5) {
  return(list(
    swap = swap,
    swap_within = swap_within,
    swap_all = swap_all,
    linked_cols = NULL,
    spatial_factors = ~ row + col,
    obj_function = objective_function,
    optimise_params = optim_params(random_initialisation = n)
  ))
}

#' 4 treatments replicated 5 times in a 5 by 4 grid
df_simple <- function() {
  df <- expand.grid(col = 1:4, row = 1:5)
  df$treatment <- LETTERS[1:4]

  return(df)
}

#' A 4 block, 12 wholeplot split plot, with plots emptied by position
split_plot_design <- function(
  subplot_treatment = letters[1:4],
  missing = list()
) {
  design <- data.frame(
    row = rep(1:12, each = 4),
    col = rep(1:4, times = 12),
    block = rep(1:4, each = 12),
    wholeplot = rep(1:12, each = 4),
    wholeplot_treatment = rep(rep(LETTERS[1:3], each = 4), times = 4)
  )
  if (!is.null(subplot_treatment)) {
    design$subplot_treatment <- rep(subplot_treatment, 12)
  }

  for (column in names(missing)) {
    design[[column]][missing[[column]]] <- NA
  }

  return(to_factor(design, names(design))$df)
}

# A whole plot holds one whole-plot treatment, a strip one strip treatment.
# Reports the offending groups and their counts rather than a bare `all()`.
expect_distinct_per_group <- function(values, groups, n = 1) {
  label <- deparse(substitute(values))
  counts <- tapply(
    as.character(values),
    groups,
    function(x) return(length(unique(x)))
  )
  # A factor level the data no longer uses gives `NA` rather than a count
  counts <- counts[!is.na(counts)]
  offenders <- counts[counts != n]

  expect(
    length(offenders) == 0,
    sprintf(
      "%s holds %s distinct value(s) per group, not %d: %s.",
      label,
      paste(sort(unique(offenders)), collapse = "/"),
      n,
      paste(names(offenders), offenders, sep = "=", collapse = ", ")
    )
  )

  return(invisible(values))
}

test_that("shuffle_item_sets only exchanges equally replicated treatments", {
  design <- data.frame(
    block = factor(rep(1, 7)),
    treatment = factor(c("A", "A", "B", "B", "C", "D", NA))
  )
  movable <- !is.na(design$treatment)

  for (seed in 1:5) {
    set.seed(seed)
    shuffled <- shuffle_item_sets(
      design,
      "treatment",
      design$block,
      NULL,
      movable
    )

    expect_equal(table(shuffled$treatment), table(design$treatment))
    expect_true(is.na(shuffled$treatment[7]))

    # The two plots a doubly replicated treatment held stay one set
    expect_equal(shuffled$treatment[1], shuffled$treatment[2])
    expect_equal(shuffled$treatment[3], shuffled$treatment[4])
    expect_setequal(as.character(shuffled$treatment[1:4]), c("A", "B"))
    expect_setequal(as.character(shuffled$treatment[5:6]), c("C", "D"))
  }
})

test_that("shuffle_item_sets leaves a group holding one treatment alone", {
  design <- data.frame(
    block = factor(c(1, 1, 2, 2)),
    treatment = factor(c("A", "A", "B", "C"))
  )

  set.seed(1)
  shuffled <- shuffle_item_sets(
    design,
    "treatment",
    design$block,
    NULL,
    rep(TRUE, 4)
  )

  expect_equal(shuffled$treatment[1:2], design$treatment[1:2])
})

test_that("speed runs with random initialisation", {
  test_data <- df_simple()

  result <- speed(
    data = test_data,
    swap = "treatment",
    spatial_factors = ~ row + col,
    iterations = 1000,
    optimise_params = optim_params(stop_at_optimal = FALSE),
    seed = 42,
    quiet = TRUE
  )

  result_random <- speed(
    data = test_data,
    swap = "treatment",
    spatial_factors = ~ row + col,
    iterations = 1000,
    optimise_params = optim_params(
      random_initialisation = TRUE,
      stop_at_optimal = FALSE
    ),
    seed = 42,
    quiet = TRUE
  )

  expect_named(
    result_random,
    c(
      "design_df",
      "score",
      "scores",
      "temperatures",
      "iterations_run",
      "stopped_early",
      "treatments",
      "seed",
      "metadata"
    )
  )

  expect_true(is.data.frame(result_random$design_df))
  expect_true(is.numeric(result_random$score))
  expect_true(is.numeric(result_random$scores))
  expect_true(is.numeric(result_random$temperatures))
  expect_true(is.logical(result_random$stopped_early))
  expect_true(is.character(result_random$treatments))

  expect_equal(nrow(result_random$design_df), 20)
  expect_equal(ncol(result_random$design_df), 3)
  expect_equal(result_random$score, 1)
  expect_equal(length(result_random$scores), 1000)
  expect_equal(length(result_random$temperatures), 1000)
  expect_equal(result_random$iterations_run, 1000)
  expect_equal(result_random$stopped_early, FALSE)
  expect_equal(result_random$treatments, c("A", "B", "C", "D"))

  expect_false(isTRUE(all.equal(
    result_random$design_df$treatment,
    result$design_df$treatment
  )))
})

test_that("speed runs with n random initialisation", {
  df <- df_simple()
  initial_score <- objective_function(df, "treatment", c("row", "col"))$score

  result <- speed(
    data = df,
    swap = "treatment",
    iterations = 1000,
    early_stop_iterations = 500,
    optimise_params = optim_params(random_initialisation = TRUE),
    seed = 112,
    quiet = TRUE
  )

  result_n <- speed(
    data = df,
    swap = "treatment",
    iterations = 1000,
    early_stop_iterations = 500,
    optimise_params = optim_params(random_initialisation = 5),
    seed = 112,
    quiet = TRUE
  )

  expect_false(all(result_n$design_df$treatment == result$design_df$treatment))
  expect_lt(result_n$scores[1], initial_score)
  expect_equal(result_n$score, 1)
  expect_equal(sort(result_n$design_df$treatment), sort(df$treatment))
})

test_that("one seed gives the same design however many restarts it draws", {
  df <- df_simple()

  run <- function() {
    return(speed(
      data = df,
      swap = "treatment",
      iterations = 100,
      optimise_params = optim_params(
        random_initialisation = 50,
        stop_at_optimal = FALSE
      ),
      seed = 3,
      quiet = TRUE
    ))
  }

  first <- run()
  second <- run()

  expect_equal(first$design_df, second$design_df)
})

test_that("random_initialise returns immediately on a zero score", {
  # Zeroing both weights makes the objective identically 0, so the early return
  # is exercised without relying on a shuffle finding a perfect layout.
  test_data <- df_simple()

  result <- speed(
    test_data,
    swap = "treatment",
    spatial_factors = ~ row + col,
    iterations = 10,
    seed = 1,
    quiet = TRUE,
    optimise_params = optim_params(
      random_initialisation = 3,
      adj_weight = 0,
      bal_weight = 0
    )
  )

  expect_equal(result$score, 0)
  expect_equal(sort(result$design_df$treatment), sort(test_data$treatment))
})

test_that("random initialisation leaves missing plots where they are", {
  design <- split_plot_design(
    subplot_treatment = NULL,
    missing = list(wholeplot_treatment = c(1, 9, 11))
  )

  shuffled <- random_initialise(
    design,
    list(wp = optimise_level("wholeplot_treatment", "block")),
    seed = 42
  )

  expect_identical(
    which(is.na(shuffled$wholeplot_treatment)),
    which(is.na(design$wholeplot_treatment))
  )
  expect_equal(
    sort(shuffled$wholeplot_treatment),
    sort(design$wholeplot_treatment)
  )
  expect_false(identical(
    as.character(shuffled$wholeplot_treatment),
    as.character(design$wholeplot_treatment)
  ))
})

test_that("random initialisation leaves each level's missing plots in place", {
  # `sp` is missing on the plots of one wholeplot, `wp` on the plots of another
  design <- split_plot_design(
    missing = list(
      subplot_treatment = 5:8,
      wholeplot_treatment = 25:28
    )
  )
  levels <- list(
    wp = optimise_level("wholeplot_treatment", "block"),
    sp = optimise_level("subplot_treatment", "wholeplot")
  )

  shuffled <- random_initialise(design, levels, seed = 42)

  for (column in c("wholeplot_treatment", "subplot_treatment")) {
    expect_identical(
      which(is.na(shuffled[[column]])),
      which(is.na(design[[column]]))
    )
  }

  # A plot missing at one level still carries the other level's treatment, so
  # that treatment is free to move
  expect_false(identical(
    as.character(shuffled$wholeplot_treatment[5:8]),
    as.character(design$wholeplot_treatment[5:8])
  ))
})

test_that("random initialisation keeps units whole around a missing subplot", {
  # A plot missing a subplot treatment still carries a wholeplot treatment, so
  # holding it back would leave part of that wholeplot's label set behind
  design <- split_plot_design(missing = list(subplot_treatment = c(1, 5)))
  levels <- list(
    wp = optimise_level("wholeplot_treatment", "block", swap_all = TRUE),
    sp = optimise_level("subplot_treatment", "wholeplot", swap_all = TRUE)
  )

  for (seed in 1:10) {
    shuffled <- random_initialise(design, levels, seed = seed)

    expect_distinct_per_group(
      shuffled$wholeplot_treatment,
      shuffled$wholeplot
    )
  }
})

test_that("random initialisation keeps a swap_all design's units whole", {
  split_data <- data.frame(
    row = rep(1:12, each = 4),
    col = rep(1:4, times = 12),
    block = rep(1:4, each = 12),
    wholeplot = rep(1:12, each = 4),
    wholeplot_treatment = rep(rep(LETTERS[1:3], each = 4), times = 4),
    subplot_treatment = rep(letters[1:4], 12)
  )

  result <- speed(
    split_data,
    swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
    swap_within = list(wp = "block", sp = "wholeplot"),
    swap_all = TRUE,
    iterations = 200,
    optimise_params = optim_params(random_initialisation = 5),
    seed = 42,
    quiet = TRUE
  )
  design_df <- result$design_df

  # `swap_all` moves whole sets of like-treatment plots, so every wholeplot
  # still holds one wholeplot treatment across all four of its plots
  expect_distinct_per_group(design_df$wholeplot_treatment, design_df$wholeplot)

  expect_equal(
    table(design_df$block, design_df$wholeplot_treatment),
    table(split_data$block, split_data$wholeplot_treatment)
  )

  # The shuffle still moves something, rather than passing the design through
  expect_false(identical(
    as.character(design_df$wholeplot_treatment),
    as.character(split_data$wholeplot_treatment)
  ))
})

test_that("random initialisation keeps swap_all strips whole", {
  df_strip <- data.frame(
    row = rep(1:12, each = 6),
    col = rep(1:6, times = 12),
    block = rep(rep(1:2, each = 3), times = 4) + rep(0:2 * 2, each = 24),
    vertical_treatment = rep(rep(LETTERS[1:3], times = 2), times = 12),
    horizontal_treatment = rep(rep(letters[1:4], each = 6), times = 3)
  )

  result <- speed(
    df_strip,
    swap = list(ht = "horizontal_treatment", vt = "vertical_treatment"),
    swap_within = list(ht = "block", vt = "block"),
    swap_all = TRUE,
    iterations = list(ht = 100, vt = 100),
    optimise_params = optim_params(random_initialisation = 5),
    seed = 42,
    quiet = TRUE
  )
  design_df <- result$design_df

  # A strip runs the width of its block, so each holds a single treatment
  strips <- list(
    vertical_treatment = paste(design_df$block, design_df$col),
    horizontal_treatment = paste(design_df$block, design_df$row)
  )
  for (column in names(strips)) {
    expect_distinct_per_group(design_df[[column]], strips[[column]])
  }
})

test_that("random initialisation keeps linked_cols paired under swap_all", {
  split_data <- data.frame(
    row = rep(1:12, each = 4),
    col = rep(1:4, times = 12),
    block = rep(1:4, each = 12),
    wholeplot = rep(1:12, each = 4),
    wholeplot_treatment = rep(rep(LETTERS[1:3], each = 4), times = 4)
  )
  split_data$wholeplot_name <- paste0(
    "trt-",
    tolower(split_data$wholeplot_treatment)
  )

  result <- speed(
    split_data,
    swap = "wholeplot_treatment",
    swap_within = "block",
    linked_cols = "wholeplot_name",
    swap_all = TRUE,
    iterations = 200,
    optimise_params = optim_params(random_initialisation = 5),
    seed = 42,
    quiet = TRUE
  )

  # One name per treatment, and the same one it started with. A label moved
  # without its linked column would show up as an extra pair here.
  pairing <- function(design_df) {
    pairs <- unique(data.frame(
      treatment = as.character(design_df$wholeplot_treatment),
      name = as.character(design_df$wholeplot_name)
    ))
    return(pairs[order(pairs$treatment), ])
  }

  expect_equal(
    pairing(result$design_df),
    pairing(split_data),
    ignore_attr = "row.names"
  )
})
