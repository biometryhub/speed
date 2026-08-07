# Helpers ---------------------------------------------------------------------

simple_df <- function() {
  data.frame(
    row = rep(1:4, times = 5),
    col = rep(1:5, each = 4),
    treatment = rep(LETTERS[1:4], 5),
    trt_name = rep(paste("Variety", LETTERS[1:4]), 5),
    stringsAsFactors = FALSE
  )
}

split_plot_df <- function() {
  df <- data.frame(
    row = rep(1:12, each = 4),
    col = rep(1:4, times = 12),
    block = rep(1:4, each = 12),
    wholeplot = rep(1:12, each = 4),
    wp_trt = rep(rep(LETTERS[1:3], each = 4), times = 4),
    sp_trt = rep(letters[1:4], 12),
    stringsAsFactors = FALSE
  )
  df$wp_label <- paste("Irrigation", df$wp_trt)
  df$sp_label <- paste("Variety", df$sp_trt)
  df
}

# `treatment` -> `trt_name` is a fixed mapping, so a linked column is correct
# exactly when every row still pairs the two the same way as the input did
pairing_of <- function(df, key, value) {
  unique(data.frame(k = as.character(df[[key]]), v = as.character(df[[value]])))
}

expect_pairing_preserved <- function(input, output, key, value) {
  before <- pairing_of(input, key, value)
  after <- pairing_of(output, key, value)
  before <- before[order(before$k), ]
  after <- after[order(after$k), ]
  rownames(before) <- NULL
  rownames(after) <- NULL
  expect_equal(after, before)
}

# Simple designs ---------------------------------------------------------------

test_that("linked_cols keeps a companion column with its treatment", {
  df <- simple_df()
  result <- speed(
    df,
    swap = "treatment",
    linked_cols = "trt_name",
    seed = 42,
    quiet = TRUE
  )

  expect_pairing_preserved(df, result$design_df, "treatment", "trt_name")
})

test_that("linked_cols leaves no bookkeeping columns behind", {
  df <- simple_df()
  result <- speed(
    df,
    swap = "treatment",
    linked_cols = "trt_name",
    seed = 42,
    quiet = TRUE
  )

  expect_false(any(grepl("^\\.origin", names(result$design_df))))
  expect_equal(names(result$design_df), names(df))
})

test_that("linked_cols preserves the type of the companion column", {
  df <- simple_df()
  df$trt_num <- match(df$treatment, LETTERS)
  df$trt_fct <- factor(df$trt_name)

  result <- speed(
    df,
    swap = "treatment",
    linked_cols = c("trt_name", "trt_num", "trt_fct"),
    seed = 42,
    quiet = TRUE
  )

  expect_type(result$design_df$trt_name, "character")
  expect_type(result$design_df$trt_num, "integer")
  expect_s3_class(result$design_df$trt_fct, "factor")
  expect_equal(levels(result$design_df$trt_fct), levels(df$trt_fct))
})

test_that("linked_cols actually moves the companion column", {
  df <- simple_df()
  result <- speed(
    df,
    swap = "treatment",
    linked_cols = "trt_name",
    seed = 42,
    quiet = TRUE
  )

  # The optimised design differs from the input, so the companion must have moved too
  expect_false(identical(
    as.character(result$design_df$treatment),
    df$treatment
  ))
  expect_false(identical(as.character(result$design_df$trt_name), df$trt_name))
})

test_that("linked_cols carries a per-plot column on a single-swap level", {
  df <- simple_df()
  # Unique per row, so no value-level lookup could reconstruct it
  df$plot_id <- sprintf("P%02d", seq_len(nrow(df)))

  result <- speed(
    df,
    swap = "treatment",
    linked_cols = "plot_id",
    seed = 42,
    quiet = TRUE
  )

  expect_setequal(result$design_df$plot_id, df$plot_id)
  # Each plot_id must still sit with the treatment it started with
  expect_pairing_preserved(df, result$design_df, "plot_id", "treatment")
})

test_that("linked_cols does not change the design or its scores", {
  df <- simple_df()
  without <- speed(df, swap = "treatment", seed = 42, quiet = TRUE)
  with <- speed(
    df,
    swap = "treatment",
    linked_cols = "trt_name",
    seed = 42,
    quiet = TRUE
  )

  expect_equal(with$design_df$treatment, without$design_df$treatment)
  expect_equal(with$score, without$score)
  expect_equal(with$scores, without$scores)
})

test_that("linked_cols is score neutral with random initialisation", {
  df <- simple_df()
  params <- optim_params(random_initialisation = 5)

  without <- speed(
    df,
    swap = "treatment",
    optimise_params = params,
    seed = 7,
    quiet = TRUE
  )
  with <- speed(
    df,
    swap = "treatment",
    linked_cols = "trt_name",
    optimise_params = params,
    seed = 7,
    quiet = TRUE
  )

  expect_equal(with$design_df$treatment, without$design_df$treatment)
  expect_equal(with$score, without$score)
  expect_pairing_preserved(df, with$design_df, "treatment", "trt_name")
})

test_that("linked_cols works with swap_within", {
  df <- simple_df()
  df$block <- rep(1:5, each = 4)

  result <- speed(
    df,
    swap = "treatment",
    swap_within = "block",
    linked_cols = "trt_name",
    seed = 42,
    quiet = TRUE
  )

  expect_pairing_preserved(df, result$design_df, "treatment", "trt_name")
})

test_that("linked_cols carries original values through NA treatments", {
  df <- simple_df()
  df$treatment[c(3, 11)] <- NA
  df$trt_name[c(3, 11)] <- NA

  result <- speed(
    df,
    swap = "treatment",
    linked_cols = "trt_name",
    seed = 42,
    quiet = TRUE
  )

  # NA plots are never swapped, so they keep their own row's values
  expect_true(all(is.na(result$design_df$trt_name[is.na(
    result$design_df$treatment
  )])))
  expect_equal(sum(is.na(result$design_df$trt_name)), 2)
})

# Hierarchical designs ---------------------------------------------------------

test_that("linked_cols links a different column at each level of a split-plot", {
  df <- split_plot_df()

  result <- speed(
    df,
    swap = list(wp = "wp_trt", sp = "sp_trt"),
    swap_within = list(wp = "block", sp = "wholeplot"),
    linked_cols = list(wp = "wp_label", sp = "sp_label"),
    swap_all = TRUE,
    seed = 42,
    quiet = TRUE
  )

  expect_pairing_preserved(df, result$design_df, "wp_trt", "wp_label")
  expect_pairing_preserved(df, result$design_df, "sp_trt", "sp_label")
})

test_that("linked_cols can be given for one level only", {
  df <- split_plot_df()

  sp_only <- speed(
    df,
    swap = list(wp = "wp_trt", sp = "sp_trt"),
    swap_within = list(wp = "block", sp = "wholeplot"),
    linked_cols = list(sp = "sp_label"),
    swap_all = TRUE,
    seed = 42,
    quiet = TRUE
  )
  expect_pairing_preserved(df, sp_only$design_df, "sp_trt", "sp_label")

  wp_only <- speed(
    df,
    swap = list(wp = "wp_trt", sp = "sp_trt"),
    swap_within = list(wp = "block", sp = "wholeplot"),
    linked_cols = list(wp = "wp_label"),
    swap_all = TRUE,
    seed = 42,
    quiet = TRUE
  )
  expect_pairing_preserved(df, wp_only$design_df, "wp_trt", "wp_label")
})

test_that("linked_cols is score neutral for hierarchical designs", {
  df <- split_plot_df()
  args <- list(
    data = df,
    swap = list(wp = "wp_trt", sp = "sp_trt"),
    swap_within = list(wp = "block", sp = "wholeplot"),
    swap_all = TRUE,
    seed = 42,
    quiet = TRUE
  )

  without <- do.call(speed, args)
  with <- do.call(
    speed,
    c(args, list(linked_cols = list(wp = "wp_label", sp = "sp_label")))
  )

  expect_equal(with$design_df$wp_trt, without$design_df$wp_trt)
  expect_equal(with$design_df$sp_trt, without$design_df$sp_trt)
  expect_equal(with$score, without$score)
})

test_that("linked_cols works via the optimise argument", {
  df <- split_plot_df()

  result <- speed(
    df,
    swap = list(wp = "wp_trt", sp = "sp_trt"),
    optimise = list(
      wp = list(
        swap = "wp_trt",
        swap_within = "block",
        linked_cols = "wp_label"
      ),
      sp = list(
        swap = "sp_trt",
        swap_within = "wholeplot",
        linked_cols = "sp_label"
      )
    ),
    swap_all = TRUE,
    seed = 42,
    quiet = TRUE
  )

  expect_pairing_preserved(df, result$design_df, "wp_trt", "wp_label")
  expect_pairing_preserved(df, result$design_df, "sp_trt", "sp_label")
})

test_that("linked_cols accumulates across levels sharing one swap column", {
  # Both levels optimise `lines`, so one index must follow both passes
  df <- data.frame(
    row = rep(1:10, times = 4),
    col = rep(1:4, each = 10),
    site = rep(c("a", "b"), each = 20),
    lines = rep(1:10, 4),
    stringsAsFactors = FALSE
  )
  df$line_name <- paste0("L", df$lines)

  result <- speed(
    df,
    swap = "lines",
    linked_cols = "line_name",
    optimise = list(
      connectivity = list(spatial_factors = ~site),
      balance = list(swap_within = "site", spatial_factors = ~col)
    ),
    seed = 11,
    quiet = TRUE
  )

  expect_pairing_preserved(df, result$design_df, "lines", "line_name")
})

# Validation -------------------------------------------------------------------

test_that("linked_cols rejects columns that are not in the data", {
  df <- simple_df()
  expect_error(
    speed(
      df,
      swap = "treatment",
      linked_cols = "nope",
      seed = 42,
      quiet = TRUE
    ),
    "'nope' not found in"
  )
})

test_that("linked_cols rejects columns used by the optimisation", {
  df <- simple_df()
  df$block <- rep(1:5, each = 4)

  expect_error(
    speed(
      df,
      swap = "treatment",
      linked_cols = "treatment",
      seed = 42,
      quiet = TRUE
    ),
    "already used as a swap"
  )
  expect_error(
    speed(
      df,
      swap = "treatment",
      swap_within = "block",
      linked_cols = "block",
      seed = 42,
      quiet = TRUE
    ),
    "already used as a swap"
  )
  expect_error(
    speed(df, swap = "treatment", linked_cols = "row", seed = 42, quiet = TRUE),
    "already used as a swap"
  )
})

test_that("linked_cols rejects an unknown level name", {
  df <- split_plot_df()

  expect_error(
    speed(
      df,
      swap = list(wp = "wp_trt", sp = "sp_trt"),
      swap_within = list(wp = "block", sp = "wholeplot"),
      linked_cols = list(nope = "wp_label"),
      swap_all = TRUE,
      seed = 42,
      quiet = TRUE
    ),
    "no matching level for 'nope'"
  )
})

test_that("linked_cols rejects one column linked to two swap columns", {
  df <- split_plot_df()

  # swap_all = FALSE so the functional dependence check does not fire first
  expect_error(
    speed(
      df,
      swap = list(wp = "wp_trt", sp = "sp_trt"),
      swap_within = list(wp = "block", sp = "wholeplot"),
      linked_cols = list(wp = "sp_label", sp = "sp_label"),
      seed = 42,
      quiet = TRUE
    ),
    "can only travel with one swap column"
  )
})

test_that("linked_cols carries a per-plot column on a swap_all level", {
  df <- split_plot_df()
  # Unique per row, so no value-level lookup could reconstruct it
  df$plot_id <- sprintf("P%02d", seq_len(nrow(df)))

  result <- speed(
    df,
    swap = list(wp = "wp_trt", sp = "sp_trt"),
    swap_within = list(wp = "block", sp = "wholeplot"),
    linked_cols = list(wp = "plot_id"),
    swap_all = TRUE,
    seed = 42,
    quiet = TRUE
  )

  # No value duplicated or lost, and each plot_id still sits with its own treatment
  expect_setequal(result$design_df$plot_id, df$plot_id)
  expect_pairing_preserved(df, result$design_df, "plot_id", "wp_trt")
})

test_that("linked_cols survives cross-cutting swap_all levels", {
  # `block` and `site` cut across each other, so a level 1 swap can unbalance a site
  # mid-search. The provenance index must stay a permutation regardless.
  df <- data.frame(
    row = rep(1:6, times = 2),
    col = rep(1:2, each = 6),
    block = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
    site = c("a", "a", "a", "b", "b", "b", "a", "a", "a", "b", "b", "b"),
    lines = c("X", "X", "Z", "Y", "Y", "Z", "Y", "Y", "Z", "X", "X", "Z"),
    stringsAsFactors = FALSE
  )
  df$plot_id <- sprintf("P%02d", seq_len(nrow(df)))

  for (seed in 1:5) {
    warnings_seen <- character(0)
    result <- withCallingHandlers(
      speed(
        df,
        swap = "lines",
        optimise = list(
          lvl1 = list(swap_within = "block", swap_all = TRUE),
          lvl2 = list(swap_within = "site", swap_all = TRUE)
        ),
        linked_cols = "plot_id",
        iterations = 30,
        seed = seed,
        quiet = TRUE
      ),
      warning = function(w) {
        warnings_seen <<- c(warnings_seen, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )

    # The recycling this guards against; a frozen-group warning is expected on some seeds
    expect_false(any(grepl("number of items to replace", warnings_seen)))
    expect_setequal(result$design_df$plot_id, df$plot_id)
    expect_pairing_preserved(df, result$design_df, "plot_id", "lines")
  }
})

test_that("linked_cols allows a functionally dependent column on a swap_all level", {
  df <- split_plot_df()

  expect_no_error(
    speed(
      df,
      swap = list(wp = "wp_trt", sp = "sp_trt"),
      swap_within = list(wp = "block", sp = "wholeplot"),
      linked_cols = list(wp = "wp_label"),
      swap_all = TRUE,
      seed = 42,
      quiet = TRUE
    )
  )
})
