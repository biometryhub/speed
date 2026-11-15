# Multi-level designs: split-plot, split-split plot, strip plot and MET.

test_that("speed handles split plot designs", {
  df_split <- data.frame(
    row = rep(1:12, each = 4),
    col = rep(1:4, times = 12),
    block = rep(1:4, each = 12),
    wholeplot = rep(1:12, each = 4),
    wholeplot_treatment = rep(rep(LETTERS[1:3], each = 4), times = 4),
    subplot_treatment = rep(letters[1:4], 12)
  )

  result <- speed(
    df_split,
    swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
    swap_within = list(wp = "block", sp = "wholeplot"),
    early_stop_iterations = list(wp = 1000, sp = 10000),
    swap_all = TRUE,
    seed = 1,
    quiet = TRUE
  )

  expect_s3_class(result, "design")
  expect_equal(result$score, 100)
  expect_equal(result$iterations_run, 1726)
  expect_equal(result$stopped_early, c(wp = TRUE, sp = TRUE))

  vdiffr::expect_doppelganger(
    "speed_splitplot_wp",
    autoplot(result, treatments = "wholeplot_treatment")
  )
  vdiffr::expect_doppelganger(
    "speed_splitplot_sp",
    autoplot(result, treatments = "subplot_treatment")
  )
})

test_that("speed handles split-split plot designs", {
  df_split_split <- data.frame(
    row = rep(1:16, each = 9),
    col = rep(1:9, times = 16),
    block = rep(1:4, each = 36),
    wholeplot = rep(rep(1:3, each = 3), times = 16) + rep(0:3 * 3, each = 36),
    wholeplot_treatment = rep(rep(LETTERS[1:3], each = 3), times = 16),
    subplot = rep(1:48, each = 3),
    subplot_treatment = rep(rep(letters[1:4], each = 3), times = 12),
    subsubplot_treatment = rep(c("x", "y", "z"), 48)
  )

  result <- speed(
    df_split_split,
    swap = list(
      wp = "wholeplot_treatment",
      sp = "subplot_treatment",
      ssp = "subsubplot_treatment"
    ),
    swap_within = list(wp = "block", sp = "wholeplot", ssp = "subplot"),
    iterations = list(wp = 500, sp = 500, ssp = 1000),
    early_stop_iterations = list(wp = 200, sp = 200, ssp = 400),
    swap_all = TRUE,
    seed = 42,
    quiet = TRUE
  )

  expect_s3_class(result, "design")

  expect_named(
    result,
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

  expect_true(is.data.frame(result$design_df))
  expect_true(is.numeric(result$score))
  expect_true(is.list(result$scores))
  expect_true(is.list(result$temperatures))
  expect_true(is.numeric(result$iterations_run))
  expect_true(is.logical(result$stopped_early))
  expect_true(is.list(result$treatments))

  expect_named(result$scores, c("wp", "sp", "ssp"))
  expect_named(result$temperatures, c("wp", "sp", "ssp"))
  expect_named(result$stopped_early, c("wp", "sp", "ssp"))
  expect_named(result$treatments, c("wp", "sp", "ssp"))

  expect_equal(nrow(result$design_df), 144) # 16 rows × 9 cols
  expect_equal(ncol(result$design_df), 9) # All columns plus `treatment`
  expect_equal(length(result$scores), 3)
  expect_equal(sapply(result$scores, length), c(wp = 229, sp = 201, ssp = 1000))

  expect_equal(result$score, 393)
  expect_equal(result$iterations_run, 1430)
  expect_equal(result$stopped_early, c(wp = TRUE, sp = TRUE, ssp = FALSE))
  expect_equal(result$seed, 42)

  expect_equal(result$treatments$wp, c("A", "B", "C"))
  expect_equal(result$treatments$sp, c("a", "b", "c", "d"))
  expect_equal(result$treatments$ssp, c("x", "y", "z"))

  vdiffr::expect_doppelganger(
    "speed_split_split_wp",
    autoplot(result, treatments = "wholeplot_treatment")
  )
  vdiffr::expect_doppelganger(
    "speed_split_split_sp",
    autoplot(result, treatments = "subplot_treatment")
  )
  vdiffr::expect_doppelganger(
    "speed_split_split_ssp",
    autoplot(result, treatments = "subsubplot_treatment")
  )
})

test_that("speed handles strip plot designs", {
  df_strip <- data.frame(
    row = rep(1:12, each = 6), # 12 rows total (4 rows per block x 6 blocks)
    col = rep(1:6, times = 12), # 6 columns repeated
    block = rep(rep(1:2, each = 3), times = 4) + rep(0:2 * 2, each = 24), # 6 blocks, 12 plots each
    vertical_treatment = rep(rep(LETTERS[1:3], times = 2), times = 12), # A, B, C
    horizontal_treatment = rep(rep(letters[1:4], each = 6), times = 3), # a, b, c, d
    plot_in_block = rep(1:12, times = 6)
  )

  result <- speed(
    df_strip,
    swap = list(ht = "horizontal_treatment", vt = "vertical_treatment"),
    swap_within = list(ht = "block", vt = "block"),
    iterations = list(ht = 500, vt = 500),
    early_stop_iterations = list(ht = 200, vt = 200),
    swap_all = TRUE,
    seed = 42,
    quiet = TRUE
  )

  expect_s3_class(result, "design")

  expect_named(
    result,
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

  expect_true(is.data.frame(result$design_df))
  expect_true(is.numeric(result$score))
  expect_true(is.list(result$scores))
  expect_true(is.list(result$temperatures))
  expect_true(is.numeric(result$iterations_run))
  expect_true(is.logical(result$stopped_early))
  expect_true(is.list(result$treatments))

  expect_named(result$scores, c("ht", "vt"))
  expect_named(result$temperatures, c("ht", "vt"))
  expect_named(result$stopped_early, c("ht", "vt"))
  expect_named(result$treatments, c("ht", "vt"))

  expect_equal(nrow(result$design_df), 72) # 16 rows × 9 cols
  expect_equal(ncol(result$design_df), 7) # All columns plus `treatment`
  expect_equal(length(result$scores), 2)
  expect_equal(sapply(result$scores, length), c(ht = 232, vt = 369))

  expect_equal(result$score, 138)
  expect_equal(result$iterations_run, 601)
  expect_equal(result$stopped_early, c(ht = TRUE, vt = TRUE))
  expect_equal(result$seed, 42)

  expect_equal(result$treatments$ht, c("a", "b", "c", "d"))
  expect_equal(result$treatments$vt, c("A", "B", "C"))

  vdiffr::expect_doppelganger(
    "speed_strip_ht",
    autoplot(result, treatments = "horizontal_treatment")
  )
  vdiffr::expect_doppelganger(
    "speed_strip_vt",
    autoplot(result, treatments = "vertical_treatment")
  )
})

test_that("speed handles MET", {
  # 5 sites, 100 treatments, 7 total reps
  # 5x28x5
  treatments <- rep(1:100, 7)
  df_site <- initialise_design_df(1, 28, 5, 14, 5)
  df_initial <- rbind(df_site, df_site, df_site, df_site, df_site)
  df_initial$treatment <- treatments
  df_initial$site <- rep(c("a", "b", "c", "d", "e"), each = 140)

  # will be handled in speed function
  df_initial$site_row <- paste(df_initial$site, df_initial$row, sep = "_")
  df_initial$site_col <- paste(df_initial$site, df_initial$col, sep = "_")
  df_initial$site_block <- paste(df_initial$site, df_initial$block, sep = "_")

  optimise <- list(
    connectivity = list(spatial_factors = ~site),
    balance = list(
      swap_within = "site",
      spatial_factors = ~ site_col + site_block
    )
  )

  speed_design <- speed(
    data = df_initial,
    swap = "treatment",
    optimise = optimise,
    optimise_params = optim_params(random_initialisation = TRUE),
    adj_weight = 0,
    seed = 112,
    quiet = TRUE
  )
  design_df <- speed_design$design_df

  expect_equal(sort(design_df$treatment), sort(treatments))
  expect_setequal(unique(table(design_df$treatment, design_df$site)), c(1, 2))
  expect_equal(
    unique(apply(table(design_df$treatment, design_df$site), 1, var)),
    0.3
  )
  expect_equal(max(table(design_df$site_row, design_df$treatment)), 1)
  expect_equal(max(table(design_df$site_col, design_df$treatment)), 1)
})

test_that("speed handles MET with unequal site dimensions", {
  # 5 sites, 57 treatments, 7-8 total reps
  # 28x5, 20x3, 20x4, 15x4, 22x3
  # all_treatments <- c(rep(1:50, 7), rep(51:57, 8))
  locked_treatments <- c(rep(1:31, 2), rep(32:57, 3))
  treatments <- c(rep(1:31, 5), rep(32:50, 4), rep(51:57, 5))

  df_site_1 <- initialise_design_df(locked_treatments, 28, 5, 7, 5)
  df_site_1$site <- "a"
  df_site_2 <- initialise_design_df(1, 20, 3, 10, 3)
  df_site_2$site <- "b"
  df_site_3 <- initialise_design_df(1, 20, 4, 10, 4)
  df_site_3$site <- "c"
  df_site_4 <- initialise_design_df(1, 15, 4, 5, 4)
  df_site_4$site <- "d"
  df_site_5 <- initialise_design_df(1, 22, 3, 11, 3)
  df_site_5$site <- "e"

  df_initial <- rbind(df_site_1, df_site_2, df_site_3, df_site_4, df_site_5)
  df_initial[df_initial$site != "a", ]$treatment <- treatments
  df_initial$swappable_site <- df_initial$site != "a"

  df_initial$site_row <- paste(df_initial$site, df_initial$row, sep = "_")
  df_initial$site_col <- paste(df_initial$site, df_initial$col, sep = "_")
  df_initial$site_block <- paste(df_initial$site, df_initial$block, sep = "_")

  optimise <- list(
    connectivity = list(
      swap_within = "swappable_site",
      spatial_factors = ~site
    ),
    balance = list(
      swap_within = "site",
      spatial_factors = ~ site_col + site_block
    )
  )

  speed_design <- speed(
    data = df_initial,
    swap = "treatment",
    early_stop_iterations = 5000,
    optimise = optimise,
    optimise_params = optim_params(random_initialisation = 50),
    adj_weight = 0,
    seed = 112,
    quiet = TRUE
  )
  design_df <- speed_design$design_df

  expect_setequal(
    unique(table(
      design_df[design_df$site == "a", ]$treatment,
      design_df[design_df$site == "a", ]$site
    )),
    c(2, 3)
  )
  expect_setequal(
    unique(table(
      design_df[design_df$site != "a", ]$treatment,
      design_df[design_df$site != "a", ]$site
    )),
    c(1, 2)
  )
  expect_setequal(
    table(design_df$treatment, design_df$site) |>
      apply(1, var) |>
      round(3) |>
      unique(),
    c(0.3, 0.8)
  )
  expect_equal(max(table(design_df$site_row, design_df$treatment)), 1)
  expect_equal(max(table(design_df$site_col, design_df$treatment)), 1)
})

test_that("multi-swap designs gain a combined treatment column", {
  df_split <- data.frame(
    row = rep(1:12, each = 4),
    col = rep(1:4, times = 12),
    block = rep(1:4, each = 12),
    wholeplot = rep(1:12, each = 4),
    wholeplot_treatment = factor(rep(rep(LETTERS[1:3], each = 4), times = 4)),
    subplot_treatment = rep(letters[1:4], 12)
  )

  result <- speed(
    df_split,
    swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
    swap_within = list(wp = "block", sp = "wholeplot"),
    iterations = 50,
    swap_all = TRUE,
    seed = 1,
    quiet = TRUE
  )
  design_df <- result$design_df

  expect_equal(names(design_df), c(names(df_split), "treatment"))
  expect_type(design_df$treatment, "character")
  expect_equal(
    design_df$treatment,
    paste(design_df$wholeplot_treatment, design_df$subplot_treatment, sep = "-")
  )
  # swap columns keep their input types
  expect_s3_class(design_df$wholeplot_treatment, "factor")

  plt <- autoplot(result)
  expect_s3_class(plt, "ggplot")
  expect_equal(rlang::as_label(plt$layers[[1]]$mapping$fill), "treatment")
  expect_setequal(
    levels(plt$layers[[1]]$data$treatment),
    unique(design_df$treatment)
  )
})

test_that("an existing treatment column is not overwritten", {
  df_split <- data.frame(
    row = rep(1:12, each = 4),
    col = rep(1:4, times = 12),
    block = rep(1:4, each = 12),
    wholeplot = rep(1:12, each = 4),
    wholeplot_treatment = rep(rep(LETTERS[1:3], each = 4), times = 4),
    subplot_treatment = rep(letters[1:4], 12),
    treatment = "user"
  )

  result <- speed(
    df_split,
    swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
    swap_within = list(wp = "block", sp = "wholeplot"),
    iterations = 50,
    swap_all = TRUE,
    seed = 1,
    quiet = TRUE
  )

  expect_equal(names(result$design_df), names(df_split))
  expect_true(all(result$design_df$treatment == "user"))
})

test_that("designs swapping one column gain no treatment column", {
  df <- data.frame(
    row = rep(1:4, times = 5),
    col = rep(1:5, each = 4),
    variety = rep(LETTERS[1:4], 5)
  )

  simple <- speed(df, swap = "variety", iterations = 50, seed = 1, quiet = TRUE)
  expect_equal(names(simple$design_df), names(df))

  # Same swap column at every level, as in a MET
  multi <- speed(
    df,
    swap = "variety",
    optimise = list(
      one = list(iterations = 50),
      two = list(iterations = 50)
    ),
    seed = 1,
    quiet = TRUE
  )
  expect_equal(names(multi$design_df), names(df))
})
