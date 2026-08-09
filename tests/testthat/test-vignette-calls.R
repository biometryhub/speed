# The vignettes are pre-computed (see data-raw/precompute-vignettes.R), so their
# code is no longer executed by `R CMD check` on any platform. That saves
# several minutes per run, but it means an argument rename could land without
# anything noticing until someone regenerates the vignettes.
#
# These mirror the distinctive call shapes from each vignette on a tiny budget.
# They are not about design quality - only that the call still works - so the
# iteration counts are deliberately far below what the vignettes use.

test_that("speed.qmd call shapes still work", {
  crd_design <- initialise_design_df(items = 8, nrows = 8, ncols = 4)
  crd <- speed(
    crd_design,
    swap = "treatment",
    iterations = 50,
    seed = 42,
    quiet = TRUE
  )

  expect_s3_class(crd, "design")
  expect_s3_class(autoplot(crd), "ggplot")

  s <- summary(crd)
  expect_s3_class(s, "summary.design")
  expect_false(is.null(s$per_level[[1]]$score))
  expect_s3_class(summary(crd, efficiency = TRUE), "summary.design")

  rcbd_design <- initialise_design_df(
    items = paste0("V", 1:6),
    nrows = 4,
    ncols = 6,
    block_nrows = 1,
    block_ncols = 6
  )
  rcbd <- speed(
    rcbd_design,
    swap = "treatment",
    swap_within = "block",
    iterations = 50,
    seed = 42,
    quiet = TRUE
  )
  expect_s3_class(rcbd, "design")
})

test_that("complex_designs.qmd split-split call shape still works", {
  splits <- list(
    subsubplot = list(items = c("x", "y", "z"), nrows = 1, ncols = 1),
    subplot = list(items = c("a", "b", "c", "d"), nrows = 1, ncols = 3),
    wholeplot = list(items = c("A", "B", "C"), nrows = 4, ncols = 3),
    block = list(nrows = 4, ncols = 9)
  )
  split_split_df <- initialise_split_design_df(
    splits = splits,
    rep_dim = c(4, 1)
  )
  expect_s3_class(split_split_df, "data.frame")

  optimise <- list(
    wp = list(
      swap = "wholeplot_treatment",
      swap_within = "block",
      iterations = 20,
      early_stop_iterations = 10,
      swap_all = TRUE
    ),
    sp = list(
      swap = "subplot_treatment",
      swap_within = "wholeplot",
      iterations = 20,
      early_stop_iterations = 10,
      swap_all = TRUE
    ),
    ssp = list(
      swap = "subsubplot_treatment",
      swap_within = "subplot",
      iterations = 20,
      early_stop_iterations = 10
    )
  )
  res <- speed(split_split_df, optimise = optimise, seed = 42, quiet = TRUE)

  expect_s3_class(res, "design")
  expect_s3_class(
    autoplot(res, treatments = "subplot_treatment", block = "wholeplot"),
    "ggplot"
  )
})

test_that("complex_designs.qmd strip plot call shape still works", {
  df_strip <- data.frame(
    row = rep(1:12, each = 6),
    col = rep(1:6, times = 12),
    block = rep(rep(1:2, each = 3), times = 4) + rep(0:2 * 2, each = 24),
    vertical_treatment = rep(rep(LETTERS[1:3], times = 2), times = 12),
    horizontal_treatment = rep(rep(letters[1:4], each = 6), times = 3)
  )

  strip_result <- speed(
    df_strip,
    swap = list(ht = "horizontal_treatment", vt = "vertical_treatment"),
    swap_within = list(ht = "block", vt = "block"),
    iterations = list(ht = 20, vt = 20),
    swap_all = TRUE,
    seed = 42,
    quiet = TRUE
  )

  expect_s3_class(strip_result, "design")
  expect_s3_class(
    autoplot(strip_result, treatments = "horizontal_treatment"),
    "ggplot"
  )
})

test_that("factorial.qmd call shape still works", {
  treatments <- with(
    expand.grid(paste0("A", 1:8), paste0("B", 1:3)),
    paste(Var1, Var2, sep = "-")
  )
  factorial_design <- initialise_design_df(treatments, 24, 3, 8, 3)

  factorial_result <- speed(
    data = factorial_design,
    swap = "treatment",
    swap_within = "block",
    spatial_factors = ~ row + col,
    obj_function = objective_function_factorial,
    optimise_params = optim_params(
      adaptive_swaps = TRUE,
      swap_all_blocks = TRUE,
      cooling_rate = 0.999
    ),
    early_stop_iterations = 20,
    iterations = 50,
    interaction_weight = 10,
    seed = 112,
    quiet = TRUE
  )

  expect_s3_class(factorial_result, "design")
})

test_that("custom_objective_functions.qmd call shape still works", {
  bibd_df <- initialise_design_df(
    items = 5,
    nrows = 3,
    ncols = 10,
    block_nrows = 3,
    block_ncols = 1
  )

  # a user-supplied objective returning the documented list(score = ...)
  bibd_objective_function <- function(layout_df, swap, spatial_cols, ...) {
    return(list(
      score = calculate_balance_score(layout_df, swap, spatial_cols)
    ))
  }

  result <- speed(
    bibd_df,
    swap = "treatment",
    spatial_factors = ~ block + row + col,
    obj_function = bibd_objective_function,
    optimise_params = optim_params(random_initialisation = TRUE),
    iterations = 50,
    seed = 42,
    quiet = TRUE
  )

  expect_s3_class(result, "design")
})

test_that("genetic_relationship.qmd call shapes still work", {
  design_df <- initialise_design_df(
    items = 6,
    nrows = 6,
    ncols = 4,
    block_nrows = 6,
    block_ncols = 1
  )
  trts <- sort(unique(as.character(design_df$treatment)))
  rel <- diag(length(trts))
  dimnames(rel) <- list(trts, trts)

  # a bare matrix is accepted and prepped internally
  related_result <- speed(
    design_df,
    swap = "treatment",
    swap_within = "block",
    relationship = rel,
    iterations = 50,
    seed = 42,
    quiet = TRUE
  )
  expect_s3_class(related_result, "design")

  # and the same matrix can be prepped once up front
  prepped <- prep_relationship(rel)
  expect_type(prepped, "list")
  expect_no_error(
    calculate_adjacency_score(related_result$design_df, swap = "treatment")
  )
})
