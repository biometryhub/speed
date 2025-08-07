library(testthat)

# Test cases for the enhanced initialise_design_df function

test_that("initialise_design_df creates basic CRD correctly", {
  design <- initialise_design_df(
    items = 4,
    nrows = 4,
    ncols = 4,
    design_type = "crd",
    seed = 123
  )
  
  expect_s3_class(design, "data.frame")
  expect_equal(nrow(design), 16)
  expect_true(all(c("row", "col", "treatment") %in% names(design)))
  expect_equal(length(unique(design$treatment)), 4)
})

test_that("initialise_design_df creates RCBD correctly", {
  design <- initialise_design_df(
    items = c("A", "B", "C", "D"),
    nrows = 8,
    ncols = 4,
    block_nrows = 2,
    block_ncols = 2,
    design_type = "rcbd",
    seed = 123
  )
  
  expect_s3_class(design, "data.frame")
  expect_equal(nrow(design), 32)
  expect_true(all(c("row", "col", "treatment", "block") %in% names(design)))
  expect_equal(length(unique(design$block)), 8)
})

test_that("initialise_design_df creates split-plot design correctly", {
  design <- initialise_design_df(
    items = list(
      whole_plot = c("A", "B"),
      sub_plot = c("X", "Y", "Z")
    ),
    nrows = 6,
    ncols = 4,
    design_type = "split_plot",
    wp_nrows = 2,
    wp_ncols = 2,
    seed = 123
  )
  
  expect_s3_class(design, "data.frame")
  expect_equal(nrow(design), 24)
  expect_true(all(c("row", "col", "whole_plot", "whole_plot_treatment", 
                   "sub_plot_treatment", "treatment") %in% names(design)))
  expect_equal(length(unique(design$whole_plot)), 6)  # 3 rows × 2 cols of whole plots
  expect_equal(length(unique(design$whole_plot_treatment)), 2)
  expect_equal(length(unique(design$sub_plot_treatment)), 3)
})

test_that("initialise_design_df creates strip-plot design correctly", {
  design <- initialise_design_df(
    items = list(
      row_factor = c("R1", "R2"),
      col_factor = c("C1", "C2", "C3")
    ),
    nrows = 6,
    ncols = 6,
    design_type = "strip_plot",
    wp_nrows = 3,
    wp_ncols = 2,
    seed = 123
  )
  
  expect_s3_class(design, "data.frame")
  expect_equal(nrow(design), 36)
  expect_true(all(c("row", "col", "row_strip", "col_strip", 
                   "row_treatment", "col_treatment", "treatment") %in% names(design)))
  expect_equal(length(unique(design$row_strip)), 2)
  expect_equal(length(unique(design$col_strip)), 3)
})

test_that("initialise_design_df handles split-plot with blocking", {
  design <- initialise_design_df(
    items = list(
      whole_plot = c("WP1", "WP2"),
      sub_plot = c("SP1", "SP2")
    ),
    nrows = 8,
    ncols = 4,
    block_nrows = 4,
    block_ncols = 2,
    design_type = "split_plot",
    wp_nrows = 2,
    wp_ncols = 2,
    seed = 123
  )
  
  expect_s3_class(design, "data.frame")
  expect_equal(nrow(design), 32)
  expect_true(all(c("row", "col", "whole_plot", "whole_plot_treatment", 
                   "sub_plot_treatment", "treatment", "block") %in% names(design)))
  expect_equal(length(unique(design$block)), 4)
})

test_that("initialise_design_df validates split-plot parameters", {
  expect_error(
    initialise_design_df(
      items = list(whole_plot = c("A", "B")),  # Missing sub_plot
      nrows = 6,
      ncols = 4,
      design_type = "split_plot",
      wp_nrows = 2,
      wp_ncols = 2
    ),
    "must be a list with 'whole_plot' and 'sub_plot' elements"
  )
  
  expect_error(
    initialise_design_df(
      items = list(whole_plot = c("A", "B"), sub_plot = c("X", "Y")),
      nrows = 6,
      ncols = 4,
      design_type = "split_plot"
      # Missing wp_nrows and wp_ncols
    ),
    "both `wp_nrows` and `wp_ncols` must be specified"
  )
})

test_that("initialise_design_df validates strip-plot parameters", {
  expect_error(
    initialise_design_df(
      items = list(row_factor = c("R1", "R2")),  # Missing col_factor
      nrows = 6,
      ncols = 6,
      design_type = "strip_plot",
      wp_nrows = 3,
      wp_ncols = 2
    ),
    "must be a list with 'row_factor' and 'col_factor' elements"
  )
})

test_that("initialise_design_df handles reproducibility with seed", {
  design1 <- initialise_design_df(
    items = 4,
    nrows = 4,
    ncols = 4,
    design_type = "crd",
    seed = 123
  )
  
  design2 <- initialise_design_df(
    items = 4,
    nrows = 4,
    ncols = 4,
    design_type = "crd",
    seed = 123
  )
  
  expect_equal(design1, design2)
})

test_that("initialise_design_df handles non-randomized designs", {
  design <- initialise_design_df(
    items = c("A", "B", "C", "D"),
    nrows = 2,
    ncols = 2,
    design_type = "crd",
    randomize = FALSE
  )
  
  expect_equal(design$treatment, c("A", "B", "C", "D"))
})

test_that("initialise_design_df works with large split-plot designs", {
  design <- initialise_design_df(
    items = list(
      whole_plot = LETTERS[1:8],
      sub_plot = paste0("SP", 1:5)
    ),
    nrows = 20,
    ncols = 16,
    design_type = "split_plot",
    wp_nrows = 4,
    wp_ncols = 4,
    seed = 123
  )
  
  expect_s3_class(design, "data.frame")
  expect_equal(nrow(design), 320)
  expect_equal(length(unique(design$whole_plot)), 20)  # 5 × 4 whole plots
})
