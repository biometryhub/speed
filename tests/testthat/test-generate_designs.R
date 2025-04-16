test_that("generate_designs produces expected output", {
    nrows <- 5
    ncols <- 8
    treatments <- paste0("T", 1:5)  # 5 treatments
    blocks <- create_block_structure(nrows, ncols, 5, 1)  # 8 blocks of 5x1

    # Run optimization with early stopping
    set.seed(123)  # For reproducibility
    result_sm <- optimize_design(nrows, ncols, treatments, blocks,
                                 iterations = 10000,
                                 start_temp = 100,
                                 cooling_rate = 0.99,
                                 adj_weight = 1,
                                 bal_weight = 1,
                                 early_stop_iterations = 2000, quiet = TRUE)
    # plot_design(result_sm$design)
    expect_snapshot(result_sm)
    vdiffr::expect_doppelganger(title = "simple_design", plot_design(result_sm$design))
    distribution_sm <- evaluate_distribution(result_sm$design, treatments)
    # print("Treatment counts by row:")
    # print(distribution_sm$row_counts)
    expect_equal(as.numeric(table(distribution_sm$row_counts)), c(10, 15))
    expect_equal(as.numeric(table(distribution_sm$col_counts)), 40)
    # print("Treatment counts by column:")
    # print(distribution$col_counts)
    # table(distribution$col_counts)

})

# test_that("generate_designs handles edge cases", {
#   result <- generate_designs(edge_case_input)
#   expected <- edge_case_expected_output
#   expect_equal(result, expected)
# })
