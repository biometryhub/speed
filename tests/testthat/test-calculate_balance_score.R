test_that("calculate_balance_score works as expected", {
    # TODO: manually calculate the actual variance
    # 1 2 3
    # 1 2 3
    # 1 2 3
    design <- data.frame(Row = rep(1:3, each = 3),
                         Col = rep(1:3, times = 3),
                         Treatment = rep(LETTERS[1:3], 3))

    expect_equal(calculate_balance_score(design, "Treatment", c("Row", "Col")), 9)

    # 1 3 2
    # 2 1 3
    # 3 2 1
    design <- data.frame(Row = rep(1:3, each = 3),
                         Col = rep(1:3, times = 3),
                         Treatment = c("A", "C", "B",
                                       "B", "A", "C",
                                       "C", "B", "A"))
    expect_equal(calculate_balance_score(design, "Treatment", c("Row", "Col")), 0)
})
