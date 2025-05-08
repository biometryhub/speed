testthat::test_that("Replication score calculation works", {
    # Example design
    design <- data.frame(
        site = rep(1:3, each = 6),
        treatment = rep(LETTERS[1:3], each = 6)
    )

    # Test with a simple design
    expect_equal(calculate_replication_score(design, "treatment", "site"), 672)
})


# Test case 1: Perfect connectivity
design1 <- data.frame(
    site = rep(1:2, each = 3),
    treatment = rep(c("A", "B", "C"), 2)
)
score1 <- calculate_connectivity_score(design1, "treatment", "site")
print(paste("Test Case 1 Score:", score1))  # Expected: 3

# Test case 2: Uneven connectivity
design2 <- data.frame(
    site = c(1, 1, 1, 2, 2, 2),
    treatment = c("A", "A", "A", "B", "B", "B")
)
score2 <- calculate_connectivity_score(design2, "treatment", "site")
print(paste("Test Case 2 Score:", score2))  # Expected: 1

# Test case 3: Minimum replication not met
design3 <- data.frame(
    site = c(1, 2),
    treatment = c("A", "B")
)
score3 <- calculate_connectivity_score(design3, "treatment", "site")
print(paste("Test Case 3 Score:", score3))  # Expected: 0

# Test case 4: Three sites, uneven distribution
design4 <- data.frame(
    site = rep(1:3, each = 2),
    treatment = c("A", "A", "B", "B", "C", "C")
)
score4 <- calculate_connectivity_score(design4, "treatment", "site")
print(paste("Test Case 4 Score:", score4))  # Expected: 3

# Test case 5: Three sites, one treatment missing
design5 <- data.frame(
    site = rep(1:3, each = 2),
    treatment = c("A", "A", "B", "B", "A", "A")
)
score5 <- calculate_connectivity_score(design5, "treatment", "site")
print(paste("Test Case 5 Score:", score5))  # Expected: 2
