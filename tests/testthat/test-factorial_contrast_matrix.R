test_that("factorial_contrast_matrix builds an orthonormal effect space", {
  treatments <- expand.grid(
    stage = c("early", "late"),
    cultivar = c("A", "B", "C")
  )
  treatments$treatment <- with(treatments, paste(stage, cultivar, sep = ":"))

  contrasts <- factorial_contrast_matrix(
    treatments,
    ~ stage + cultivar
  )

  expect_equal(dim(contrasts), c(3, 6))
  expect_equal(colnames(contrasts), treatments$treatment)
  expect_equal(unname(rowSums(contrasts)), rep(0, 3), tolerance = 1e-12)
  expect_equal(unname(tcrossprod(contrasts)), diag(3), tolerance = 1e-12)
})

test_that("factorial_contrast_matrix supports interactions", {
  treatments <- expand.grid(
    stage = c("early", "late"),
    cultivar = c("A", "B", "C")
  )
  treatments$treatment <- with(treatments, paste(stage, cultivar, sep = ":"))

  contrasts <- factorial_contrast_matrix(
    treatments,
    ~ stage * cultivar
  )

  expect_equal(nrow(contrasts), nrow(treatments) - 1)
  expect_equal(unname(tcrossprod(contrasts)), diag(5), tolerance = 1e-12)
})

test_that("factorial_contrast_matrix validates its inputs", {
  treatments <- data.frame(
    treatment = c("A", "B"),
    factor_a = c("low", "high")
  )

  expect_error(factorial_contrast_matrix(1:2, ~factor_a), "data frame")
  expect_error(factorial_contrast_matrix(treatments, factor_a ~ 1), "one-sided")
  expect_error(factorial_contrast_matrix(treatments, ~missing), "not found")

  duplicated <- rbind(treatments, treatments[1, ])
  expect_error(
    factorial_contrast_matrix(duplicated, ~factor_a),
    "one non-missing row"
  )
})
