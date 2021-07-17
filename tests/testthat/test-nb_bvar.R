dfx <- nb_bvar(
    data = macrodata[, 1:7],
    agc = list(5, 60, TRUE, 1),
    lags = 1,
    overall_tightness = 0.2,
    lag_decay = 1,
    include_intercept = TRUE
)

test_that("Correct pmean", {
  expect_equal(mean(dfx$pmean), 0.9046183)
})

test_that("Correct lpdens", {
  expect_equal(mean(dfx$lpdens), -1.19755317)
})

test_that("Is a data frame", {
  expect_identical(class(dfx), "data.frame")
})