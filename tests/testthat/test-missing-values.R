test_that("fill_forward uses only earlier observations", {
  x <- c(NA, 1, NA, NA, 4, NA)
  expect_equal(fill_forward(x), c(NA, 1, 1, 1, 4, 4))
})

test_that("handle_missing never uses future values and drops leading gaps", {
  df <- data.frame(a = c(NA, NA, 3, NA, 5, 6), b = c(1, 2, 3, 4, NA, 6))
  out <- handle_missing(df, c("a", "b"))
  expect_equal(nrow(out), 4)
  expect_equal(out$a, c(3, 3, 5, 6))
  expect_equal(out$b, c(3, 4, 4, 6))
  expect_false(anyNA(out))
})

test_that("handle_missing leaves complete data untouched", {
  df <- data.frame(a = 1:5 + 0.5)
  expect_equal(handle_missing(df, "a"), df)
})

test_that("excessive missingness is rejected", {
  df <- data.frame(a = c(1, NA, NA, NA, 5))
  expect_error(handle_missing(df, "a", max_missing_fraction = 0.2), "Missing fraction")
})
