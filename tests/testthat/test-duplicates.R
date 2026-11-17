make_duplicate_data <- function() {
  stamps <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC") + c(0, 60, 60, 120)
  data.frame(timestamp = stamps, v = c(1, 2, 4, 5))
}

test_that("duplicate timestamps keep the first observation by default", {
  out <- remove_duplicates(make_duplicate_data(), "timestamp", "first")
  expect_equal(nrow(out), 3)
  expect_equal(out$v, c(1, 2, 5))
})

test_that("duplicate timestamps can keep the last observation", {
  out <- remove_duplicates(make_duplicate_data(), "timestamp", "last")
  expect_equal(out$v, c(1, 4, 5))
})

test_that("duplicate timestamps can be averaged", {
  out <- remove_duplicates(make_duplicate_data(), "timestamp", "mean")
  expect_equal(out$v, c(1, 3, 5))
})

test_that("data without duplicates is unchanged and bad strategies fail", {
  df <- make_duplicate_data()[c(1, 2, 4), ]
  expect_equal(nrow(remove_duplicates(df, "timestamp")), 3)
  expect_error(remove_duplicates(df, "timestamp", "median"), "strategy")
})
