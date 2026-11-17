test_that("irregular intervals and missing intervals are detected", {
  stamps <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC") + c(0, 60, 120, 300, 360)
  info <- detect_timestamp_gaps(stamps)
  expect_equal(info$interval_seconds, 60)
  expect_equal(nrow(info$gaps), 1)
  expect_equal(info$gaps$after_row, 3)
  expect_equal(info$gaps$missing_intervals, 2)
})

test_that("regular timestamps report no gaps", {
  stamps <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC") + (0:9) * 60
  expect_equal(nrow(detect_timestamp_gaps(stamps)$gaps), 0)
})

test_that("interval validation honours the configured action", {
  df <- data.frame(
    timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC") + c(0, 60, 120, 300, 360),
    v = 1:5
  )
  cfg <- make_test_config(list(irregular_action = "stop"))
  expect_error(validate_timestamp_intervals(df, "timestamp", cfg), "irregular")
  cfg$irregular_action <- "warn"
  expect_warning(validate_timestamp_intervals(df, "timestamp", cfg), "irregular")
  cfg$irregular_action <- "ignore"
  expect_warning(invisible(capture.output(validate_timestamp_intervals(df, "timestamp", cfg))), NA)
})

test_that("regularizing inserts missing rows on the grid", {
  df <- data.frame(
    timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC") + c(0, 60, 240),
    v = c(1, 2, 5)
  )
  out <- regularize_timestamps(df, "timestamp", 60)
  expect_equal(nrow(out), 5)
  expect_equal(is.na(out$v), c(FALSE, FALSE, TRUE, TRUE, FALSE))
  expect_equal(diff(as.numeric(out$timestamp)), rep(60, 4))
})

test_that("off-grid timestamps cannot be regularized", {
  df <- data.frame(
    timestamp = as.POSIXct("2026-01-01 00:00:00", tz = "UTC") + c(0, 60, 150),
    v = 1:3
  )
  expect_error(regularize_timestamps(df, "timestamp", 60), "not aligned")
})
