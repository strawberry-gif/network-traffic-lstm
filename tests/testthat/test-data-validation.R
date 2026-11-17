test_that("missing required columns are rejected", {
  df <- make_traffic_data(50)
  expect_error(validate_required_columns(df, c("timestamp", "not_there")), "Missing required columns")
  expect_error(select_columns(df, "timestamp", c("traffic_volume", "absent")), "Missing required columns")
})

test_that("unparseable timestamps are rejected", {
  df <- make_traffic_data(20)
  df$timestamp[5] <- "not a date"
  expect_error(process_timestamps(df, "timestamp"), "could not be parsed")
})

test_that("unsorted timestamps are put in chronological order", {
  df <- make_traffic_data(20)
  shuffled <- df[c(5:20, 1:4), ]
  out <- process_timestamps(shuffled, "timestamp")
  expect_false(is.unsorted(out$timestamp))
  expect_s3_class(out$timestamp, "POSIXct")
})

test_that("non-numeric columns are rejected and numeric strings are converted", {
  df <- make_traffic_data(20)
  df$traffic_volume <- as.character(df$traffic_volume)
  converted <- validate_numeric_columns(df, "traffic_volume")
  expect_true(is.numeric(converted$traffic_volume))
  df$traffic_volume[3] <- "abc"
  expect_error(validate_numeric_columns(df, "traffic_volume"), "non-numeric")
})

test_that("infinite values become missing values", {
  df <- make_traffic_data(10)
  df$traffic_volume[2] <- Inf
  out <- validate_numeric_columns(df, "traffic_volume")
  expect_true(is.na(out$traffic_volume[2]))
})

test_that("column_map renames source columns to canonical names", {
  df <- make_traffic_data(10)
  colnames(df) <- c("time", "pkts", "octets", "volume")
  mapped <- apply_column_map(df, c(timestamp = "time", traffic_volume = "volume"))
  expect_true(all(c("timestamp", "traffic_volume") %in% colnames(mapped)))
  expect_error(apply_column_map(df, c(timestamp = "missing_col")), "missing source columns")
})

test_that("config validation catches inconsistent settings", {
  cfg <- make_test_config()
  expect_true(validate_config(cfg))
  bad_ratio <- cfg
  bad_ratio$train_ratio <- 0.9
  expect_error(validate_config(bad_ratio), "sum to 1")
  bad_window <- cfg
  bad_window$window_size <- 0
  expect_error(validate_config(bad_window), "window_size")
  bad_strategy <- cfg
  bad_strategy$duplicate_strategy <- "median"
  expect_error(validate_config(bad_strategy), "duplicate_strategy")
})

test_that("target and feature columns are not duplicated", {
  cfg <- make_test_config()
  expect_equal(get_input_cols(cfg), "traffic_volume")
  cfg$feature_cols <- c("packets", "traffic_volume")
  expect_equal(get_input_cols(cfg), c("traffic_volume", "packets"))
})

test_that("a missing data file gives a clear error", {
  expect_error(load_data(file.path(tempdir(), "does_not_exist.csv")), "not found")
})
