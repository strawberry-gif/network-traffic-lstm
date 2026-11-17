run_pipeline_on <- function(df, cfg) {
  result <- NULL
  invisible(capture.output(result <- preprocess_pipeline(write_temp_csv(df), cfg)))
  result
}

test_that("scaler parameters come from training rows only", {
  cfg <- make_test_config()
  df <- make_traffic_data(200)
  result <- run_pipeline_on(df, cfg)
  train_end <- result$boundaries$train_end
  expect_equal(result$scaler$mins[["traffic_volume"]], min(df$traffic_volume[1:train_end]))
  expect_equal(result$scaler$maxs[["traffic_volume"]], max(df$traffic_volume[1:train_end]))
})

test_that("changing test-period data changes neither scaler nor training or validation sequences", {
  cfg <- make_test_config()
  df <- make_traffic_data(200)
  baseline <- run_pipeline_on(df, cfg)
  val_end <- baseline$boundaries$val_end
  altered <- df
  altered$traffic_volume[(val_end + 1):200] <- altered$traffic_volume[(val_end + 1):200] * 10 + 500
  changed <- run_pipeline_on(altered, cfg)
  expect_equal(changed$scaler$mins, baseline$scaler$mins)
  expect_equal(changed$scaler$ranges, baseline$scaler$ranges)
  seq_a <- prepare_sequences(baseline, cfg)
  seq_b <- prepare_sequences(changed, cfg)
  expect_identical(seq_a$train$X, seq_b$train$X)
  expect_identical(seq_a$train$y, seq_b$train$y)
  expect_identical(seq_a$val$X, seq_b$val$X)
  expect_identical(seq_a$val$y, seq_b$val$y)
})

test_that("missing values are filled from the previous observation only", {
  cfg <- make_test_config()
  df <- make_traffic_data(200)
  df$traffic_volume[50] <- NA
  result <- run_pipeline_on(df, cfg)
  expect_equal(result$full_raw$traffic_volume[50], df$traffic_volume[49])
})

test_that("the leakage assertion rejects overlapping boundaries", {
  boundaries <- list(train_end = 100, val_end = 130, n = 160)
  good <- list(
    train = list(target_rows = 6:100),
    val = list(target_rows = 101:130),
    test = list(target_rows = 131:160)
  )
  expect_true(assert_no_future_leakage(good, boundaries))
  bad <- good
  bad$train$target_rows <- 6:105
  expect_error(assert_no_future_leakage(bad, boundaries), "Leakage check failed")
  bad <- good
  bad$test$target_rows <- 130:160
  expect_error(assert_no_future_leakage(bad, boundaries), "Leakage check failed")
})

test_that("split sizes that cannot support the window are rejected", {
  df <- data.frame(v = as.numeric(1:12))
  expect_error(split_data(df, 0.7, 0.15, 0.15, window_size = 10), "too small")
})
