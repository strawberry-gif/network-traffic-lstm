test_that("create_sequences builds correct windows and aligned targets", {
  values <- cbind(as.numeric(1:10), as.numeric(11:20))
  colnames(values) <- c("a", "b")
  seqs <- create_sequences(values, target_idx = 1, window_size = 3, target_rows = 4:10)
  expect_equal(dim(seqs$X), c(7, 3, 2))
  expect_equal(length(seqs$y), 7)
  expect_equal(seqs$X[1, , 1], c(1, 2, 3))
  expect_equal(seqs$X[1, , 2], c(11, 12, 13))
  expect_equal(seqs$y[1], 4)
  expect_equal(seqs$X[7, , 1], c(7, 8, 9))
  expect_equal(seqs$y[7], 10)
})

test_that("sequence generation rejects invalid input", {
  values <- matrix(as.numeric(1:10), ncol = 1)
  expect_error(create_sequences(values, 1, 3, 3:5), "Target rows")
  expect_error(create_sequences(values, 1, 3, 4:11), "Target rows")
  expect_error(create_sequences(values, 2, 3, 4:5), "Target column")
  expect_error(create_sequences(values, 1, 3, integer(0)), "No target rows")
  values[4, 1] <- NA
  expect_error(create_sequences(values, 1, 3, 5:6), "missing")
})

test_that("future observations never enter earlier windows", {
  values <- matrix(as.numeric(1:10), ncol = 1)
  changed <- values
  changed[8:10, 1] <- 999
  original <- create_sequences(values, 1, 3, 4:7)
  perturbed <- create_sequences(changed, 1, 3, 4:7)
  expect_identical(original$X, perturbed$X)
  expect_identical(original$y, perturbed$y)
})

test_that("get_target_rows follows split boundaries and keeps context", {
  boundaries <- list(train_end = 140, val_end = 170, n = 200)
  rows <- get_target_rows(boundaries, 5)
  expect_equal(range(rows$train), c(6, 140))
  expect_equal(range(rows$val), c(141, 170))
  expect_equal(range(rows$test), c(171, 200))
})

test_that("validation and test sequences reuse preceding history as context", {
  cfg <- make_test_config()
  result <- NULL
  invisible(capture.output(result <- preprocess_pipeline(write_temp_csv(make_traffic_data(200)), cfg)))
  seqs <- prepare_sequences(result, cfg)
  b <- result$boundaries
  values <- result$full_scaled$traffic_volume
  expect_equal(seqs$val$target_rows[1], b$train_end + 1)
  expect_equal(seqs$val$X[1, , 1], values[(b$train_end + 1 - cfg$window_size):b$train_end])
  expect_equal(seqs$test$X[1, , 1], values[(b$val_end + 1 - cfg$window_size):b$val_end])
  expect_equal(length(seqs$val$y), b$val_end - b$train_end)
  expect_equal(length(seqs$test$y), b$n - b$val_end)
})

test_that("input dimensions are validated", {
  cfg <- make_test_config()
  result <- NULL
  invisible(capture.output(result <- preprocess_pipeline(write_temp_csv(make_traffic_data(200)), cfg)))
  seqs <- prepare_sequences(result, cfg)
  expect_true(validate_sequence_dims(seqs$train, cfg$window_size, 1))
  expect_error(validate_sequence_dims(seqs$train, cfg$window_size, 2), "Dimension check failed")
  expect_error(validate_sequence_dims(seqs$train, cfg$window_size + 1, 1), "Dimension check failed")
})

test_that("multivariate configuration yields one feature per input column", {
  cfg <- make_test_config(list(feature_cols = c("traffic_volume", "packets", "bytes")))
  result <- NULL
  invisible(capture.output(result <- preprocess_pipeline(write_temp_csv(make_traffic_data(200)), cfg)))
  seqs <- prepare_sequences(result, cfg)
  expect_equal(dim(seqs$train$X)[3], 3)
  expect_equal(seqs$input_cols[1], "traffic_volume")
})
