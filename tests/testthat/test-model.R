test_that("model accepts (window, features) input and emits one value per sample", {
  skip_if_not(keras_ready(), "keras3 with a Python backend is not available")
  model <- build_lstm_model(window_size = 5, n_features = 2, lstm_units = 4)
  X <- array(runif(8 * 5 * 2), dim = c(8, 5, 2))
  prediction <- predict(model, X, verbose = 0)
  expect_equal(dim(prediction), c(8, 1))
  expect_true(validate_model_io(model, X))
})

test_that("invalid model hyperparameters are rejected", {
  skip_if_not(keras_ready(), "keras3 with a Python backend is not available")
  expect_error(build_lstm_model(5, 1, lstm_units = 0), "positive integers")
  expect_error(build_lstm_model(5, 1, dropout_rate = 1.5), "dropout_rate")
})

test_that("training on small sequences returns a history and a usable model", {
  skip_if_not(keras_ready(), "keras3 with a Python backend is not available")
  cfg <- make_test_config()
  values <- matrix(50 + 20 * sin(seq_len(120) / 10), ncol = 1)
  values <- (values - min(values)) / (max(values) - min(values))
  train_seq <- create_sequences(values, 1, cfg$window_size, 6:80)
  val_seq <- create_sequences(values, 1, cfg$window_size, 81:100)
  run <- train_with_seed(1, train_seq, val_seq, cfg)
  expect_equal(run$status, "ok")
  expect_true(all(c("epoch", "loss", "val_loss") %in% colnames(run$history)))
  expect_true(is.finite(run$best_val_loss))
  test_seq <- create_sequences(values, 1, cfg$window_size, 101:120)
  pred <- predict_model(run$model, test_seq$X)
  expect_equal(length(pred), 20)
})

test_that("a failing training run is reported instead of aborting", {
  skip_if_not(keras_ready(), "keras3 with a Python backend is not available")
  cfg <- make_test_config(list(lstm_units = 0))
  values <- matrix(seq(0, 1, length.out = 60), ncol = 1)
  train_seq <- create_sequences(values, 1, cfg$window_size, 6:40)
  val_seq <- create_sequences(values, 1, cfg$window_size, 41:50)
  suppressMessages(run <- train_with_seed(1, train_seq, val_seq, cfg))
  expect_equal(run$status, "failed")
  expect_true(is.na(run$best_val_loss))
  expect_null(select_best_run(list(run)))
})

test_that("best run selection uses validation loss", {
  skip_if_not(keras_ready(), "keras3 with a Python backend is not available")
  runs <- list(
    list(status = "ok", seed = 1, best_val_loss = 0.5),
    list(status = "failed", seed = 2, best_val_loss = NA_real_),
    list(status = "ok", seed = 3, best_val_loss = 0.2)
  )
  expect_equal(select_best_run(runs)$seed, 3)
})

test_that("history conversion produces one row per epoch", {
  skip_if_not(keras_ready(), "keras3 with a Python backend is not available")
  history <- list(metrics = list(loss = c(0.5, 0.4), val_loss = c(0.6, 0.5)))
  df <- history_to_data_frame(history)
  expect_equal(nrow(df), 2)
  expect_equal(df$val_loss, c(0.6, 0.5))
})
