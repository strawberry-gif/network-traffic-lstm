test_that("compute_metrics matches hand-calculated values", {
  m <- compute_metrics(c(1, 2, 3), c(1, 2, 4))
  expect_equal(m$MAE, 1 / 3)
  expect_equal(m$MSE, 1 / 3)
  expect_equal(m$RMSE, sqrt(1 / 3))
  expect_equal(m$R2, 0.5)
})

test_that("perfect predictions give zero error and R2 of one", {
  m <- compute_metrics(c(2, 4, 6), c(2, 4, 6))
  expect_equal(m$MAE, 0)
  expect_equal(m$RMSE, 0)
  expect_equal(m$R2, 1)
})

test_that("metrics validate their inputs", {
  expect_error(compute_metrics(1:3, 1:2), "same length")
  expect_error(compute_metrics(numeric(0), numeric(0)), "empty")
  expect_error(compute_metrics(c(1, NA), c(1, 2)), "missing")
  expect_true(is.na(compute_metrics(c(3, 3, 3), c(3, 3, 4))$R2))
})

test_that("naive and moving average baselines forecast as defined", {
  values <- matrix(as.numeric(1:12), ncol = 1)
  fns <- baseline_predict_functions(1)
  set <- collect_forecast_set(fns, values, 1, window_size = 4, origins = c(4, 8), horizon = 2)
  expect_equal(set$actual, matrix(c(5, 9, 6, 10), nrow = 2))
  expect_equal(set$predictions$Naive, matrix(c(4, 8, 4, 8), nrow = 2))
  expect_equal(set$predictions$MovingAverage[, 1], c(2.5, 6.5))
  expect_equal(set$predictions$MovingAverage[, 2], c(mean(c(2, 3, 4, 2.5)), mean(c(6, 7, 8, 6.5))))
})

test_that("recursive forecasting feeds predictions back into the window", {
  values <- matrix(as.numeric(1:10), ncol = 1)
  step_up <- function(windows) windows[, dim(windows)[2], 1] + 1
  out <- recursive_horizon_forecast(step_up, values, origins = c(5, 7), window_size = 3, horizon = 3, target_idx = 1)
  expect_equal(out, matrix(c(6, 8, 7, 9, 8, 10), nrow = 2))
})

test_that("forecast origins and horizons are validated", {
  values <- matrix(as.numeric(1:10), ncol = 1)
  fn <- function(windows) windows[, 1, 1]
  expect_error(recursive_horizon_forecast(fn, values, 2, 3, 1, 1), "origin")
  expect_error(recursive_horizon_forecast(fn, values, 9, 3, 3, 1), "origin")
  two_col <- cbind(values, values)
  expect_error(recursive_horizon_forecast(fn, two_col, 5, 3, 2, 1), "univariate")
})

test_that("origin sets stay inside the test segment", {
  boundaries <- list(train_end = 140, val_end = 170, n = 200)
  origins <- build_origin_set(boundaries, window_size = 5, horizon = 4, stride = 1)
  expect_equal(min(origins), 170)
  expect_equal(max(origins), 196)
  expect_error(build_origin_set(boundaries, 5, 40, 1), "too short")
})

test_that("horizon tables and run summaries aggregate correctly", {
  set <- list(
    actual = matrix(c(1, 2, 3, 2, 3, 4), nrow = 3),
    predictions = list(Model = matrix(c(1, 2, 3, 3, 4, 5), nrow = 3))
  )
  table <- horizon_metrics_table(set)
  expect_equal(nrow(table), 2)
  expect_equal(table$MAE, c(0, 1))
  runs <- data.frame(
    method = rep("LSTM", 2), horizon = 1, MAE = c(1, 3), MSE = c(1, 9), RMSE = c(1, 3), R2 = c(0.5, 0.1)
  )
  summary_df <- summarize_metric_runs(runs)
  expect_equal(summary_df$MAE, 2)
  expect_equal(summary_df$MAE_sd, stats::sd(c(1, 3)))
  expect_equal(summary_df$n_runs, 2)
})

test_that("comparison table reports change against the naive baseline", {
  runs <- data.frame(
    method = c("LSTM", "LSTM", "Naive"), horizon = 1,
    MAE = c(1, 1, 2), MSE = c(1, 1, 4), RMSE = c(1, 1.5, 2), R2 = c(0.5, 0.4, 0.2)
  )
  table <- build_comparison_table(runs)
  lstm <- table[table$method == "LSTM", ]
  expect_equal(lstm$rmse_change_vs_naive_pct, (1.25 - 2) / 2 * 100)
  expect_equal(unname(count_seeds_beating_baseline(runs)), c(2, 2))
})

test_that("prediction analysis summarises bias and spread", {
  out <- analyze_predictions(c(10, 20, 30), c(11, 19, 33))
  expect_equal(out$n, 3)
  expect_equal(out$mean_error, mean(c(1, -1, 3)))
  expect_equal(out$max_abs_error, 3)
  expect_true(out$correlation > 0.9)
})

test_that("ARIMA baseline selects an order and predicts one step ahead", {
  set.seed(1)
  series <- as.numeric(arima.sim(list(ar = 0.7), n = 300))
  boundaries <- list(train_end = 210, val_end = 255, n = 300)
  cfg <- make_test_config()
  model <- fit_arima_baseline(series, boundaries, cfg)
  expect_false(is.null(model))
  pred <- arima_one_step(model, series, 256:300)
  expect_equal(length(pred), 45)
  expect_true(all(is.finite(pred)))
})
