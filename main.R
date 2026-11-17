required_packages <- c("here", "ggplot2", "keras3")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Missing R packages: ", paste(missing_packages, collapse = ", "),
       ". Install them with install.packages() and run keras3::install_keras().", call. = FALSE)
}

library(here)
library(keras3)
library(ggplot2)

source(here("R", "config.R"))
source(here("R", "data_preprocessing.R"))
source(here("R", "sequence_generation.R"))
source(here("R", "model.R"))
source(here("R", "training.R"))
source(here("R", "prediction.R"))
source(here("R", "baselines.R"))
source(here("R", "evaluation.R"))
source(here("R", "visualization.R"))

validate_config(config)
ensure_directories(config)

if (!file.exists(config$data_path)) {
  if (!isTRUE(config$allow_synthetic_generation)) {
    stop("Data file not found at ", config$data_path,
         " and synthetic generation is disabled in config.", call. = FALSE)
  }
  cat("Data file not found. Generating SYNTHETIC dataset (development use only)...\n")
  source(here("synthetic_data_generator.R"))
  generate_synthetic_traffic(n_points = config$synthetic_points, output_file = config$data_path)
}

cat("\n===== PREPROCESSING =====\n")
preprocessed <- preprocess_pipeline(config$data_path, config)
scaler <- preprocessed$scaler
target_col <- config$target_col
input_cols <- preprocessed$input_cols
n_features <- length(input_cols)
boundaries <- preprocessed$boundaries
timestamp_col <- preprocessed$timestamp_col
values <- as.matrix(preprocessed$full_scaled[, input_cols, drop = FALSE])
target_idx <- match(target_col, input_cols)

p_hist <- plot_historical(preprocessed$full_raw, timestamp_col, target_col)
ggsave(file.path(config$plot_dir, "historical_traffic.png"), p_hist, width = 10, height = 5)

cat("\n===== SEQUENCE GENERATION =====\n")
seq_data <- prepare_sequences(preprocessed, config)
cat(sprintf("Sequences: train=%d, val=%d, test=%d (window=%d, features=%d)\n",
            length(seq_data$train$y), length(seq_data$val$y), length(seq_data$test$y),
            config$window_size, n_features))

horizon <- min(config$forecast_steps, boundaries$n - boundaries$val_end)
if (n_features > 1 && horizon > 1) {
  cat("Multivariate inputs have no known future values: horizon evaluation and forecasting are limited to one step.\n")
  horizon <- 1
}

cat("\n===== BASELINES =====\n")
baseline_fns <- baseline_predict_functions(target_idx)
test_origins <- seq_data$test$target_rows - 1
one_step_set <- unscale_forecast_set(
  collect_forecast_set(baseline_fns, values, target_idx, config$window_size, test_origins, 1),
  scaler, target_col
)
test_actual <- inverse_scale_vector(seq_data$test$y, scaler, target_col)
if (!isTRUE(all.equal(as.numeric(one_step_set$actual[, 1]), test_actual))) {
  stop("Alignment check failed: baseline targets differ from test sequence targets.", call. = FALSE)
}

one_step_rows <- list()
for (method in names(one_step_set$predictions)) {
  m <- compute_metrics(test_actual, one_step_set$predictions[[method]][, 1])
  one_step_rows[[length(one_step_rows) + 1]] <- data.frame(
    method = method, horizon = 1, seed = NA_real_,
    MAE = m$MAE, MSE = m$MSE, RMSE = m$RMSE, R2 = m$R2, stringsAsFactors = FALSE
  )
}

arima_pred_original <- NULL
if (isTRUE(config$run_arima)) {
  arima_model <- fit_arima_baseline(values[, target_idx], boundaries, config)
  if (!is.null(arima_model)) {
    cat(sprintf("ARIMA order selected on validation data: (%d, %d, %d)\n",
                arima_model$order[1], arima_model$order[2], arima_model$order[3]))
    arima_pred_scaled <- tryCatch(
      arima_one_step(arima_model, values[, target_idx], seq_data$test$target_rows),
      error = function(e) {
        warning("ARIMA test prediction failed: ", conditionMessage(e), call. = FALSE)
        NULL
      }
    )
    if (!is.null(arima_pred_scaled) && all(is.finite(arima_pred_scaled))) {
      arima_pred_original <- inverse_scale_vector(arima_pred_scaled, scaler, target_col)
      m <- compute_metrics(test_actual, arima_pred_original)
      one_step_rows[[length(one_step_rows) + 1]] <- data.frame(
        method = "ARIMA", horizon = 1, seed = NA_real_,
        MAE = m$MAE, MSE = m$MSE, RMSE = m$RMSE, R2 = m$R2, stringsAsFactors = FALSE
      )
    }
  }
}

horizon_origins <- build_origin_set(boundaries, config$window_size, horizon, config$horizon_stride)
baseline_horizon_set <- unscale_forecast_set(
  collect_forecast_set(baseline_fns, values, target_idx, config$window_size, horizon_origins, horizon),
  scaler, target_col
)
horizon_rows <- list(horizon_metrics_table(baseline_horizon_set))

cat("\n===== TRAINING (multiple seeds) =====\n")
runs <- list()
seed_rows <- list()
for (seed in config$seeds) {
  cat(sprintf("\n----- Seed %d -----\n", as.integer(seed)))
  run <- train_with_seed(seed, seq_data$train, seq_data$val, config)
  if (identical(run$status, "ok")) {
    write.csv(run$history,
              file.path(config$results_dir, sprintf("training_history_seed_%d.csv", as.integer(seed))),
              row.names = FALSE)
    if (length(runs) == 0) {
      validate_model_io(run$model, seq_data$train$X)
    }
    pred_scaled <- predict_model(run$model, seq_data$test$X)
    run$test_pred <- inverse_scale_vector(pred_scaled, scaler, target_col)
    m <- compute_metrics(test_actual, run$test_pred)
    one_step_rows[[length(one_step_rows) + 1]] <- data.frame(
      method = "LSTM", horizon = 1, seed = seed,
      MAE = m$MAE, MSE = m$MSE, RMSE = m$RMSE, R2 = m$R2, stringsAsFactors = FALSE
    )
    seed_rows[[length(seed_rows) + 1]] <- data.frame(
      seed = seed, status = "ok", best_val_loss = run$best_val_loss, epochs_run = run$epochs_run,
      MAE = m$MAE, MSE = m$MSE, RMSE = m$RMSE, R2 = m$R2, error = NA_character_,
      stringsAsFactors = FALSE
    )
    model_ref <- run$model
    lstm_fn <- list(LSTM = function(windows) predict_model(model_ref, windows))
    lstm_set <- unscale_forecast_set(
      collect_forecast_set(lstm_fn, values, target_idx, config$window_size, horizon_origins, horizon),
      scaler, target_col
    )
    seed_horizon <- horizon_metrics_table(lstm_set)
    horizon_rows[[length(horizon_rows) + 1]] <- seed_horizon
  } else {
    seed_rows[[length(seed_rows) + 1]] <- data.frame(
      seed = seed, status = "failed", best_val_loss = NA_real_, epochs_run = 0L,
      MAE = NA_real_, MSE = NA_real_, RMSE = NA_real_, R2 = NA_real_, error = run$error,
      stringsAsFactors = FALSE
    )
  }
  runs[[length(runs) + 1]] <- run
}

seed_table <- do.call(rbind, seed_rows)
write.csv(seed_table, file.path(config$results_dir, "seed_metrics.csv"), row.names = FALSE)

best_run <- select_best_run(runs)
if (is.null(best_run)) {
  stop("All training runs failed. See results/seed_metrics.csv for the error messages. ",
       "If keras3 or TensorFlow is not installed, run keras3::install_keras().", call. = FALSE)
}
cat(sprintf("\nBest validation loss: seed %d (val_loss=%.6f)\n",
            as.integer(best_run$seed), best_run$best_val_loss))

cat("\n===== EVALUATION =====\n")
best_metrics <- compute_metrics(test_actual, best_run$test_pred)
print_metrics(best_metrics)
write.csv(data.frame(Metric = names(best_metrics), Value = unlist(best_metrics)),
          file.path(config$results_dir, "test_metrics.csv"), row.names = FALSE)

one_step_runs <- do.call(rbind, one_step_rows)
comparison <- build_comparison_table(one_step_runs)
write.csv(comparison, file.path(config$results_dir, "model_comparison.csv"), row.names = FALSE)
cat("\nOne-step-ahead comparison on the test set (LSTM aggregated over successful seeds):\n")
print(comparison, row.names = FALSE)

beating <- count_seeds_beating_baseline(one_step_runs, "Naive")
lstm_row <- comparison[comparison$method == "LSTM", ]
if (nrow(lstm_row) == 1 && is.finite(lstm_row$rmse_change_vs_naive_pct)) {
  cat(sprintf("Mean LSTM RMSE is %.2f%% %s than the naive baseline; the LSTM had lower RMSE in %d of %d seeds.\n",
              abs(lstm_row$rmse_change_vs_naive_pct),
              ifelse(lstm_row$rmse_change_vs_naive_pct < 0, "lower", "higher"),
              beating[["beating"]], beating[["total"]]))
  cat("No significance test is applied; interpret differences with the seed spread in mind.\n")
}

horizon_runs <- do.call(rbind, horizon_rows)
horizon_summary <- summarize_metric_runs(horizon_runs)
write.csv(horizon_summary, file.path(config$results_dir, "horizon_metrics.csv"), row.names = FALSE)

analysis <- analyze_predictions(test_actual, best_run$test_pred)
write.csv(analysis, file.path(config$results_dir, "prediction_analysis.csv"), row.names = FALSE)
cat("\nActual vs predicted analysis (best-validation seed):\n")
print(analysis, row.names = FALSE)

test_timestamps <- preprocessed$full_raw[[timestamp_col]][seq_data$test$target_rows]
predictions_df <- data.frame(
  timestamp = test_timestamps,
  actual = test_actual,
  lstm = best_run$test_pred,
  naive = one_step_set$predictions$Naive[, 1],
  moving_average = one_step_set$predictions$MovingAverage[, 1]
)
if (!is.null(arima_pred_original)) {
  predictions_df$arima <- arima_pred_original
}
write.csv(predictions_df, file.path(config$results_dir, "test_predictions.csv"), row.names = FALSE)

cat("\n===== VISUALIZATION =====\n")
ggsave(file.path(config$plot_dir, "training_loss.png"), plot_loss(best_run$history), width = 8, height = 5)
ggsave(file.path(config$plot_dir, "actual_vs_predicted.png"),
       plot_actual_vs_predicted(test_actual, best_run$test_pred, timestamps = test_timestamps),
       width = 10, height = 5)
ggsave(file.path(config$plot_dir, "residuals.png"),
       plot_residuals(test_actual, best_run$test_pred, timestamps = test_timestamps),
       width = 10, height = 4)
ggsave(file.path(config$plot_dir, "horizon_rmse.png"),
       plot_horizon_metrics(horizon_summary, "RMSE"), width = 8, height = 5)

cat("\n===== FUTURE FORECASTING =====\n")
if (n_features == 1) {
  last_matrix <- tail(values, config$window_size)
  forecast_values <- forecast_future(
    model = best_run$model,
    last_sequence = last_matrix,
    scaler = scaler,
    target_col = target_col,
    n_steps = config$forecast_steps,
    window_size = config$window_size,
    col_order = input_cols
  )
  last_time <- tail(preprocessed$full_raw[[timestamp_col]], 1)
  forecast_times <- last_time + seq_along(forecast_values) * preprocessed$interval_seconds
  forecast_df <- data.frame(timestamp = forecast_times, forecast = forecast_values)
  write.csv(forecast_df, file.path(config$results_dir, "forecast.csv"), row.names = FALSE)
  cat("Forecasted values:\n")
  print(forecast_df, row.names = FALSE)
  ggsave(file.path(config$plot_dir, "future_forecast.png"),
         plot_forecast(preprocessed$full_raw, forecast_values, preprocessed$interval_seconds,
                       timestamp_col = timestamp_col, target_col = target_col),
         width = 10, height = 5)
} else {
  cat("Skipped: multivariate inputs have no known future values, so no future forecast is produced.\n")
}

cat("\n===== SAVING ARTIFACTS =====\n")
save_model(best_run$model, config$model_save_path)
saveRDS(scaler, config$scaler_save_path)
saveRDS(config, file.path(config$results_dir, "run_config.rds"))
writeLines(capture.output(sessionInfo()), file.path(config$results_dir, "session_info.txt"))
cat("Model saved to", config$model_save_path, "\n")
cat("Scaler saved to", config$scaler_save_path, "\n")

cat("\n===== PIPELINE COMPLETED SUCCESSFULLY =====\n")
