metric_names <- c("MAE", "MSE", "RMSE", "R2")

compute_metrics <- function(actual, predicted) {
  actual <- as.numeric(actual)
  predicted <- as.numeric(predicted)
  if (length(actual) != length(predicted)) {
    stop("Error: actual and predicted must have the same length.", call. = FALSE)
  }
  if (length(actual) == 0) {
    stop("Error: Cannot compute metrics on empty vectors.", call. = FALSE)
  }
  if (anyNA(actual) || anyNA(predicted)) {
    stop("Error: actual and predicted must not contain missing values.", call. = FALSE)
  }
  mae <- mean(abs(actual - predicted))
  mse <- mean((actual - predicted)^2)
  rmse <- sqrt(mse)
  ss_res <- sum((actual - predicted)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  r2 <- if (ss_tot > 0) 1 - (ss_res / ss_tot) else NA_real_

  list(MAE = mae, MSE = mse, RMSE = rmse, R2 = r2)
}

print_metrics <- function(metrics) {
  cat("Evaluation Metrics:\n")
  cat(sprintf("MAE : %.4f\n", metrics$MAE))
  cat(sprintf("MSE : %.4f\n", metrics$MSE))
  cat(sprintf("RMSE: %.4f\n", metrics$RMSE))
  cat(sprintf("R2  : %.4f\n", metrics$R2))
}

build_origin_set <- function(boundaries, window_size, horizon, stride) {
  first_origin <- max(boundaries$val_end, window_size)
  last_origin <- boundaries$n - horizon
  if (last_origin < first_origin) {
    stop("Error: The test segment is too short for the requested forecast horizon.", call. = FALSE)
  }
  seq.int(first_origin, last_origin, by = stride)
}

collect_forecast_set <- function(predict_fns, values, target_idx, window_size, origins, horizon) {
  actual <- matrix(NA_real_, nrow = length(origins), ncol = horizon)
  for (h in seq_len(horizon)) {
    actual[, h] <- values[origins + h, target_idx]
  }
  predictions <- lapply(predict_fns, function(fn) {
    recursive_horizon_forecast(fn, values, origins, window_size, horizon, target_idx)
  })
  list(origins = origins, actual = actual, predictions = predictions)
}

unscale_forecast_set <- function(forecast_set, scaler, target_col) {
  forecast_set$actual <- inverse_scale_vector(forecast_set$actual, scaler, target_col)
  forecast_set$predictions <- lapply(forecast_set$predictions, function(m) {
    inverse_scale_vector(m, scaler, target_col)
  })
  forecast_set
}

horizon_metrics_table <- function(forecast_set) {
  rows <- list()
  for (method in names(forecast_set$predictions)) {
    for (h in seq_len(ncol(forecast_set$actual))) {
      m <- compute_metrics(forecast_set$actual[, h], forecast_set$predictions[[method]][, h])
      rows[[length(rows) + 1]] <- data.frame(
        method = method, horizon = h,
        MAE = m$MAE, MSE = m$MSE, RMSE = m$RMSE, R2 = m$R2,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

summarize_metric_runs <- function(df) {
  keys <- unique(df[, c("method", "horizon")])
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    selected <- df$method == keys$method[i] & df$horizon == keys$horizon[i]
    out <- data.frame(method = keys$method[i], horizon = keys$horizon[i],
                      n_runs = sum(selected), stringsAsFactors = FALSE)
    for (m in metric_names) {
      out[[m]] <- mean(df[[m]][selected])
      out[[paste0(m, "_sd")]] <- if (sum(selected) > 1) stats::sd(df[[m]][selected]) else NA_real_
    }
    out
  })
  do.call(rbind, rows)
}

build_comparison_table <- function(one_step_runs) {
  summary_df <- summarize_metric_runs(one_step_runs)
  naive_rmse <- summary_df$RMSE[summary_df$method == "Naive"]
  if (length(naive_rmse) == 1 && is.finite(naive_rmse) && naive_rmse > 0) {
    summary_df$rmse_change_vs_naive_pct <- (summary_df$RMSE - naive_rmse) / naive_rmse * 100
  } else {
    summary_df$rmse_change_vs_naive_pct <- NA_real_
  }
  summary_df
}

count_seeds_beating_baseline <- function(one_step_runs, baseline = "Naive") {
  lstm <- one_step_runs[one_step_runs$method == "LSTM", ]
  reference <- one_step_runs$RMSE[one_step_runs$method == baseline]
  if (nrow(lstm) == 0 || length(reference) != 1) {
    return(c(beating = NA_integer_, total = nrow(lstm)))
  }
  c(beating = sum(lstm$RMSE < reference), total = nrow(lstm))
}

analyze_predictions <- function(actual, predicted) {
  actual <- as.numeric(actual)
  predicted <- as.numeric(predicted)
  if (length(actual) != length(predicted) || length(actual) == 0) {
    stop("Error: actual and predicted must be non-empty and equally long.", call. = FALSE)
  }
  errors <- predicted - actual
  nonzero <- abs(actual) > 1e-8
  relative <- abs(errors[nonzero]) / abs(actual[nonzero])
  correlation <- if (stats::sd(actual) > 0 && stats::sd(predicted) > 0) stats::cor(actual, predicted) else NA_real_
  data.frame(
    n = length(actual),
    mean_error = mean(errors),
    sd_error = if (length(errors) > 1) stats::sd(errors) else NA_real_,
    max_abs_error = max(abs(errors)),
    mape_pct = if (any(nonzero)) mean(relative) * 100 else NA_real_,
    share_within_10pct = if (any(nonzero)) mean(relative <= 0.10) else NA_real_,
    correlation = correlation
  )
}
