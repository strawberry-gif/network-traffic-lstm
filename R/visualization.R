library(ggplot2)

plot_historical <- function(data, timestamp_col = "timestamp", target_col = "traffic_volume") {
  df <- data.frame(time = data[[timestamp_col]], value = data[[target_col]])
  ggplot(df, aes(x = time, y = value)) +
    geom_line(color = "steelblue") +
    labs(title = "Historical Network Traffic", x = "Time", y = target_col) +
    theme_minimal()
}

plot_loss <- function(history) {
  df <- if (is.data.frame(history)) history else history_to_data_frame(history)
  long <- rbind(
    data.frame(epoch = df$epoch, loss = df$loss, series = "Training Loss"),
    data.frame(epoch = df$epoch, loss = df$val_loss, series = "Validation Loss")
  )
  ggplot(long, aes(x = epoch, y = loss, color = series)) +
    geom_line() +
    scale_color_manual(values = c("Training Loss" = "blue", "Validation Loss" = "red")) +
    labs(title = "Training and Validation Loss", x = "Epoch", y = "Loss") +
    theme_minimal() +
    theme(legend.title = element_blank())
}

plot_actual_vs_predicted <- function(actual, predicted, timestamps = NULL, y_label = "Traffic Volume") {
  df <- rbind(
    data.frame(time = if (is.null(timestamps)) seq_along(actual) else timestamps,
               value = actual, series = "Actual"),
    data.frame(time = if (is.null(timestamps)) seq_along(predicted) else timestamps,
               value = predicted, series = "Predicted")
  )
  ggplot(df, aes(x = time, y = value, color = series, linetype = series)) +
    geom_line(linewidth = 0.8) +
    scale_color_manual(values = c("Actual" = "black", "Predicted" = "red")) +
    scale_linetype_manual(values = c("Actual" = "solid", "Predicted" = "dashed")) +
    labs(title = "Actual vs Predicted Traffic (Test Set, One Step Ahead)",
         x = "Time", y = y_label) +
    theme_minimal() +
    theme(legend.title = element_blank())
}

plot_residuals <- function(actual, predicted, timestamps = NULL) {
  df <- data.frame(
    time = if (is.null(timestamps)) seq_along(actual) else timestamps,
    residual = predicted - actual
  )
  ggplot(df, aes(x = time, y = residual)) +
    geom_hline(yintercept = 0, color = "grey40") +
    geom_line(color = "darkorange") +
    labs(title = "Prediction Residuals (Predicted - Actual)", x = "Time", y = "Residual") +
    theme_minimal()
}

plot_horizon_metrics <- function(horizon_summary, metric = "RMSE") {
  if (!metric %in% colnames(horizon_summary)) {
    stop("Error: Metric '", metric, "' not found in the horizon summary.", call. = FALSE)
  }
  df <- data.frame(horizon = horizon_summary$horizon,
                   value = horizon_summary[[metric]],
                   method = horizon_summary$method)
  ggplot(df, aes(x = horizon, y = value, color = method)) +
    geom_line(linewidth = 0.8) +
    geom_point() +
    labs(title = paste(metric, "by Forecast Horizon (Test Set)"),
         x = "Steps Ahead", y = metric) +
    theme_minimal() +
    theme(legend.title = element_blank())
}

plot_forecast <- function(historical_data, forecast_values, interval_seconds,
                          timestamp_col = "timestamp", target_col = "traffic_volume",
                          context_points = 200) {
  last_time <- tail(historical_data[[timestamp_col]], 1)
  forecast_times <- last_time + seq_along(forecast_values) * interval_seconds
  history_part <- tail(historical_data, context_points)
  df <- rbind(
    data.frame(time = history_part[[timestamp_col]], value = history_part[[target_col]], type = "Historical"),
    data.frame(time = forecast_times, value = forecast_values, type = "Forecast")
  )
  ggplot(df, aes(x = time, y = value, color = type)) +
    geom_line(linewidth = 0.8) +
    labs(title = "Historical Traffic and Future Forecast", x = "Time", y = target_col) +
    scale_color_manual(values = c("Historical" = "steelblue", "Forecast" = "orange")) +
    theme_minimal() +
    theme(legend.title = element_blank())
}
