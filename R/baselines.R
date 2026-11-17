baseline_predict_functions <- function(target_idx) {
  list(
    Naive = function(windows) {
      windows[, dim(windows)[2], target_idx]
    },
    MovingAverage = function(windows) {
      apply(windows[, , target_idx, drop = FALSE], 1, mean)
    }
  )
}

arima_one_step <- function(model, series, rows) {
  fit <- stats::arima(
    series,
    order = model$order,
    fixed = model$coefficients,
    transform.pars = FALSE,
    method = "CSS"
  )
  residuals <- as.numeric(stats::residuals(fit))
  as.numeric(series[rows]) - residuals[rows]
}

fit_arima_baseline <- function(series, boundaries, config) {
  train_series <- series[seq_len(boundaries$train_end)]
  val_rows <- (boundaries$train_end + 1):boundaries$val_end
  if (length(train_series) < 30) {
    warning("Training series is too short for ARIMA; ARIMA baseline skipped.", call. = FALSE)
    return(NULL)
  }
  best <- NULL
  best_rmse <- Inf
  for (d in 0:config$arima_max_d) {
    for (p in 0:config$arima_max_p) {
      for (q in 0:config$arima_max_q) {
        order <- c(p, d, q)
        fit <- tryCatch(
          suppressWarnings(stats::arima(train_series, order = order, method = "CSS")),
          error = function(e) NULL
        )
        if (is.null(fit)) {
          next
        }
        candidate <- list(order = order, coefficients = as.numeric(stats::coef(fit)))
        pred <- tryCatch(
          suppressWarnings(arima_one_step(candidate, series[seq_len(boundaries$val_end)], val_rows)),
          error = function(e) NULL
        )
        if (is.null(pred) || any(!is.finite(pred))) {
          next
        }
        rmse <- sqrt(mean((series[val_rows] - pred)^2))
        if (rmse < best_rmse) {
          best_rmse <- rmse
          best <- list(order = order, coefficients = candidate$coefficients, val_rmse = rmse)
        }
      }
    }
  }
  if (is.null(best)) {
    warning("ARIMA fitting failed for every candidate order; ARIMA baseline skipped.", call. = FALSE)
  }
  best
}
