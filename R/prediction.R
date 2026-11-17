predict_model <- function(model, X) {
  if (length(dim(X)) != 3) {
    stop("Error: X must be a 3-dimensional array (samples, window, features).", call. = FALSE)
  }
  as.numeric(predict(model, X, verbose = 0))
}

recursive_horizon_forecast <- function(predict_fn, values, origins, window_size, horizon, target_idx) {
  n_features <- ncol(values)
  if (horizon > 1 && n_features > 1) {
    stop("Error: Multi-step recursive forecasting needs future values of every input; ",
         "only a univariate pipeline supports horizon > 1.", call. = FALSE)
  }
  if (any(origins < window_size) || any(origins + horizon > nrow(values))) {
    stop("Error: Forecast origins must satisfy window_size <= origin and origin + horizon <= nrow(values).", call. = FALSE)
  }
  n_origins <- length(origins)
  windows <- array(0, dim = c(n_origins, window_size, n_features))
  for (i in seq_len(n_origins)) {
    windows[i, , ] <- values[(origins[i] - window_size + 1):origins[i], , drop = FALSE]
  }
  result <- matrix(NA_real_, nrow = n_origins, ncol = horizon)
  for (h in seq_len(horizon)) {
    p <- as.numeric(predict_fn(windows))
    if (length(p) != n_origins) {
      stop("Error: Prediction function returned ", length(p), " values for ", n_origins, " windows.", call. = FALSE)
    }
    result[, h] <- p
    if (h < horizon) {
      if (window_size > 1) {
        windows[, 1:(window_size - 1), ] <- windows[, 2:window_size, , drop = FALSE]
      }
      windows[, window_size, target_idx] <- p
    }
  }
  result
}

forecast_future <- function(model, last_sequence, scaler, target_col, n_steps,
                            window_size, col_order, future_features = NULL) {
  if (!is.numeric(n_steps) || length(n_steps) != 1 || n_steps < 1 || n_steps != round(n_steps)) {
    stop("Error: n_steps must be a positive integer.", call. = FALSE)
  }
  if (!is.matrix(last_sequence) || nrow(last_sequence) != window_size || ncol(last_sequence) != length(col_order)) {
    stop("Error: last_sequence must be a window_size x n_features matrix matching col_order.", call. = FALSE)
  }
  if (anyNA(last_sequence)) {
    stop("Error: last_sequence contains missing values.", call. = FALSE)
  }
  if (!target_col %in% col_order) {
    stop("Error: target_col is not part of col_order.", call. = FALSE)
  }
  target_idx <- match(target_col, col_order)
  other_idx <- setdiff(seq_along(col_order), target_idx)
  other_cols <- col_order[other_idx]

  if (length(other_idx) > 0) {
    if (is.null(future_features)) {
      stop("Error: Multivariate forecasting requires future values of: ",
           paste(other_cols, collapse = ", "),
           ". None were supplied, so no forecast is produced. Use a univariate configuration instead.",
           call. = FALSE)
    }
    if (!all(other_cols %in% colnames(future_features))) {
      stop("Error: future_features must contain columns: ", paste(other_cols, collapse = ", "), call. = FALSE)
    }
    future_matrix <- as.matrix(future_features[, other_cols, drop = FALSE])
    if (nrow(future_matrix) < n_steps || anyNA(future_matrix)) {
      stop("Error: future_features must provide at least n_steps complete rows.", call. = FALSE)
    }
  }

  current <- last_sequence
  predictions <- numeric(n_steps)
  for (i in seq_len(n_steps)) {
    X_pred <- array(current, dim = c(1, nrow(current), ncol(current)))
    pred_value <- as.numeric(predict(model, X_pred, verbose = 0))
    predictions[i] <- pred_value
    new_row <- numeric(ncol(current))
    new_row[target_idx] <- pred_value
    if (length(other_idx) > 0) {
      new_row[other_idx] <- future_matrix[i, ]
    }
    current <- rbind(current[-1, , drop = FALSE], matrix(new_row, nrow = 1))
  }
  inverse_scale_vector(predictions, scaler, target_col)
}
