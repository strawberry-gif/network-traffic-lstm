library(here)

config <- list(
  data_path = here("data", "raw", "network_traffic.csv"),
  allow_synthetic_generation = TRUE,
  synthetic_points = 2000,
  timestamp_col = "timestamp",
  target_col = "traffic_volume",
  feature_cols = c("traffic_volume"),
  column_map = NULL,
  timestamp_formats = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d"),
  duplicate_strategy = "first",
  irregular_action = "warn",
  interval_tolerance = 0.01,
  regularize_grid = FALSE,
  max_missing_fraction = 0.2,
  window_size = 10,
  forecast_steps = 10,
  horizon_stride = 1,
  train_ratio = 0.70,
  val_ratio = 0.15,
  test_ratio = 0.15,
  lstm_units = 50,
  dropout_rate = 0.2,
  activation = "tanh",
  output_activation = "linear",
  epochs = 50,
  batch_size = 16,
  learning_rate = 0.001,
  early_stopping_patience = 5,
  reduce_lr_patience = 3,
  reduce_lr_factor = 0.5,
  min_lr = 1e-6,
  seeds = c(42, 123, 2024, 7, 99),
  deterministic = FALSE,
  verbose = 1,
  run_arima = TRUE,
  arima_max_p = 2,
  arima_max_d = 1,
  arima_max_q = 2,
  model_save_path = here("models", "lstm_model.keras"),
  scaler_save_path = here("models", "scaler.rds"),
  checkpoint_dir = here("models", "checkpoints"),
  plot_dir = here("plots"),
  results_dir = here("results")
)

ensure_directories <- function(config) {
  dirs <- c(
    dirname(config$data_path),
    dirname(config$model_save_path),
    dirname(config$scaler_save_path),
    config$checkpoint_dir,
    config$plot_dir,
    config$results_dir
  )
  for (d in unique(dirs)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(unique(dirs))
}

validate_config <- function(config) {
  required <- c(
    "data_path", "timestamp_col", "target_col", "feature_cols", "timestamp_formats",
    "duplicate_strategy", "irregular_action", "interval_tolerance", "regularize_grid",
    "max_missing_fraction", "window_size", "forecast_steps", "horizon_stride",
    "train_ratio", "val_ratio", "test_ratio", "lstm_units", "dropout_rate",
    "activation", "output_activation", "epochs", "batch_size", "learning_rate",
    "early_stopping_patience", "reduce_lr_patience", "reduce_lr_factor", "min_lr",
    "seeds", "run_arima", "model_save_path", "scaler_save_path", "checkpoint_dir",
    "plot_dir", "results_dir"
  )
  missing_keys <- setdiff(required, names(config))
  if (length(missing_keys) > 0) {
    stop("Invalid config: missing entries: ", paste(missing_keys, collapse = ", "), call. = FALSE)
  }
  if (!is.character(config$target_col) || length(config$target_col) != 1) {
    stop("Invalid config: target_col must be a single column name.", call. = FALSE)
  }
  if (!is.character(config$timestamp_col) || length(config$timestamp_col) != 1) {
    stop("Invalid config: timestamp_col must be a single column name.", call. = FALSE)
  }
  if (!is.character(config$feature_cols) || length(config$feature_cols) < 1) {
    stop("Invalid config: feature_cols must be a non-empty character vector.", call. = FALSE)
  }
  ratios <- c(config$train_ratio, config$val_ratio, config$test_ratio)
  if (any(!is.finite(ratios)) || any(ratios <= 0) || abs(sum(ratios) - 1) > 1e-6) {
    stop("Invalid config: train, validation and test ratios must be positive and sum to 1.", call. = FALSE)
  }
  positive_integers <- c("window_size", "forecast_steps", "horizon_stride", "epochs",
                         "batch_size", "lstm_units", "early_stopping_patience",
                         "reduce_lr_patience")
  for (key in positive_integers) {
    value <- config[[key]]
    if (!is.numeric(value) || length(value) != 1 || !is.finite(value) || value < 1 || value != round(value)) {
      stop("Invalid config: ", key, " must be a positive integer.", call. = FALSE)
    }
  }
  if (!is.numeric(config$dropout_rate) || config$dropout_rate < 0 || config$dropout_rate >= 1) {
    stop("Invalid config: dropout_rate must be in [0, 1).", call. = FALSE)
  }
  if (!is.numeric(config$learning_rate) || config$learning_rate <= 0) {
    stop("Invalid config: learning_rate must be positive.", call. = FALSE)
  }
  if (!config$duplicate_strategy %in% c("first", "last", "mean")) {
    stop("Invalid config: duplicate_strategy must be 'first', 'last' or 'mean'.", call. = FALSE)
  }
  if (!config$irregular_action %in% c("warn", "stop", "ignore")) {
    stop("Invalid config: irregular_action must be 'warn', 'stop' or 'ignore'.", call. = FALSE)
  }
  if (!is.numeric(config$max_missing_fraction) || config$max_missing_fraction < 0 || config$max_missing_fraction > 1) {
    stop("Invalid config: max_missing_fraction must be in [0, 1].", call. = FALSE)
  }
  if (!is.numeric(config$seeds) || length(config$seeds) < 1 || anyDuplicated(config$seeds) > 0) {
    stop("Invalid config: seeds must be a non-empty vector of unique numbers.", call. = FALSE)
  }
  invisible(TRUE)
}
