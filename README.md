# Network Traffic Prediction with LSTM in R

End-to-end R pipeline for forecasting network traffic volume with an LSTM (keras3), compared against naive, moving-average and ARIMA baselines. The shipped dataset is synthetic and exists for development and testing only; the project is structured so a real traffic dataset can be used instead.

## Overview

1. Load, validate and clean a time series CSV (timestamps, duplicates, gaps, missing values).
2. Split chronologically into train / validation / test, fitting the min-max scaler on the training part only.
3. Build sliding-window sequences. Validation and test targets use the preceding rows as history, so no target is lost to windowing.
4. Train the LSTM once per random seed, with early stopping, learning-rate reduction and checkpointing.
5. Evaluate on the test set (MAE, MSE, RMSE, R2) against baselines, across seeds and across forecast horizons.
6. Produce a recursive future forecast (univariate configuration only).

## System requirements

- R 4.1 or newer
- Python with TensorFlow, installed through keras3 (`keras3::install_keras()`)
- Runs on CPU; no GPU is required
- Packages: `here`, `ggplot2`, `keras3`, plus `testthat` for the tests

## Installation

```r
install.packages(c("here", "ggplot2", "keras3", "testthat"))
keras3::install_keras()
```

Use ggplot2 3.4.0 or newer (the plots use `linewidth`).

## Running

From the project root:

```
Rscript main.R
```

If `data/raw/network_traffic.csv` is missing and `allow_synthetic_generation` is `TRUE`, a synthetic dataset is generated and clearly reported as synthetic.

## Project layout

```
main.R                        pipeline entry point
synthetic_data_generator.R    synthetic data (development only)
R/config.R                    configuration, validation, directory creation
R/data_preprocessing.R        loading, validation, cleaning, splitting, scaling
R/sequence_generation.R       sliding windows and leakage/dimension checks
R/model.R                     LSTM definition
R/training.R                  seeding, training, history, run selection
R/prediction.R                prediction and recursive forecasting
R/baselines.R                 naive, moving average, ARIMA
R/evaluation.R                metrics, horizon tables, comparisons
R/visualization.R             plots
tests/                        testthat suite
data/raw/                     input data (see data/README.md)
```

## Dataset structure

A CSV with one timestamp column and numeric columns. The default file has `timestamp`, `packets`, `bytes`, `traffic_volume`. Timestamps must parse with one of `timestamp_formats` (or be numeric epoch seconds) and are treated as UTC. See `data/README.md` for requirements and the source register.

## Configuration

All settings live in `R/config.R`.

| Setting | Meaning |
|---------|---------|
| `data_path`, `timestamp_col`, `target_col`, `feature_cols` | Input file and columns. The model input is `unique(c(target_col, feature_cols))`, target first |
| `column_map` | Optional named vector `c(canonical = "source column")` that renames dataset columns |
| `timestamp_formats` | Formats tried in order |
| `duplicate_strategy` | `"first"`, `"last"` or `"mean"` for duplicate timestamps |
| `irregular_action`, `interval_tolerance` | `"warn"`, `"stop"` or `"ignore"` when intervals deviate from the median interval by more than the tolerance |
| `regularize_grid` | Reindex to a complete time grid; gaps become missing values |
| `max_missing_fraction` | Stop if a used column has more missing values than this share |
| `window_size`, `forecast_steps`, `horizon_stride` | Input window, forecast/evaluation horizon, spacing of evaluation origins |
| `train_ratio`, `val_ratio`, `test_ratio` | Chronological split; must sum to 1 |
| `lstm_units`, `dropout_rate`, `activation`, `output_activation`, `learning_rate` | Model |
| `epochs`, `batch_size`, early-stopping and learning-rate-reduction settings | Training |
| `seeds` | One training run per seed |
| `deterministic` | Ask TensorFlow for deterministic ops (may be slower or unsupported for some ops) |
| `run_arima`, `arima_max_p/d/q` | ARIMA baseline and search grid |
| `model_save_path`, `scaler_save_path`, `checkpoint_dir`, `plot_dir`, `results_dir` | Output locations |

## Preprocessing

Required columns are checked, timestamps parsed and sorted, duplicates resolved, interval irregularities reported, columns coerced to numeric (non-numeric text is an error, infinite values become missing), and missing values forward-filled. Forward fill uses earlier observations only; leading rows without an earlier value are dropped. The scaler (min-max) is fitted on the training rows only and applied to all rows. Scaled validation and test values can fall outside [0, 1].

## Model architecture

`LSTM(lstm_units, tanh) -> Dropout(dropout_rate) -> Dense(1, linear)`; Adam optimizer, MSE loss, MAE metric. Input shape is `(window_size, n_features)`; the model predicts the next value of the target.

## Training procedure

For every seed the pipeline sets the R and TensorFlow seeds, trains with early stopping on validation loss (best weights restored), reduces the learning rate on plateau and checkpoints the best epoch to `models/checkpoints/`. A run that fails is recorded with its error message and does not stop the other seeds; if every run fails the pipeline stops with an error. The run with the lowest validation loss is selected for the saved model, `test_metrics.csv`, plots and the future forecast. The model is trained on the training segment only; it is not refit on validation or test data.

## Prediction procedure

- One-step: each test target is predicted from the preceding `window_size` observed rows.
- Multi-step: recursive forecasting feeds each prediction back into the window. It is only available for a univariate configuration (`feature_cols` equal to the target). With additional columns the future values of those columns are unknown, so the pipeline limits evaluation to one step and skips the future forecast rather than reusing stale values. `forecast_future()` accepts a `future_features` data frame (scaled units) if real future inputs are available.
- Predictions are converted back to original units with the training-derived scaler.

## Evaluation

Metrics: MAE, MSE, RMSE, R2, always computed in original units on the chronologically last segment.

- Baselines: naive (last observed value), moving average (mean of the last `window_size` values), ARIMA (order chosen on the validation segment, parameters fitted on the training segment, applied one step ahead; no multi-step ARIMA).
- Repeated runs: one LSTM per seed; results are reported as mean and standard deviation across successful seeds.
- Horizon evaluation: rolling origins across the test segment, 1 to `forecast_steps` steps ahead, for the LSTM and the naive and moving-average baselines. Horizon results use origins that leave room for the full horizon, so horizon 1 there covers slightly fewer points than the main one-step comparison.
- Actual vs predicted analysis: bias, residual spread, largest error, percentage error and correlation.

The pipeline prints how the LSTM compares with the naive baseline but does not claim superiority and applies no significance test.

## Testing

```
Rscript tests/testthat.R
```

Tests cover validation, missing values, duplicates, timestamps, sequence generation, leakage, model input/output dimensions, forecasting, inverse scaling, metrics and baselines. Model tests are skipped when keras3 with a Python backend is not available.

## Output files

| Path | Content |
|------|---------|
| `results/test_metrics.csv` | Test metrics of the selected LSTM |
| `results/seed_metrics.csv` | Per-seed status, validation loss, epochs, test metrics, errors |
| `results/model_comparison.csv` | One-step comparison of LSTM (mean/sd over seeds) and baselines |
| `results/horizon_metrics.csv` | Metrics per method and horizon |
| `results/prediction_analysis.csv` | Actual vs predicted summary |
| `results/test_predictions.csv` | Test actuals and predictions per method |
| `results/forecast.csv` | Future forecast (univariate only) |
| `results/training_history_seed_<seed>.csv` | Training history per seed |
| `results/run_config.rds`, `results/session_info.txt` | Configuration and session used |
| `models/lstm_model.keras`, `models/scaler.rds`, `models/checkpoints/` | Model, scaler, checkpoints |
| `plots/*.png` | Historical series, loss, actual vs predicted, residuals, horizon RMSE, forecast |

## Current limitations

- A single LSTM layer with fixed hyperparameters; no hyperparameter search.
- Multi-step forecasting is recursive and univariate only.
- ARIMA is evaluated one step ahead only.
- The default dataset is synthetic, so reported numbers on it say nothing about real traffic.
- No statistical significance testing between models.
- Bit-for-bit reproducibility across machines is not guaranteed; set `deterministic = TRUE` to request deterministic operations.
- Irregularly sampled data is reported or regularized, not modelled as irregular.
