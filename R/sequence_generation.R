row_range <- function(from, to) {
  if (from > to) integer(0) else seq.int(from, to)
}

get_target_rows <- function(boundaries, window_size) {
  rows <- list(
    train = row_range(window_size + 1, boundaries$train_end),
    val = row_range(boundaries$train_end + 1, boundaries$val_end),
    test = row_range(boundaries$val_end + 1, boundaries$n)
  )
  if (length(rows$train) < 1 || length(rows$val) < 1 || length(rows$test) < 1) {
    stop("Error: Every split must contain at least one target row.", call. = FALSE)
  }
  rows
}

create_sequences <- function(values, target_idx, window_size, target_rows) {
  if (!is.matrix(values) || !is.numeric(values)) {
    stop("Error: values must be a numeric matrix.", call. = FALSE)
  }
  if (anyNA(values)) {
    stop("Error: values contain missing entries; handle them before sequence generation.", call. = FALSE)
  }
  n <- nrow(values)
  n_features <- ncol(values)
  if (length(target_idx) != 1 || is.na(target_idx) || target_idx < 1 || target_idx > n_features) {
    stop("Error: Target column not found in the input matrix.", call. = FALSE)
  }
  if (length(target_rows) < 1) {
    stop("Error: No target rows supplied.", call. = FALSE)
  }
  if (any(target_rows <= window_size) || any(target_rows > n)) {
    stop("Error: Target rows must lie in (window_size, nrow(values)].", call. = FALSE)
  }
  n_seq <- length(target_rows)
  X <- array(0, dim = c(n_seq, window_size, n_features))
  y <- numeric(n_seq)
  for (i in seq_len(n_seq)) {
    t <- target_rows[i]
    X[i, , ] <- values[(t - window_size):(t - 1), , drop = FALSE]
    y[i] <- values[t, target_idx]
  }
  list(X = X, y = y, target_rows = target_rows)
}

assert_no_future_leakage <- function(sequences, boundaries) {
  train_rows <- sequences$train$target_rows
  val_rows <- sequences$val$target_rows
  test_rows <- sequences$test$target_rows
  if (max(train_rows) > boundaries$train_end) {
    stop("Leakage check failed: training targets extend beyond the training boundary.", call. = FALSE)
  }
  if (min(val_rows) <= boundaries$train_end || max(val_rows) > boundaries$val_end) {
    stop("Leakage check failed: validation targets are outside the validation boundaries.", call. = FALSE)
  }
  if (min(test_rows) <= boundaries$val_end || max(test_rows) > boundaries$n) {
    stop("Leakage check failed: test targets are outside the test boundaries.", call. = FALSE)
  }
  invisible(TRUE)
}

validate_sequence_dims <- function(sequence, window_size, n_features) {
  dims <- dim(sequence$X)
  if (length(dims) != 3 || dims[2] != window_size || dims[3] != n_features) {
    stop(sprintf("Dimension check failed: X has dims (%s), expected (n, %d, %d).",
                 paste(dims, collapse = ", "), window_size, n_features), call. = FALSE)
  }
  if (length(sequence$y) != dims[1]) {
    stop("Dimension check failed: y length does not match the number of sequences.", call. = FALSE)
  }
  invisible(TRUE)
}

prepare_sequences <- function(preprocessed, config) {
  window_size <- config$window_size
  input_cols <- preprocessed$input_cols
  values <- as.matrix(preprocessed$full_scaled[, input_cols, drop = FALSE])
  target_idx <- match(config$target_col, input_cols)
  rows <- get_target_rows(preprocessed$boundaries, window_size)

  sequences <- list(
    train = create_sequences(values, target_idx, window_size, rows$train),
    val = create_sequences(values, target_idx, window_size, rows$val),
    test = create_sequences(values, target_idx, window_size, rows$test),
    input_cols = input_cols,
    target_idx = target_idx
  )
  assert_no_future_leakage(sequences, preprocessed$boundaries)
  for (part in c("train", "val", "test")) {
    validate_sequence_dims(sequences[[part]], window_size, length(input_cols))
  }
  sequences
}
