get_input_cols <- function(config) {
  unique(c(config$target_col, config$feature_cols))
}

load_data <- function(file_path) {
  if (!is.character(file_path) || length(file_path) != 1 || !file.exists(file_path)) {
    stop("Error: Data file not found at ", paste(file_path, collapse = ", "), call. = FALSE)
  }
  data <- tryCatch(
    read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM"),
    error = function(e) {
      stop("Error: Could not read data file: ", conditionMessage(e), call. = FALSE)
    }
  )
  if (nrow(data) == 0) {
    stop("Error: Dataset is empty.", call. = FALSE)
  }
  data
}

inspect_data <- function(data) {
  cat("Dataset dimensions:", dim(data), "\n")
  cat("Column names:", colnames(data), "\n")
  cat("First few rows:\n")
  print(head(data))
  cat("Summary statistics:\n")
  print(summary(data))
  invisible(data)
}

apply_column_map <- function(data, column_map) {
  if (is.null(column_map) || length(column_map) == 0) {
    return(data)
  }
  if (is.null(names(column_map)) || any(names(column_map) == "")) {
    stop("Error: column_map must be a named character vector (canonical name = source column).", call. = FALSE)
  }
  absent <- setdiff(unname(column_map), colnames(data))
  if (length(absent) > 0) {
    stop("Error: column_map refers to missing source columns: ", paste(absent, collapse = ", "), call. = FALSE)
  }
  for (canonical in names(column_map)) {
    source_name <- column_map[[canonical]]
    if (canonical != source_name) {
      if (canonical %in% colnames(data)) {
        stop("Error: column_map target '", canonical, "' already exists in the dataset.", call. = FALSE)
      }
      colnames(data)[colnames(data) == source_name] <- canonical
    }
  }
  data
}

validate_required_columns <- function(data, required_cols) {
  absent <- setdiff(required_cols, colnames(data))
  if (length(absent) > 0) {
    stop("Error: Missing required columns: ", paste(absent, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

process_timestamps <- function(data, timestamp_col = "timestamp",
                               formats = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d")) {
  if (!timestamp_col %in% colnames(data)) {
    stop("Error: Column '", timestamp_col, "' not found.", call. = FALSE)
  }
  raw <- data[[timestamp_col]]
  if (inherits(raw, "POSIXct")) {
    parsed <- raw
  } else if (is.numeric(raw)) {
    parsed <- as.POSIXct(raw, origin = "1970-01-01", tz = "UTC")
  } else {
    text <- trimws(as.character(raw))
    parsed <- as.POSIXct(rep(NA_real_, length(text)), origin = "1970-01-01", tz = "UTC")
    for (fmt in formats) {
      pending <- is.na(parsed) & !is.na(text) & nzchar(text)
      if (!any(pending)) {
        break
      }
      parsed[pending] <- as.POSIXct(text[pending], format = fmt, tz = "UTC")
    }
  }
  if (anyNA(parsed)) {
    bad <- which(is.na(parsed))
    stop(sprintf("Error: %d timestamps could not be parsed (first bad row: %d). Check timestamp_formats.",
                 length(bad), bad[1]), call. = FALSE)
  }
  if (is.unsorted(parsed)) {
    cat("Timestamps were not chronological; sorting.\n")
  }
  data[[timestamp_col]] <- parsed
  data <- data[order(parsed), , drop = FALSE]
  rownames(data) <- NULL
  data
}

remove_duplicates <- function(data, timestamp_col = "timestamp", strategy = "first") {
  if (!strategy %in% c("first", "last", "mean")) {
    stop("Error: duplicate strategy must be 'first', 'last' or 'mean'.", call. = FALSE)
  }
  is_duplicate <- duplicated(data[[timestamp_col]])
  duplicate_count <- sum(is_duplicate)
  if (duplicate_count == 0) {
    cat("No duplicate timestamps found.\n")
    return(data)
  }
  cat("Resolving", duplicate_count, "duplicate timestamps using strategy:", strategy, "\n")
  if (strategy == "first") {
    result <- data[!is_duplicate, , drop = FALSE]
  } else if (strategy == "last") {
    result <- data[!duplicated(data[[timestamp_col]], fromLast = TRUE), , drop = FALSE]
  } else {
    result <- data[!is_duplicate, , drop = FALSE]
    group <- as.numeric(data[[timestamp_col]])
    numeric_cols <- setdiff(names(data)[vapply(data, is.numeric, logical(1))], timestamp_col)
    for (col in numeric_cols) {
      group_means <- ave(data[[col]], group, FUN = function(v) {
        if (all(is.na(v))) NA_real_ else mean(v, na.rm = TRUE)
      })
      result[[col]] <- group_means[!is_duplicate]
    }
  }
  rownames(result) <- NULL
  result
}

infer_interval_seconds <- function(timestamps) {
  if (length(timestamps) < 2) {
    stop("Error: At least two timestamps are required to infer the sampling interval.", call. = FALSE)
  }
  diffs <- diff(as.numeric(timestamps))
  diffs <- diffs[diffs > 0]
  if (length(diffs) == 0) {
    stop("Error: Cannot infer the sampling interval from the timestamps.", call. = FALSE)
  }
  stats::median(diffs)
}

detect_timestamp_gaps <- function(timestamps, expected_interval = NULL, tolerance = 0.01) {
  interval <- if (is.null(expected_interval)) infer_interval_seconds(timestamps) else expected_interval
  diffs <- diff(as.numeric(timestamps))
  irregular <- which(abs(diffs - interval) > tolerance * interval)
  gaps <- data.frame(
    after_row = irregular,
    from_time = timestamps[irregular],
    to_time = timestamps[irregular + 1],
    gap_seconds = diffs[irregular],
    missing_intervals = round(diffs[irregular] / interval) - 1
  )
  list(interval_seconds = interval, gaps = gaps)
}

validate_timestamp_intervals <- function(data, timestamp_col, config) {
  info <- detect_timestamp_gaps(data[[timestamp_col]], tolerance = config$interval_tolerance)
  cat(sprintf("Sampling interval: %.0f seconds\n", info$interval_seconds))
  if (nrow(info$gaps) > 0) {
    message_text <- sprintf(
      "%d irregular timestamp intervals detected (expected %.0f seconds); first at row %d.",
      nrow(info$gaps), info$interval_seconds, info$gaps$after_row[1]
    )
    if (config$irregular_action == "stop") {
      stop("Error: ", message_text, call. = FALSE)
    } else if (config$irregular_action == "warn") {
      warning(message_text, call. = FALSE)
    }
  } else {
    cat("Timestamps are evenly spaced.\n")
  }
  info
}

regularize_timestamps <- function(data, timestamp_col, interval_seconds) {
  secs <- as.numeric(data[[timestamp_col]])
  grid <- seq(secs[1], secs[length(secs)], by = interval_seconds)
  if (any(is.na(match(secs, grid)))) {
    stop("Error: Timestamps are not aligned to a regular grid; cannot regularize.", call. = FALSE)
  }
  result <- data[match(grid, secs), , drop = FALSE]
  result[[timestamp_col]] <- as.POSIXct(grid, origin = "1970-01-01", tz = "UTC")
  rownames(result) <- NULL
  result
}

select_columns <- function(data, timestamp_col, input_cols) {
  validate_required_columns(data, c(timestamp_col, input_cols))
  data[, c(timestamp_col, input_cols), drop = FALSE]
}

validate_numeric_columns <- function(data, cols) {
  for (col in cols) {
    x <- data[[col]]
    if (is.factor(x)) {
      x <- as.character(x)
    }
    if (is.character(x)) {
      converted <- suppressWarnings(as.numeric(x))
      bad <- is.na(converted) & !is.na(x) & nzchar(trimws(x))
      if (any(bad)) {
        stop("Error: Column '", col, "' contains non-numeric values (first at row ", which(bad)[1], ").", call. = FALSE)
      }
      x <- converted
    }
    if (!is.numeric(x)) {
      stop("Error: Column '", col, "' must be numeric.", call. = FALSE)
    }
    x[is.infinite(x)] <- NA_real_
    data[[col]] <- as.numeric(x)
  }
  data
}

fill_forward <- function(x) {
  if (length(x) == 0) {
    return(x)
  }
  last_valid <- cummax(ifelse(is.na(x), 0L, seq_along(x)))
  has_history <- last_valid > 0
  result <- x
  result[has_history] <- x[last_valid[has_history]]
  result
}

handle_missing <- function(data, cols, max_missing_fraction = 1) {
  missing_count <- colSums(is.na(data[, cols, drop = FALSE]))
  if (any(missing_count > 0)) {
    cat("Missing values detected:\n")
    print(missing_count)
    fractions <- missing_count / nrow(data)
    if (any(fractions > max_missing_fraction)) {
      stop(sprintf("Error: Missing fraction exceeds %.2f in: %s", max_missing_fraction,
                   paste(names(fractions)[fractions > max_missing_fraction], collapse = ", ")), call. = FALSE)
    }
    for (col in cols) {
      if (any(is.na(data[[col]]))) {
        data[[col]] <- fill_forward(data[[col]])
      }
    }
    incomplete <- !complete.cases(data[, cols, drop = FALSE])
    if (any(incomplete)) {
      cat("Dropping", sum(incomplete), "leading rows that have no earlier observation to fill from.\n")
      data <- data[!incomplete, , drop = FALSE]
      rownames(data) <- NULL
    }
    if (nrow(data) == 0) {
      stop("Error: No rows remain after handling missing values.", call. = FALSE)
    }
  } else {
    cat("No missing values found.\n")
  }
  data
}

split_data <- function(data, train_ratio, val_ratio, test_ratio, window_size = 1) {
  if (abs(train_ratio + val_ratio + test_ratio - 1) > 1e-6) {
    stop("Error: Split ratios must sum to 1.", call. = FALSE)
  }
  n <- nrow(data)
  train_end <- floor(train_ratio * n + 1e-9)
  val_end <- floor((train_ratio + val_ratio) * n + 1e-9)
  if (train_end - window_size < 1 || val_end - train_end < 1 || n - val_end < 1) {
    stop(sprintf("Error: Dataset of %d rows is too small for window_size=%d and the requested split.",
                 n, window_size), call. = FALSE)
  }
  train <- data[seq_len(train_end), , drop = FALSE]
  val <- data[(train_end + 1):val_end, , drop = FALSE]
  test <- data[(val_end + 1):n, , drop = FALSE]
  cat(sprintf("Split: Train=%d, Val=%d, Test=%d\n", nrow(train), nrow(val), nrow(test)))
  list(
    train = train,
    val = val,
    test = test,
    boundaries = list(train_end = train_end, val_end = val_end, n = n)
  )
}

fit_scaler <- function(data, cols) {
  for (col in cols) {
    if (!is.numeric(data[[col]]) || any(!is.finite(data[[col]]))) {
      stop("Error: Cannot fit scaler: column '", col, "' must be numeric and finite in the training data.", call. = FALSE)
    }
  }
  mins <- vapply(cols, function(col) min(data[[col]]), numeric(1))
  maxs <- vapply(cols, function(col) max(data[[col]]), numeric(1))
  ranges <- maxs - mins
  ranges[ranges == 0] <- 1
  list(mins = mins, maxs = maxs, ranges = ranges, cols = cols)
}

transform_scaler <- function(data, scaler) {
  for (col in intersect(scaler$cols, colnames(data))) {
    data[[col]] <- (data[[col]] - scaler$mins[[col]]) / scaler$ranges[[col]]
  }
  data
}

inverse_transform_scaler <- function(data, scaler) {
  for (col in intersect(scaler$cols, colnames(data))) {
    data[[col]] <- data[[col]] * scaler$ranges[[col]] + scaler$mins[[col]]
  }
  data
}

inverse_scale_vector <- function(x, scaler, col) {
  if (!col %in% scaler$cols) {
    stop("Error: Column '", col, "' is not part of the fitted scaler.", call. = FALSE)
  }
  x * scaler$ranges[[col]] + scaler$mins[[col]]
}

preprocess_pipeline <- function(file_path, config) {
  validate_config(config)
  input_cols <- get_input_cols(config)
  timestamp_col <- config$timestamp_col

  cat("Loading data...\n")
  data <- load_data(file_path)
  cat("Inspecting data...\n")
  inspect_data(data)

  cat("Applying column mapping and validating columns...\n")
  data <- apply_column_map(data, config$column_map)
  validate_required_columns(data, c(timestamp_col, input_cols))

  cat("Processing timestamps and sorting...\n")
  data <- process_timestamps(data, timestamp_col, config$timestamp_formats)

  cat("Resolving duplicate timestamps...\n")
  data <- remove_duplicates(data, timestamp_col, config$duplicate_strategy)

  cat("Validating timestamp intervals...\n")
  interval_info <- validate_timestamp_intervals(data, timestamp_col, config)
  if (isTRUE(config$regularize_grid) && nrow(interval_info$gaps) > 0) {
    cat("Regularizing timestamps onto a complete grid; gaps become missing values.\n")
    data <- regularize_timestamps(data, timestamp_col, interval_info$interval_seconds)
  }

  cat("Selecting and validating input columns...\n")
  data <- select_columns(data, timestamp_col, input_cols)
  data <- validate_numeric_columns(data, input_cols)

  cat("Handling missing values...\n")
  data <- handle_missing(data, input_cols, config$max_missing_fraction)

  cat("Splitting data chronologically...\n")
  splits <- split_data(data, config$train_ratio, config$val_ratio, config$test_ratio, config$window_size)

  cat("Fitting scaler on training data only...\n")
  scaler <- fit_scaler(splits$train, input_cols)

  cat("Applying scaling...\n")
  full_scaled <- transform_scaler(data, scaler)
  boundaries <- splits$boundaries

  list(
    train = full_scaled[seq_len(boundaries$train_end), , drop = FALSE],
    val = full_scaled[(boundaries$train_end + 1):boundaries$val_end, , drop = FALSE],
    test = full_scaled[(boundaries$val_end + 1):boundaries$n, , drop = FALSE],
    full_raw = data,
    full_scaled = full_scaled,
    boundaries = boundaries,
    scaler = scaler,
    feature_cols = config$feature_cols,
    target_col = config$target_col,
    input_cols = input_cols,
    timestamp_col = timestamp_col,
    interval_seconds = interval_info$interval_seconds,
    gaps = interval_info$gaps
  )
}
