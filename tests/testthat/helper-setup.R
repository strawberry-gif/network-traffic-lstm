project_root <- here::here()

source_project_file <- function(name) {
  source(file.path(project_root, "R", name), local = globalenv())
}

for (file_name in c("config.R", "data_preprocessing.R", "sequence_generation.R",
                    "prediction.R", "baselines.R", "evaluation.R")) {
  source_project_file(file_name)
}

keras_ready <- function() {
  if (!requireNamespace("keras3", quietly = TRUE)) {
    return(FALSE)
  }
  isTRUE(tryCatch(reticulate::py_module_available("keras"), error = function(e) FALSE))
}

if (keras_ready()) {
  for (file_name in c("model.R", "training.R")) {
    source_project_file(file_name)
  }
}

make_test_config <- function(overrides = list()) {
  cfg <- get("config", envir = globalenv())
  cfg$window_size <- 5
  cfg$forecast_steps <- 3
  cfg$epochs <- 2
  cfg$batch_size <- 8
  cfg$lstm_units <- 4
  cfg$seeds <- c(1, 2)
  cfg$verbose <- 0
  cfg$checkpoint_dir <- file.path(tempdir(), "checkpoints")
  for (name in names(overrides)) {
    cfg[[name]] <- overrides[[name]]
  }
  cfg
}

make_traffic_data <- function(n = 200, interval = 60, start = "2026-01-01 00:00:00") {
  stamps <- as.POSIXct(start, tz = "UTC") + (seq_len(n) - 1) * interval
  signal <- 50 + 20 * sin(seq_len(n) / 10)
  data.frame(
    timestamp = format(stamps, "%Y-%m-%d %H:%M:%S"),
    packets = round(signal * 1.5),
    bytes = round(signal * 1000),
    traffic_volume = signal,
    stringsAsFactors = FALSE
  )
}

write_temp_csv <- function(df) {
  path <- tempfile(fileext = ".csv")
  write.csv(df, path, row.names = FALSE)
  path
}

registerS3method("predict", "mock_last_plus", function(object, x, ...) {
  matrix(x[, dim(x)[2], 1] + 0.1, ncol = 1)
})

registerS3method("predict", "mock_second_feature", function(object, x, ...) {
  matrix(x[, dim(x)[2], 2], ncol = 1)
})
