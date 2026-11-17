generate_synthetic_traffic <- function(n_points = 1000,
                                       start_date = "2026-01-01 00:00:00",
                                       interval_minutes = 1,
                                       output_file = "data/raw/network_traffic.csv",
                                       seed = 2026) {
  if (!is.numeric(n_points) || n_points < 1) {
    stop("Error: n_points must be a positive number.", call. = FALSE)
  }
  if (!is.null(seed)) {
    set.seed(seed)
  }

  start_time <- as.POSIXct(start_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  if (is.na(start_time)) {
    stop("Error: start_date must use the format %Y-%m-%d %H:%M:%S.", call. = FALSE)
  }
  timestamps <- seq(start_time, by = interval_minutes * 60, length.out = n_points)
  timestamps_char <- format(timestamps, format = "%Y-%m-%d %H:%M:%S")

  hour_fraction <- as.numeric(format(timestamps, "%H")) + as.numeric(format(timestamps, "%M")) / 60
  day_of_week <- as.POSIXlt(timestamps)$wday + 1

  daily_pattern <- 50 + 30 * sin(hour_fraction * 2 * pi / 24)
  weekly_factor <- ifelse(day_of_week %in% c(2:6), 1.2, 0.8)
  noise <- rnorm(n_points, mean = 0, sd = 5)

  traffic_volume <- round(daily_pattern * weekly_factor + noise, 1)
  traffic_volume <- pmax(traffic_volume, 0)

  packets <- round(traffic_volume * runif(n_points, 1.2, 1.8))
  bytes <- round(traffic_volume * 1000 * runif(n_points, 0.8, 1.2))

  df <- data.frame(
    timestamp = timestamps_char,
    packets = packets,
    bytes = bytes,
    traffic_volume = traffic_volume
  )

  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  write.csv(df, file = output_file, row.names = FALSE)
  cat("Synthetic dataset generated at", output_file, "\n")
  cat("Total rows:", nrow(df), "\n")
  cat("NOTE: this dataset is synthetic and does not represent real network traffic.\n")
  df
}
