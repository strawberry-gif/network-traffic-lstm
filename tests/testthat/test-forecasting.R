make_scaler <- function() {
  fit_scaler(data.frame(traffic_volume = c(10, 30), packets = c(0, 1)), c("traffic_volume", "packets"))
}

test_that("univariate recursive forecast returns n_steps values in original units", {
  model <- structure(list(), class = "mock_last_plus")
  last_sequence <- matrix(c(0.1, 0.2, 0.3, 0.4, 0.5), ncol = 1)
  out <- forecast_future(model, last_sequence, make_scaler(), "traffic_volume",
                         n_steps = 3, window_size = 5, col_order = "traffic_volume")
  expect_equal(length(out), 3)
  expect_equal(out, c(0.6, 0.7, 0.8) * 20 + 10)
})

test_that("forecast horizon and inputs are validated", {
  model <- structure(list(), class = "mock_last_plus")
  last_sequence <- matrix(c(0.1, 0.2, 0.3, 0.4, 0.5), ncol = 1)
  scaler <- make_scaler()
  expect_error(forecast_future(model, last_sequence, scaler, "traffic_volume", 0, 5, "traffic_volume"), "n_steps")
  expect_error(forecast_future(model, last_sequence, scaler, "traffic_volume", 2.5, 5, "traffic_volume"), "n_steps")
  expect_error(forecast_future(model, last_sequence, scaler, "traffic_volume", 2, 6, "traffic_volume"), "window_size")
  expect_error(forecast_future(model, last_sequence, scaler, "absent", 2, 5, "traffic_volume"), "target_col")
  with_na <- last_sequence
  with_na[2, 1] <- NA
  expect_error(forecast_future(model, with_na, scaler, "traffic_volume", 2, 5, "traffic_volume"), "missing")
})

test_that("multivariate forecasting refuses to run without future inputs", {
  model <- structure(list(), class = "mock_second_feature")
  last_sequence <- cbind(c(0.1, 0.2, 0.3, 0.4, 0.5), c(0.1, 0.1, 0.1, 0.1, 0.9))
  expect_error(
    forecast_future(model, last_sequence, make_scaler(), "traffic_volume", 3, 5,
                    c("traffic_volume", "packets")),
    "requires future values"
  )
})

test_that("multivariate forecasting uses supplied future inputs, not stale ones", {
  model <- structure(list(), class = "mock_second_feature")
  last_sequence <- cbind(c(0.1, 0.2, 0.3, 0.4, 0.5), c(0.1, 0.1, 0.1, 0.1, 0.9))
  future <- data.frame(packets = c(0.2, 0.3, 0.4))
  out <- forecast_future(model, last_sequence, make_scaler(), "traffic_volume", 3, 5,
                         c("traffic_volume", "packets"), future_features = future)
  expect_equal(out, c(0.9, 0.2, 0.3) * 20 + 10)
  too_short <- data.frame(packets = c(0.2))
  expect_error(
    forecast_future(model, last_sequence, make_scaler(), "traffic_volume", 3, 5,
                    c("traffic_volume", "packets"), future_features = too_short),
    "at least n_steps"
  )
})

test_that("predict_model validates array rank and returns a numeric vector", {
  model <- structure(list(), class = "mock_last_plus")
  X <- array(0.5, dim = c(4, 3, 1))
  expect_equal(predict_model(model, X), rep(0.6, 4))
  expect_error(predict_model(model, matrix(1, 2, 2)), "3-dimensional")
})
