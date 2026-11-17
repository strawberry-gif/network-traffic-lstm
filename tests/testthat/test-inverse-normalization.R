test_that("scaling then inverse scaling recovers the original values", {
  df <- data.frame(traffic_volume = c(10, 20, 30, 40), packets = c(1, 2, 3, 5))
  scaler <- fit_scaler(df[1:3, ], c("traffic_volume", "packets"))
  scaled <- transform_scaler(df, scaler)
  expect_equal(scaled$traffic_volume[1:3], c(0, 0.5, 1))
  expect_equal(scaled$traffic_volume[4], 1.5)
  restored <- inverse_transform_scaler(scaled, scaler)
  expect_equal(restored, df)
})

test_that("inverse_scale_vector handles vectors and matrices", {
  scaler <- fit_scaler(data.frame(traffic_volume = c(10, 30)), "traffic_volume")
  expect_equal(inverse_scale_vector(c(0, 0.5, 1), scaler, "traffic_volume"), c(10, 20, 30))
  m <- matrix(c(0, 1, 0.5, 0.25), nrow = 2)
  expect_equal(dim(inverse_scale_vector(m, scaler, "traffic_volume")), c(2, 2))
  expect_error(inverse_scale_vector(1, scaler, "unknown"), "not part of the fitted scaler")
})

test_that("a constant training column does not divide by zero", {
  scaler <- fit_scaler(data.frame(x = c(5, 5, 5)), "x")
  expect_equal(scaler$ranges[["x"]], 1)
  expect_equal(transform_scaler(data.frame(x = c(5, 6)), scaler)$x, c(0, 1))
})

test_that("inverse transform ignores columns absent from the data frame", {
  scaler <- fit_scaler(data.frame(traffic_volume = c(10, 30), packets = c(0, 1)), c("traffic_volume", "packets"))
  out <- inverse_transform_scaler(data.frame(traffic_volume = c(0, 1)), scaler)
  expect_equal(out$traffic_volume, c(10, 30))
})

test_that("non-finite training data cannot be used to fit a scaler", {
  expect_error(fit_scaler(data.frame(x = c(1, NA, 3)), "x"), "finite")
})
