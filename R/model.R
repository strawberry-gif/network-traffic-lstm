library(keras3)

build_lstm_model <- function(window_size, n_features, lstm_units = 50,
                             dropout_rate = 0.2, activation = "tanh",
                             output_activation = "linear", learning_rate = 0.001) {
  for (value in list(window_size, n_features, lstm_units)) {
    if (!is.numeric(value) || length(value) != 1 || value < 1 || value != round(value)) {
      stop("Error: window_size, n_features and lstm_units must be positive integers.", call. = FALSE)
    }
  }
  if (!is.numeric(dropout_rate) || dropout_rate < 0 || dropout_rate >= 1) {
    stop("Error: dropout_rate must be in [0, 1).", call. = FALSE)
  }

  model <- keras_model_sequential(input_shape = c(as.integer(window_size), as.integer(n_features)))
  model <- layer_lstm(model,
                      units = as.integer(lstm_units),
                      activation = activation,
                      return_sequences = FALSE)
  model <- layer_dropout(model, rate = dropout_rate)
  model <- layer_dense(model, units = 1L, activation = output_activation)

  compile(model,
          optimizer = optimizer_adam(learning_rate = learning_rate),
          loss = "mse",
          metrics = list("mae"))

  model
}

validate_model_io <- function(model, X) {
  prediction <- predict(model, X[1, , , drop = FALSE], verbose = 0)
  if (!identical(as.integer(dim(prediction)), c(1L, 1L))) {
    stop("Model output check failed: expected a (1, 1) prediction, got (",
         paste(dim(prediction), collapse = ", "), ").", call. = FALSE)
  }
  invisible(TRUE)
}
