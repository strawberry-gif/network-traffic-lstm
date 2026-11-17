library(keras3)

set_global_seed <- function(seed, deterministic = FALSE) {
  seed <- as.integer(seed)
  set.seed(seed)
  keras3::set_random_seed(seed)
  if (isTRUE(deterministic)) {
    tryCatch(
      keras3::config_enable_op_determinism(),
      error = function(e) {
        warning("Could not enable deterministic operations: ", conditionMessage(e), call. = FALSE)
      }
    )
  }
  invisible(seed)
}

history_to_data_frame <- function(history) {
  metrics <- history$metrics
  if (is.null(metrics) || length(metrics) == 0) {
    stop("Error: Training history contains no metrics.", call. = FALSE)
  }
  n_epochs <- length(metrics[[1]])
  df <- data.frame(epoch = seq_len(n_epochs))
  for (name in names(metrics)) {
    values <- as.numeric(unlist(metrics[[name]]))
    if (length(values) == n_epochs) {
      df[[name]] <- values
    }
  }
  df
}

train_model <- function(model, train_seq, val_seq, config, checkpoint_path = NULL) {
  if (length(train_seq$y) < 1 || length(val_seq$y) < 1) {
    stop("Error: Training and validation sequences must not be empty.", call. = FALSE)
  }

  early_stop <- callback_early_stopping(
    monitor = "val_loss",
    patience = config$early_stopping_patience,
    restore_best_weights = TRUE
  )

  reduce_lr <- callback_reduce_lr_on_plateau(
    monitor = "val_loss",
    factor = config$reduce_lr_factor,
    patience = config$reduce_lr_patience,
    min_lr = config$min_lr
  )

  callbacks <- list(early_stop, reduce_lr)
  if (!is.null(checkpoint_path)) {
    dir.create(dirname(checkpoint_path), recursive = TRUE, showWarnings = FALSE)
    callbacks <- c(callbacks, list(callback_model_checkpoint(
      filepath = checkpoint_path,
      monitor = "val_loss",
      save_best_only = TRUE
    )))
  }

  verbose <- if (is.null(config$verbose)) 1 else config$verbose

  fit(
    model,
    x = train_seq$X,
    y = matrix(train_seq$y, ncol = 1),
    validation_data = list(val_seq$X, matrix(val_seq$y, ncol = 1)),
    epochs = config$epochs,
    batch_size = config$batch_size,
    callbacks = callbacks,
    verbose = verbose
  )
}

train_with_seed <- function(seed, train_seq, val_seq, config) {
  checkpoint_path <- file.path(config$checkpoint_dir, sprintf("lstm_seed_%d.keras", as.integer(seed)))
  tryCatch({
    set_global_seed(seed, config$deterministic)
    n_features <- dim(train_seq$X)[3]
    model <- build_lstm_model(
      window_size = config$window_size,
      n_features = n_features,
      lstm_units = config$lstm_units,
      dropout_rate = config$dropout_rate,
      activation = config$activation,
      output_activation = config$output_activation,
      learning_rate = config$learning_rate
    )
    history <- train_model(model, train_seq, val_seq, config, checkpoint_path)
    history_df <- history_to_data_frame(history)
    list(
      status = "ok",
      seed = seed,
      model = model,
      history = history_df,
      best_val_loss = min(history_df$val_loss),
      epochs_run = nrow(history_df),
      checkpoint_path = checkpoint_path,
      error = NA_character_
    )
  }, error = function(e) {
    message(sprintf("Training failed for seed %s: %s", seed, conditionMessage(e)))
    list(
      status = "failed",
      seed = seed,
      model = NULL,
      history = NULL,
      best_val_loss = NA_real_,
      epochs_run = 0L,
      checkpoint_path = NA_character_,
      error = conditionMessage(e)
    )
  })
}

select_best_run <- function(runs) {
  ok <- Filter(function(run) identical(run$status, "ok"), runs)
  if (length(ok) == 0) {
    return(NULL)
  }
  losses <- vapply(ok, function(run) run$best_val_loss, numeric(1))
  ok[[which.min(losses)]]
}
