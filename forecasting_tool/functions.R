#### mode ####
getmode <- function(v, na = TRUE) {
  if (na == TRUE) {
    v <- v[!is.na(v)]
  }
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#### Forecasting functions ####
#### LSTM ####
lstm_forecast <- function(ts_data, horizon) {
  library(keras)

  # Normalize the data
  normalized_data <- scale(ts_data)

  # Split data into train and test sets
  train_data <- normalized_data[1:(length(normalized_data) - horizon)]
  test_data <- normalized_data[(length(normalized_data) - horizon + 1):length(normalized_data)]

  # Prepare the training data
  train_x <- train_y <- list()
  for (i in 1:(length(train_data) - horizon)) {
    train_x[[i]] <- matrix(train_data[i:(i + horizon - 1)], nrow = horizon, ncol = 1)
    train_y[[i]] <- train_data[(i + horizon)]
  }

  train_x <- array_reshape(train_x, c(length(train_x), horizon, 1))
  train_y <- unlist(train_y)

  # Define the LSTM model architecture
  model <- keras_model_sequential()
  model %>%
    layer_lstm(units = 50, input_shape = c(horizon, 1)) %>%
    layer_dense(units = 1)

  # Compile the model
  model %>% compile(
    loss = "mean_squared_error",
    optimizer = optimizer_adam()
  )

  # Train the model
  model %>% fit(
    train_x, train_y,
    epochs = 100,
    batch_size = 32,
    verbose = 0
  )

  # Make predictions for the test set
  test_x <- array_reshape(test_data, dim = c(length(test_data) / horizon, horizon, 1))
  predicted_values <- model %>% predict(test_x)

  # Denormalize the predicted values
  denormalized_values <- predicted_values * sd(ts_data) + mean(ts_data)

  # Create the forecast object
  forecast_values <- ts(denormalized_values, frequency = 12)

  return(forecast_values)
}

#### AutoML ####
automl_forecast <- function(ts_data, horizon) {
  # Convert the time series data to a data frame
  data_df <- data.frame(Date = as.numeric(time(ts_data)), Value = as.numeric(ts_data))

  # Initialize the H2O cluster
  h2o.init()

  # Convert the data frame to an H2O frame
  h2o_df <- as.h2o(data_df, destination_frame = "ts_data")

  # Set the target variable
  target <- "Value"

  # Train AutoML model
  aml <- h2o.automl(
    x = setdiff(colnames(h2o_df), target),
    y = target,
    training_frame = h2o_df,
    max_runtime_secs = 300,
    max_models = 10
  )

  # Generate predictions for the future horizon
  forecast_df <- data.frame(Date = seq(time(ts_data)[length(ts_data)] + 1 / 12, by = 1 / 12, length.out = horizon))
  forecast_h2o <- as.h2o(forecast_df, destination_frame = "forecast_data")
  forecast_predictions <- h2o.predict(aml@leader, forecast_h2o)

  # Convert the predictions to a time series object
  forecast_values <- ts(as.vector(forecast_predictions$predict), frequency = 12)

  # Shut down the H2O cluster
  h2o.shutdown(prompt = FALSE)

  return(forecast_values)
}

#### ARFIMA Forecast ####
arfima_forecast <- function(x, h) {{ arfima_fit <- auto.arima(x,
  seasonal = FALSE, stepwise = FALSE,
  approximation = FALSE, allowdrift = FALSE
)

# Extract AR and MA orders from the fitted model
ar_order <- arimaorder(arfima_fit)[1] # AR order
ma_order <- arimaorder(arfima_fit)[3] # MA order

# Create dynamic ARFIMA specification
spec <- arfimaspec(mean.model = list(
  armaOrder = c(ar_order, ma_order),
  include.mean = TRUE, arfima = TRUE
))

result <- try(
  {
    # Fit the model using the dynamic specification
    garch_fit <- arfimafit(spec = spec, data = x)

    # Generate forecasts directly using arfimaforecast
    forecast_values <- arfimaforecast(garch_fit, n.ahead = h)
  },
  silent = TRUE
)

if (inherits(result, "try-error")) {
  forecast_vector <- rep(0, times = h)
} else {
  # Extract the forecasted values as a vector
  forecast_vector <- as.vector(forecast_values@forecast$seriesFor)
}


return(forecast_vector) }}
