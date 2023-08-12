library(shiny)
library(plotly)
library(forecast)
library(rugarch)
library(reticulate)
library(h2o)
library(tensorflow)
library(keras)
library(shinythemes)
library(dplyr)
library(randomForest)
library(rpart)
library(glmnet)

# Server
# Server
server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  output$response_variable <- renderUI({
    req(data())
    selectInput("response_variable", "Select Response Variable",
                choices = colnames(data()))
  })
  
  output$x_variables <- renderUI({
    req(data(), input$response_variable, input$data_type)
    if (input$data_type == "Non-Time Series") {
      selectInput("x_variables", "Select X Variables",
                  choices = setdiff(colnames(data()), input$response_variable),
                  multiple = TRUE)
    } else {
      NULL
    }
  })
  
  tsData <- reactive({
    req(data(), input$response_variable)
    if (input$data_type == "Time Series") {
      if (input$seasonal) {
        if (!is.null(input$seasonal_period)) {
          ts(data()[[input$response_variable]], frequency = input$seasonal_period)
        } else {
          freq <- forecast::findfrequency(data()[[input$response_variable]])
          if (is.null(freq)) {
            freq <- 12  # Default seasonal period
          }
          ts(data()[[input$response_variable]], frequency = freq)
        }
      } else {
        ts(data()[[input$response_variable]], frequency = 1)
      }
    } else {
      NULL
    }
  })
  
  observe({
    choices <- colnames(data())
    updateSelectInput(session, "response_variable", choices = choices)
  })
  
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
    aml <- h2o.automl(x = setdiff(colnames(h2o_df), target),
                      y = target,
                      training_frame = h2o_df,
                      max_runtime_secs = 300,
                      max_models = 10)
    
    # Generate predictions for the future horizon
    forecast_df <- data.frame(Date = seq(time(ts_data)[length(ts_data)] + 1/12, by = 1/12, length.out = horizon))
    forecast_h2o <- as.h2o(forecast_df, destination_frame = "forecast_data")
    forecast_predictions <- h2o.predict(aml@leader, forecast_h2o)
    
    # Convert the predictions to a time series object
    forecast_values <- ts(as.vector(forecast_predictions$predict), frequency = 12)
    
    # Shut down the H2O cluster
    h2o.shutdown(prompt = FALSE)
    
    return(forecast_values)
  }
  
  forecastData <- reactive({
    if (input$data_type == "Time Series") {
      req(tsData(), input$model)
      model <- switch(input$model,
                      "ARIMA" = auto.arima(tsData()),
                      "SARIMA" = auto.arima(tsData(), seasonal = TRUE),
                      "ARCH" = {
                        spec <- ugarchspec(variance.model = list(model = "sGARCH"))
                        fit <- ugarchfit(spec, data = tsData())
                        fitted(fit)
                      },
                      "GARCH" = {
                        spec <- ugarchspec(variance.model = list(model = "gjrGARCH"))
                        fit <- ugarchfit(spec, data = tsData())
                        fitted(fit)
                      },
                      "LSTM" = lstm_forecast(tsData(), input$horizon),
                      "AutoML" = automl_forecast(tsData(), input$horizon),
                      "ETS" = forecast::forecast(tsData(), h = input$horizon)
      )
      
      if (!is.null(model)) {
        forecast_values <- forecast(model, h = input$horizon)
        forecast_values
      }
    } 
    
    if (input$data_type == "Non-Time Series") {
      req(!is.null(input$response_variable), input$x_variables, input$model1)
      
      model <- switch(input$model1,
                      "Linear Regression" = {
                        model_fit <- lm(data = data(),
                                        formula = as.formula(paste(input$response_variable, "~", paste(input$x_variables, collapse = " + "))))
                        model_fit
                      },
                      "GLM" = {
                        model_fit <- glm(data = data(),
                                         formula = as.formula(paste(input$response_variable, "~", paste(input$x_variables, collapse = " + "))),
                                         family = gaussian)
                        model_fit
                      },
                      "Logistic Regression" = {
                        model_fit <- glm(data = data(),
                                         formula = as.formula(paste(input$response_variable, "~", paste(input$x_variables, collapse = " + "))),
                                         family = binomial)
                        model_fit
                      },
                      "LASSO" = {
                        model_fit <- glmnet(x = as.matrix(data()[input$x_variables]), 
                                            y = data()[input$response_variable][,1], alpha = 1)
                        model_fit
                      },
                      "Ridge Regression" = {
                        model_fit <- glmnet(x = as.matrix(data()[input$x_variables]), 
                                            y = data()[input$response_variable][,1], alpha = 0)
                        model_fit
                      }
      )
      
      if (!is.null(model)) {
        # Check if forecast function exists
        if (input$model1 %in% c("Linear Regression", "GLM", "Logistic Regression")){
          forecast_values <- predict(model, newdata = data.frame(data()[input$x_variables]))
          forecast_values <- as.numeric(forecast_values)
        } else if (input$model1 %in% c("Classification", "LASSO", "Ridge Regression")) {
          # For non-time series data, use the fitted model to get forecasted values
          forecast_values <- predict(model, newx = as.matrix(data()[input$x_variables]))
          print(forecast_values)
          forecast_values <- as.numeric(forecast_values)
        }
        
      } else{
        NULL
      }
    } 
    
    return(forecast_values)
  })
  
  observeEvent(input$forecast, {
    forecastData()
  })
  
  
  output$plot <- renderPlotly({
    req(data(), input$response_variable, input$forecast, forecastData())
    
    if(input$data_type == "Time Series"){
      plot_data <- data.frame(
        x = time(tsData()),
        y = as.numeric(tsData()),
        type = "scatter",
        mode = "lines",
        name = "Data"
      )
    }
    
    if(input$data_type == "Non-Time Series"){
      plot_data <- data.frame(
        x = dplyr::row_number(data()),
        y = data()[input$response_variable][,1],
        type = "scatter",
        mode = "lines",
        name = "Data"
      )
      
    }
    
    
    if (!is.null(forecastData())) {
      
      if(input$data_type == "Time Series"){
        forecast_values <- forecastData()$mean
        forecast_length <- length(forecast_values)
        forecast_dates <- seq(time(tsData())[length(tsData())] + 1/12, by = 1/12, length.out = forecast_length)
        
        forecast_plot_data <- data.frame(
          x = c(time(tsData()), forecast_dates),
          y = c(as.numeric(tsData()), forecast_values),
          type = "scatter",
          mode = "lines",
          name = "Forecast"
        )
      } 
      if(input$data_type == "Non-Time Series"){
        forecast_values <- forecastData()
        
        forecast_plot_data <- data.frame(
          x = dplyr::row_number(data()),
          y = forecast_values,
          type = "scatter",
          mode = "lines",
          name = "Forecast"
        )
        
      }
      
      
      plot_ly() %>%
        add_trace(data = plot_data, x = ~x, y = ~y, type = "scatter", mode = "lines", name = "Data") %>%
        add_trace(data = forecast_plot_data, x = ~x, y = ~y, type = "scatter", mode = "lines", name = "Forecast") %>%
        layout(
          title = "Time Series Data and Forecast",
          xaxis = list(title = "Time"),
          yaxis = list(title = "Value")
        )
    } else {
      plot_ly() %>%
        add_trace(data = plot_data, x = ~x, y = ~y, type = "scatter", mode = "lines", name = "Data") %>%
        layout(
          title = "Time Series Data",
          xaxis = list(title = "Time"),
          yaxis = list(title = "Value")
        )
    }
  })
  
  output$download <- downloadHandler(
    filename = function(){
      paste0("forecast_", if_else(input$data_type == "Time Series", input$model, input$model1), ".csv")
      },
    content = function(file) {
      if (!is.null(forecastData())) {
        if(input$data_type == "Time Series"){
          forecast_values <- forecastData()$mean
          forecast_dates <- seq(time(tsData())[length(tsData())] + 1/12, by = 1/12, length.out = input$horizon)
          forecast_df <- data.frame(Date = forecast_dates, Forecast = forecast_values)
          write.csv(forecast_df, file, row.names = FALSE)
        } 
        
        if(input$data_type == "Non-Time Series"){
          forecast_values <- forecastData()
          forecast_seq <- seq(1, by = 1, length.out = length(forecastData()))
          forecast_df <- data.frame(Sequence = forecast_seq, Forecast = forecast_values)
          write.csv(forecast_df, file, row.names = FALSE)
        }
      }
    }
    
  )
}
