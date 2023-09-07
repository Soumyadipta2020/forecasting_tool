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
library(shinydashboard)
library(dashboardthemes)

# Server
server <- function(input, output, session) {
  output$file_template_download <- downloadHandler(
    filename = function(){
      paste("Sample data", ".csv", sep = "")
    },
    content = function(file){
      temp = read.csv("timeseries demo.csv", header = TRUE)
      write.csv(temp, file, row.names = FALSE)
    }
  )
  
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    
    col_type_check <- ((sum(1*((as.vector(sapply(df, class)))=="numeric")) + 
                          sum(1*((as.vector(sapply(df, class)))=="integer"))) >= ncol(df) - 1)
    if(!col_type_check){
      shinyFeedback::hideFeedback("file")
      shinyFeedback::feedbackDanger("file", !col_type_check, "Invalid file format. Please download the template.")
      req(col_type_check)
    } else{
      shinyFeedback::hideFeedback("file")
      shinyFeedback::feedbackSuccess("file", col_type_check, "Successfully Uploaded.")
      req(col_type_check)
      
      return(df)
    }
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
          title = "Actual Data and Forecast",
          xaxis = list(title = "Time"),
          yaxis = list(title = "Value")
        )
    } else {
      plot_ly() %>%
        add_trace(data = plot_data, x = ~x, y = ~y, type = "scatter", mode = "lines", name = "Data") %>%
        layout(
          title = "Actual Data",
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
