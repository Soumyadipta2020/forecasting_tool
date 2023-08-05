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

# UI
ui <- fluidPage(
  titlePanel("Forecasting Tool"),
  theme = shinytheme("sandstone"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File"),
      selectInput("data_type", "Select Data Type", 
                  choices = c("Time Series", "Non-Time Series")),
      uiOutput("response_variable"),
      uiOutput("x_variables"),
      numericInput("horizon", "Forecast Horizon", value = 12, min = 1),
      conditionalPanel(
        condition = "input.data_type == 'Time Series'",
        checkboxInput("seasonal", "Is Data Seasonal?"),
        conditionalPanel(
          condition = "input.seasonal",
          numericInput("seasonal_period", "Seasonal Period", value = 12, min = 1)
        ),
        selectInput("model", "Select Time Series Model",
                    choices = c("ARIMA", "SARIMA", "ARCH", "GARCH",
                                "LSTM", "AutoML", "ETS"))
      ),
      conditionalPanel(
        condition = "input.data_type == 'Non-Time Series'",
        selectInput("model1", "Select Model for Non-Time Series",
                    choices = c("Linear Regression", "GLM", "Logistic Regression"
                                # ,"Classification", "LASSO", "Ridge Regression"
                                ))
      ),
      actionButton("forecast", "Generate Forecast"),
      br(),br(),
      downloadButton("download", "Download Forecast")
    ),
    mainPanel(
      plotlyOutput("plot")
    )
  )
)