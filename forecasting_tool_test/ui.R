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
library(Matrix)
library(shinydashboard)


dashboardPage(
  #### dashboard header ####
  dashboardHeader(
    title = "Forecasting Tool",
    # titleWidth = "200",
    
    #### Dropdown menu for messages ####
    dropdownMenu(type = "messages", badgeStatus = "success",
                 messageItem("Updates",
                             "LSTM not working on web",
                             time = Sys.Date()
                 )),
    tags$li(class = "dropdown", 
            tags$a(href = "https://www.linkedin.com/in/soumyadipta-das/",
                   "Linked", icon("linkedin"), target = "_blank"
            )), 
    tags$li(class = "dropdown", 
            tags$a(href = "https://sites.google.com/view/soumyadipta-das",
                   tags$script(src = "https://kit.fontawesome.com/b8fb1ea7bd.js"),
                   icon = NULL,
                   tags$div(
                     tags$i(class = "fa-regular fa-id-card"),
                     tags$span("  Soumyadipta Das")
                   ), target = "_blank"
            ))
  ),
  dashboardSidebar(
    #### Sidebar menu ####
    sidebarMenu(
      id = "sidebar",
      
      #### menuitem ####
      menuItem("Forecasting_tab", tabName = "Forecasting", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      #### Forecasting tab ####
      tabItem(tabName = "Forecasting",
              tabBox(id = "tabbox_1", width = 12,
                     tabPanel("Forecasting", icon = icon("chart-line"), 
                              fluidPage(
                                sidebarLayout(
                                  sidebarPanel(
                                    fileInput("file", "Upload Your File (.csv supported)"),
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
                                                  choices = c("Linear Regression", "GLM", "Logistic Regression",
                                                              "LASSO", "Ridge Regression"))
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
                     )
              )
      )
    )
  )
)

