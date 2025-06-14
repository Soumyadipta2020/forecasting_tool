library(shiny)

forecasting_tab <- tabItem(tabName = "Forecasting",
                           tabBox(id = "tabbox_2", width = 12,
                                  tabPanel("Forecasting", icon = icon("chart-line"), 
                                           fluidPage(
                                             sidebarLayout(
                                               sidebarPanel(
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
                                                               choices = c("ARIMA", "SARIMA", "GRNN", "ARFIMA", 
                                                                           "ARCH", "GARCH", "Neural Network", 
                                                                           # "LSTM", 
                                                                           "AutoML", "ETS", "Prophet", 
                                                                           "State Space ARIMA"))
                                                 ),
                                                 conditionalPanel(
                                                   condition = "input.data_type == 'Non-Time Series'",
                                                   selectInput("model1", "Select Model for Non-Time Series",
                                                               choices = c("Linear Regression", "GLM", "Logistic Regression",
                                                                           "LASSO", "Ridge Regression"))
                                                 ),
                                                 actionButton("forecast", "Generate Forecast", icon = icon("arrow-right")),
                                                 br(),br(),
                                                 downloadButton("download", "Download")
                                               ),
                                               mainPanel(
                                                 radioGroupButtons(
                                                   inputId = "change_plot",
                                                   label = NULL,
                                                   choices = c(
                                                     `<i class='fa fa-bar-chart'></i>` = "visual",
                                                     `<i class='fa-solid fa-microchip'></i>` = "model_sum"
                                                   ),
                                                   justified = TRUE,
                                                   selected = "visual"
                                                 ),
                                                 conditionalPanel(
                                                   condition = "input.change_plot == 'visual'",
                                                   plotlyOutput("plot")
                                                 ),
                                                 conditionalPanel(
                                                   condition = "input.change_plot == 'model_sum'",
                                                   verbatimTextOutput("fitted_model")
                                                 ),
                                                 br(),
                                                 uiOutput("model_accuracy")
                                               )
                                             )
                                           )
                                  )
                           )
)