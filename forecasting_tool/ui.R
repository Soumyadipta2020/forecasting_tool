rm(list=ls())

#### Library ####
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
library(shinydashboardPlus)
library(dashboardthemes)
library(DT)


dashboardPage(
  options = list(sidebarExpandOnHover = TRUE),
  # skin = "blue",
  #### dashboard header ####
  dashboardHeader(
    title = HTML(paste("<span style='font-size: 16px;'>", "Forecasting Tool", "</span>",
                       "<span class='version-badge' style='border-radius: 10px; font-size: small; background-color: #545454;'>", 
                       "&nbsp; v.0.02 &nbsp;", "</span>")),
    titleWidth = 200,
    
    #### Dropdown menu for messages ####
    dropdownMenu(type = "notifications", badgeStatus = "warning",
                 messageItem("Feature",
                             "Data visualization and editing added",
                             time = "2023-09-16",
                             icon = icon("square-check")
                 ),
                 messageItem("Feature",
                             "File template & error handling added",
                             time = "2023-09-07",
                             icon = icon("square-check")
                 ),
                 messageItem("Issue",
                             "LSTM not working on web",
                             time = "2023-08-17",
                             icon = icon("circle-exclamation")
                 ),
                 messageItem("Limitations",
                             "General Models showing predicted values",
                             time = "2023-08-17",
                             icon = icon("circle-exclamation")
                 )
    ),
    tags$li(class = "dropdown", 
            tags$a(href = "https://www.linkedin.com/in/soumyadipta-das/",
                   "Linked", icon("linkedin"), target = "_blank"
            )), 
    # tags$li(class = "dropdown", 
    #         tags$a(href = "https://sites.google.com/view/soumyadipta-das",
    #                tags$script(src = "https://kit.fontawesome.com/b8fb1ea7bd.js"),
    #                icon = NULL,
    #                tags$div(
    #                  tags$i(class = "fa-regular fa-id-card"),
    #                  tags$span("  Soumyadipta Das")
    #                ), target = "_blank"
    #         )),
    dropdownMenu(
      headerText = "",
      icon = icon("user"),
      # icon = tags$figure(
      #   img(src="https://lh6.googleusercontent.com/8pvZAi7iQPOc933bIndP-lpbhRH3NAN6K-yS5NYq_WUEgrWafnWTRR67K8TkfdJS7BH-Q4k1r71zsO4iDaAvd_g=w16383",height=30,width=30)
      # ),
      badgeStatus = NULL,
      tags$li(
        class = "dropdown",
        HTML("<!-- group {'layout':{'type':'constrained'}} -->
              <div class='wp-block-group'><!-- media-text {'mediaId':61,'mediaType':'image'} -->
              <div class='wp-block-media-text is-stacked-on-mobile'><figure style = 'margin-left:15px;' class='wp-block-media-text__media'><img src='self.JPG' alt='' class='wp-image-61 width='75' height='75' '/></figure><div class='wp-block-media-text__content'><!-- paragraph {'placeholder':'Content…'} -->
              <p style = 'margin-left:15px;'><strong>Soumyadipta Das</strong></p>
              <!-- /paragraph -->
              
              <!-- paragraph -->
              <p style = 'margin-left:15px;'>Lead Assistant Manager, EXL</p>
              <!-- /paragraph -->
              
              <!-- paragraph {'fontSize':'small'} -->
              <p style = 'margin-left:15px;' class='has-small-font-size'>Website - <a href='https://sites.google.com/view/soumyadipta-das'>Soumyadipta Das</a></p>
              <!-- /paragraph -->
              
              <!-- paragraph {'fontSize':'small'} -->
              <p style = 'margin-left:15px;' class='has-small-font-size'>Email - <a href='mailto:soumyadipta_das@consultant.com'>soumyadipta_das@consultant.com</a></p>
              <!-- /paragraph --></div></div>
              <!-- /media-text --></div>
              <!-- /group -->"
        )
      )
    )
  ),
  dashboardSidebar(
    use_theme(dashboard_sidebar_theme),
    #### Sidebar menu ####
    sidebarMenu(
      id = "sidebar",
      
      #### menuitem ####
      menuItem("Forecasting", tabName = "Forecasting", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    use_theme(dashboard_body_theme),
    tabItems(
      #### Forecasting tab ####
      tabItem(tabName = "Forecasting",
              tabBox(id = "tabbox_1", width = 12,
                     #### Data Upload and visualization Tabpanel ####
                     tabPanel(
                       "Data Visualization", icon = icon("table"),
                       fluidPage(
                         fileInput("file", "Upload Your File (.csv supported)"),
                         shinyFeedback::useShinyFeedback(),
                         splitLayout(downloadButton("file_template_download", "Download template file"),
                                     actionButton("upload_data", "Upload data")
                         ),
                         br(), br(),
                         fluidRow(
                           box(title = "Uploaded Data", collapsible = TRUE, status = "primary", solidHeader = TRUE, 
                               width = 12,
                               dataTableOutput("uploaded_data")
                           )
                         )
                       )
                     ),
                     
                     #### Forecasting tabpanel ####
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
                                                  choices = c("ARIMA", "SARIMA", "ARCH", "GARCH",
                                                              "LSTM", "AutoML", "ETS"))
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
                                    plotlyOutput("plot")
                                  )
                                )
                              )
                     )
              )
      )
    )
  )#,
  # controlbar = dashboardControlbar()
)

