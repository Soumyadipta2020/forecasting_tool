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
library(dashboardthemes)


dashboardPage(
  # skin = "blue",
  #### dashboard header ####
  dashboardHeader(
    title = shinyDashboardLogoDIY(
      textSize = 16,
      boldText = "Forecasting Tool",
      mainText = "",
      badgeBackColor = "#545454",
      badgeTextColor = "white",
      badgeText = "v.0.01",
      badgeBorderRadius = 3
    ),
    
    #### Dropdown menu for messages ####
    dropdownMenu(type = "notifications", badgeStatus = "warning",
                 messageItem("Issue",
                             "LSTM not working on web",
                             time = Sys.Date(),
                             icon = icon("circle-exclamation")
                 ),
                 messageItem("Limitations",
                             "General Models showing predicted values",
                             time = Sys.Date(),
                             icon = icon("circle-exclamation")
                 )
    ),
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
            )),
    dropdownMenu(
      headerText = "",
      icon = icon("user"),
      # icon = tags$figure(
      #   img(src="https://lh6.googleusercontent.com/8pvZAi7iQPOc933bIndP-lpbhRH3NAN6K-yS5NYq_WUEgrWafnWTRR67K8TkfdJS7BH-Q4k1r71zsO4iDaAvd_g=w16383",height=30,width=30)
      # ),
      badgeStatus = NULL,
      tags$li(
        class = "dropdown",
        HTML("<!-- wp:group {'layout':{'type':'constrained'}} -->
              <div class='wp-block-group'><!-- wp:media-text {'mediaId':61,'mediaType':'image'} -->
              <div class='wp-block-media-text is-stacked-on-mobile'><figure class='wp-block-media-text__media'><img src='self.JPG' alt='' class='wp-image-61 width='75' height='75' '/></figure><div class='wp-block-media-text__content'><!-- wp:paragraph {'placeholder':'Content…'} -->
              <p><strong>Soumyadipta Das</strong></p>
              <!-- /wp:paragraph -->
              
              <!-- wp:paragraph -->
              <p>Lead Assistant Manager, EXL</p>
              <!-- /wp:paragraph -->
              
              <!-- wp:paragraph {'fontSize':'small'} -->
              <p class='has-small-font-size'>Website - <a href='https://sites.google.com/view/soumyadipta-das'>Soumyadipta Das</a></p>
              <!-- /wp:paragraph -->
              
              <!-- wp:paragraph {'fontSize':'small'} -->
              <p class='has-small-font-size'>Email - <a href='mailto:soumyadipta_das@consultant.com'>soumyadipta_das@consultant.com</a></p>
              <!-- /wp:paragraph --></div></div>
              <!-- /wp:media-text --></div>
              <!-- /wp:group -->"
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
      menuItem("Forecasting_tab", tabName = "Forecasting", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    use_theme(dashboard_body_theme),
    tabItems(
      #### Forecasting tab ####
      tabItem(tabName = "Forecasting",
              tabBox(id = "tabbox_1", width = 12,
                     #### Forecasting tabpanel ####
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
  )
)

