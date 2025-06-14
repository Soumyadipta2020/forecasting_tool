library(shiny)
library(shinydashboardPlus)

about_section <- tabItem(tabName = "About",
                         fluidPage(
                           h1(tags$img(src='brand_logo.png', height = '50', width ='50'), 
                              ("About the App")),
                           tagList(HTML(paste0("<b>","Github Repository: ","</b>")), a("Github", 
                                                                                       href="https://github.com/Soumyadipta2020/forecasting_tool")),
                           br(), br(),
                           "The AI Forecasting Tool, based on the R Shiny architecture, provides a comprehensive and user-friendly platform for all your data analysis needs. R Shiny, a web application framework for R, allows for the creation of interactive and visually appealing applications without requiring extensive web development skills. Leveraging this powerful framework, users can seamlessly upload their data, make necessary edits, and visualize the information in an intuitive manner. The tool offers a variety of models to choose from, allowing for tailored predictions and forecasts to suit specific requirements. Once the forecasting process is complete, users can easily extract the results for further use. Moreover, the tool includes an interactive AI component that enables discussions on multiple topics, providing valuable insights and guidance throughout the analysis process. This combination of advanced functionality, ease of use, and interactive features makes the AI Forecasting Tool an essential asset for data-driven decision-making.", 
                           h4("The top features are as follows -"),
                           tags$ul(
                             tags$li("Upload your data"),
                             tags$li("Edit and visualize your data"),
                             tags$li("Select desired model"),
                             tags$li("Predict and forecast"),
                             tags$li("Extract the results"),
                             tags$li("Discuss multiple topics with AI")
                           ),
                           h1("Latest Changelogs"),
                           fluidRow(
                             box(title = "2024-09-09", collapsible = TRUE, status = "success", solidHeader = TRUE, 
                                 width = 12, collapsed = TRUE,
                                 tags$ul(class = "tick-list",
                                         tags$li("Sample data added")
                                 )
                             ),
                             box(title = "2024-09-03", collapsible = TRUE, status = "success", solidHeader = TRUE, 
                                 width = 12, collapsed = TRUE,
                                 tags$ul(class = "tick-list",
                                         tags$li("Bug fixes & Adjustments")
                                 )
                             ),
                             box(title = "2024-08-12", collapsible = TRUE, status = "success", solidHeader = TRUE, 
                                 width = 12, collapsed = TRUE,
                                 tags$ul(class = "tick-list",
                                         tags$li("State Space ARIMA, ARFIMA forecasting models added")
                                 )
                             ),
                             box(title = "2024-07-29", collapsible = TRUE, status = "success", solidHeader = TRUE, 
                                 width = 12, collapsed = TRUE,
                                 tags$ul(class = "tick-list",
                                         tags$li("Prophet, GRNN, Neural Network forecasting model added")
                                 )
                             ),
                             box(title = "2024-07-13", collapsible = TRUE, status = "success", solidHeader = TRUE, 
                                 width = 12, collapsed = TRUE,
                                 tags$ul(class = "tick-list",
                                         tags$li("Model summary added for fitted models in forecasting tab"),
                                         tags$li("Switching between forecast plot and model summary is now possible"),
                                         # tags$li("Know your IP location added")
                                 )
                             ),
                             box(title = "2024-07-11", collapsible = TRUE, status = "success", solidHeader = TRUE, 
                                 width = 12, collapsed = TRUE,
                                 tags$ul(class = "tick-list",
                                         tags$li("Histogram added for summary statistics visualization"),
                                         tags$li("Variance, IQR & Standard deviation added for summary statistics"), 
                                         tags$li("LASSO & Ridge Regression issue resolved"),
                                         tags$li("Accuracy metrics added for models"),
                                         tags$li("Actuals added in forecast download"),
                                         tags$li("Fitted values used as forecast value during actual period")
                                 )
                             ),
                             box(title = "2024-07-07", collapsible = TRUE, status = "success", solidHeader = TRUE, 
                                 width = 12, collapsed = TRUE,
                                 tags$ul(class = "tick-list",
                                         tags$li("Data info added at homepage"),
                                         tags$li("New tab created - Summary Statistics"),
                                         tags$li("Boxplot, Violin Plot added"),
                                         tags$li("Missing value imputation added")
                                 )
                             ),
                             box(title = "2024-07-03", collapsible = TRUE, status = "success", solidHeader = TRUE, 
                                 width = 12, collapsed = TRUE,
                                 tags$ul(class = "tick-list",
                                         tags$li("Multimodal AI Chatbot added"),
                                         tags$li("Outlier treatment added"),
                                         tags$li("Data visualization and editing added"),
                                         tags$li("File template & error handling added")
                                 )
                             )
                           )
                           
                         )
)