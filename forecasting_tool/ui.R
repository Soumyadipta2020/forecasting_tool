#### UI ####
ui <- shinydashboardPlus::dashboardPage(
  title = "AI Forecasting Tool",
  preloader = list(),
  skin = "blue",
  # options = list(sidebarExpandOnHover = TRUE),
  #### dashboard header ####
  header = dashboardHeader(
    leftUi = tagList(
      tags$li(class = "dropdown", style = "margin-top:5px;",
              actionButton("home", "", icon = icon("home"), 
                           no_outline = TRUE, style = "minimal")
      )
    ),
    title = uiOutput("header_ui"),
    titleWidth = 200, 
    ##### Dropdown menu for messages #####
    dropdownMenu(type = "tasks", badgeStatus = "warning",
                 headerText = "Upcoming/Ongoing Features -",
                 messageItem(from = NULL,
                             message = "Summary Statistics of uploaded data",
                             icon = icon("spinner")
                 )
    ),
    userOutput("user"),
    controlbarIcon = shiny::icon("hire-a-helper")
  ),
  #### Sidebar ####
  sidebar = dashboardSidebar(minified = TRUE, #collapsed = TRUE,
                             use_theme(dashboard_sidebar_theme),
                             ##### Sidebar menu #####
                             sidebarMenu(
                               id = "sidebar",
                               
                               ###### menuitem ######
                               menuItem("Home", tabName = "Home", icon = icon("home")),
                               menuItem("Forecasting", tabName = "Forecasting", icon = icon("chart-line")),
                               menuItem("About", tabName = "About", icon = icon("circle-info"))
                             )
  ),
  body = dashboardBody(
    useWaiter(),
    shinyjs::useShinyjs(),
    use_copy(),
    # div(
    #   id = "myModal",
    #   shiny::modalDialog(
    #     title = "Reload RShiny",
    #     "Press the reload button if you want to reload RShiny -",
    #     fade = TRUE,
    #     footer = tagList(
    #       modalButton("Cancel", icon = icon("circle-xmark")),
    #       actionButton("reload", "Reload", icon = icon("arrows-rotate"))
    #     ),
    #     easyClose = FALSE
    #   )
    # ),
    use_theme(dashboard_body_theme),
    # tags$head(tags$style(HTML(
    #   '.myClass { 
    #     font-size: 16px;
    #     line-height: 50px;
    #     text-align: centre;
    #     font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
    #     padding: 0 15px;
    #     overflow: hidden;
    #     color: white;
    #   }
    # '))),
    # tags$script(HTML('
    #   $(document).ready(function() {
    #     $("header").find("nav").append(\'<span class="myClass" style="font-size: 16px;"> Forecasting Tool </span> <span class="version-badge" style="border-radius: 10px; font-size: small; background-color: #545454; color: white;"> &nbsp; v.0.02 &nbsp;  </span> \');
    #   })
    #  ')),
    tags$script(HTML("$('body').addClass('fixed');")),
    tags$head(tags$style(HTML('
      
        .dropdown-menu {
          width: 300px !important;
        }
       
    
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #0066cc;
        }
                              
        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #66b3ff;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #0066cc;
                              }        

        /* main sidebar */
        /* .skin-blue .main-sidebar {
                              background-color: #0080ff;
                              } */

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #66b3ff;
                              }

        /* other links in the sidebarmenu */
        /* .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #00ff00;
                              color: #000000;
                              } */

        /* other links in the sidebarmenu when hovered */
        /* .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #ff69b4;
                              } */
        /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #66b3ff;
         }
        
        /* box header */                      
        .box.box-solid.box-primary>.box-header {
                /*color: black;*/
                background: #0066cc;
                }

         /* box body */
         .box.box-solid.box-primary{
                border-bottom-color:#222d32;
                border-left-color:#222d32;
                border-right-color:#222d32;
                border-top-color:#222d32;
                background: #e6f2ff;
         } 
        
        /* background */ 
        .content-wrapper,.right-side {
          background-color: #e6f7ff;
        }
        
        /* fluid page */
        /*.container-fluid { background-color: #cceeff; }*/
        
        /* tab background */
        /*.nav-tabs {
            background-color: #cce6ff;
        }*/
        
        /*.nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
            /*background-color: #99ccff;*/
            border-color: black;
        }
        
        .nav-tabs-custom .nav-tabs li.active {
            border-top-color: black;
        }
        
        /* pointer icons */
        .tick-list {
        list-style-type: none; /* Remove default bullets */
        padding: 0;
        }
        .tick-list li {
          position: relative;
          padding-left: 25px; /* Space for the tick mark */
          /*margin-bottom: 10px;*/
        }
        .tick-list li:before {
          content: "\\2713"; /* Unicode for tick mark */
          position: absolute;
          left: 0;
          color: green; /* Tick mark color */
        }
        
         '))),
    tabItems(
      #### Home tab ####
      tabItem(tabName = "Home",
              tabBox(id = "tabbox_1", width = 12,
                     #### Data Upload and visualization Tabpanel ####
                     tabPanel(
                       "Data", icon = icon("database"),
                       fluidPage(
                         fileInput("file", list(icon("file-csv"),"Upload Your File (.csv supported)"), accept = ".csv"),
                         shinyFeedback::useShinyFeedback(),
                         splitLayout(downloadButton("file_template_download", "Download template file"),
                                     actionButton("upload_data", "Upload data")
                         ),
                         br(), br(),
                         uiOutput("info_data"),
                         fluidRow(
                           box(title = "Uploaded Data", collapsible = TRUE, status = "primary", solidHeader = TRUE, 
                               width = 12, collapsed = TRUE,
                               dataTableOutput("uploaded_data")
                           )
                         ),
                         fluidRow(
                           box(title = "Quick Visualization", collapsible = TRUE, status = "primary", solidHeader = TRUE, 
                               width = 12, collapsed = TRUE,
                               uiOutput("response_variable_graph"),
                               uiOutput("x_variable_graph"),
                               # fluidRow(
                               #   box(title = "Graph", solidHeader = TRUE, 
                               #     status = "primary", width = 12,
                               echarts4rOutput("vis_data")
                               #   )
                               # )
                           )
                         )
                       )
                     ),
                     #### Summary stat ####
                     tabPanel(
                       "Summary Statistics", icon = icon("chart-pie"),
                       fluidPage(
                         uiOutput("vars_stat"),
                         fluidRow(
                           box(title = "Summary Statistics", collapsible = TRUE, status = "primary", solidHeader = TRUE, 
                               width = 12, collapsed = TRUE,
                               dataTableOutput("summary_stat_table"),
                               downloadButton("summary_stat_download", "Download Summary Statistics")
                           ),
                           box(title = "Visualization", collapsible = TRUE, status = "primary", solidHeader = TRUE, 
                               width = 12, collapsed = FALSE,
                               selectInput("summary_stat_plot_type", "Plot Type",
                                           choices = c("Boxplot", "Violin Plot", "Histogram"), 
                                           selected = "Violin Plot"),
                               plotlyOutput("summary_stat_vis")
                           )
                         )
                       )
                     )
              )
      ),
      
      tabItem(tabName = "Forecasting",
              tabBox(id = "tabbox_2", width = 12,
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
                                                              # "LSTM", 
                                                              "AutoML", "ETS"))
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
                                    plotlyOutput("plot"),
                                    br(), 
                                    # h1("Accuracy Measures -"),
                                    uiOutput("model_accuracy")
                                  )
                                )
                              )
                     )
              )
      ),
      
      # About Section ####
      tabItem(tabName = "About",
              fluidPage(
                h1(tags$img(src='brand_logo.png', height = '50', width ='50'), 
                   ("About the App")),
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
                h1("Changelog"),
                fluidRow(
                  box(title = "2024-07-15", collapsible = TRUE, status = "success", solidHeader = TRUE, 
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
    )
  ),
  # right sidebar ####
  controlbar = dashboardControlbar(
    skin = "dark",
    collapsed = FALSE,
    overlay = FALSE,
    width = 400,
    
    controlbarMenu(
      id = "menu",
      controlbarItem(
        fluidPage(
          # "Assistant",
          # LLM Models #####
          selectInput(
            "model_gen",
            "AI Model",
            choices = c(
              "Meta-Llama-3",
              # "gpt-3.5-turbo",
              "gemini-pro",
              "microsoft-Phi-3-mini",
              # "claude-2.1",
              # "claude-instant",
              "google-gemma-7b-it",
              "Mixtral-v0.1",
              "Mistral-v0.3",
              "Yi-1.5"
            ),
            selected = "Meta-Llama-3-8B-Instruct"
          ), 
          sliderInput(
            "temperature",
            "Temperature (gemini & gpt only)",
            min = 0,
            max = 1,
            value = 0.5, 
            step = 0.1
          ),
          # Chat container #####
          tags$div(
            id = "chat-container",
            tags$div(id = "chat-history", 
                     style = "overflow-y: scroll; height: 280px; display: flex; flex-direction: column; 
                     text-align:left;", 
                     uiOutput("chat_history")),
            
            tags$div(id = "chat-input", tags$form(
              textAreaInput(
                inputId = "prompt",
                label = "",
                placeholder = "Type your message here...",
                width = "100%"
              ),
              # fileInput("file_chat", "Upload (.docx, .pptx)", accept = c(".docx", ".pptx")),
              fluidRow(
                tags$div(
                  style = "margin-left: 0em;",
                  actionButton(
                    inputId = "chat",
                    label = "Send",
                    icon = icon("paper-plane")
                  ),
                  actionButton(
                    inputId = "remove_chatThread",
                    label = "Clear History",
                    icon = icon("trash-can")
                  ),
                  CopyButton(
                    "clipbtn",
                    label = "Copy",
                    icon = icon("clipboard"),
                    text = ""
                  )
                  
                )
              )
            ))
          )
          # Chat Container end #####
          
        )
      )
    )
  ),
  footer = dashboardFooter(right = tags$a(href = "https://sites.google.com/view/soumyadipta-das", "Soumyadipta Das"), 
                           left = list(tags$img(src='brand_logo.png', 
                                                height = '25', width ='25'), "© 2023")), 
  scrollToTop = TRUE
)