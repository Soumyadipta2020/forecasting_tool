#### UI ####
ui <- shinydashboardPlus::dashboardPage(
    title = "Forecasting Tool",
    preloader = list(),
    skin = "blue",
    # options = list(sidebarExpandOnHover = TRUE),
    #### dashboard header ####
    header = dashboardHeader(
      leftUi = tagList(),
      title = uiOutput("header_ui"),
        # HTML(paste("<span style='font-size: 16px;'>", "Forecasting Tool", "</span>",
        #                  "<span class='version-badge' style='border-radius: 10px; font-size: small; background-color: #545454;'>",
        #                  "&nbsp; v.0.02 &nbsp;", "</span>")),
      titleWidth = 200, 
      ##### Dropdown menu for messages #####
      dropdownMenu(type = "notifications", badgeStatus = "warning",
                   messageItem("Feature",
                               "Multimodal AI Chatbot added",
                               time = "2024-07-01",
                               icon = icon("square-check")
                   ),
                   messageItem("Feature",
                               "Outlier treatment added",
                               time = "2023-09-25",
                               icon = icon("square-check")
                   ),
                   messageItem("Feature",
                               "Data visualization and editing added",
                               time = "2023-09-16",
                               icon = icon("square-check")
                   ),
                   messageItem("Feature",
                               "File template & error handling added",
                               time = "2023-09-07",
                               icon = icon("square-check")
                   )
      ),
      # tags$li(class = "dropdown", 
      #         tags$a(href = "https://www.linkedin.com/in/soumyadipta-das/",
      #                "Linked", icon("linkedin"), target = "_blank"
      #         )), 
      # tags$li(class = "dropdown", 
      #         tags$a(href = "https://sites.google.com/view/soumyadipta-das",
      #                tags$script(src = "https://kit.fontawesome.com/b8fb1ea7bd.js"),
      #                icon = NULL,
      #                tags$div(
      #                  tags$i(class = "fa-regular fa-id-card"),
      #                  tags$span("  Soumyadipta Das")
      #                ), target = "_blank"
      #         )),
      # dropdownMenu(
      #   headerText = "",
      #   icon = icon("user"),
      # icon = tags$figure(
      #   img(src="https://lh6.googleusercontent.com/8pvZAi7iQPOc933bIndP-lpbhRH3NAN6K-yS5NYq_WUEgrWafnWTRR67K8TkfdJS7BH-Q4k1r71zsO4iDaAvd_g=w16383",height=30,width=30)
      # ),
      #   badgeStatus = NULL,
      #   tags$li(
      #     class = "dropdown",
      #     HTML("<!-- group {'layout':{'type':'constrained'}} -->
      #           <div class='wp-block-group'><!-- media-text {'mediaId':61,'mediaType':'image'} -->
      #           <div class='wp-block-media-text is-stacked-on-mobile'><figure style = 'margin-left:15px;' class='wp-block-media-text__media'><img src='self.JPG' alt='' class='wp-image-61 width='75' height='75' '/></figure><div class='wp-block-media-text__content'><!-- paragraph {'placeholder':'Content…'} -->
      #           <p style = 'margin-left:15px;'><strong>Soumyadipta Das</strong></p>
      #           <!-- /paragraph -->
      #           
      #           <!-- paragraph -->
      #           <p style = 'margin-left:15px;'>Lead Assistant Manager, EXL</p>
      #           <!-- /paragraph -->
      #           
      #           <!-- paragraph {'fontSize':'small'} -->
      #           <p style = 'margin-left:15px;' class='has-small-font-size'>Website - <a href='https://sites.google.com/view/soumyadipta-das'>Soumyadipta Das</a></p>
      #           <!-- /paragraph -->
      #           
      #           <!-- paragraph {'fontSize':'small'} -->
      #           <p style = 'margin-left:15px;' class='has-small-font-size'>Email - <a href='mailto:soumyadipta_das@consultant.com'>soumyadipta_das@consultant.com</a></p>
      #           <!-- /paragraph --></div></div>
      #           <!-- /media-text --></div>
      #           <!-- /group -->"
      #     )
      #   )
      # ),
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
        menuItem("Forecasting", tabName = "Forecasting", icon = icon("chart-line"))
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
      tags$head(tags$style(HTML('
    
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
        
         '))),
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
                                 width = 12, collapsed = TRUE,
                                 dataTableOutput("uploaded_data")
                             )
                           ),
                           fluidRow(
                             box(title = "Graphical Visualization of Data", collapsible = TRUE, status = "primary", solidHeader = TRUE, 
                                 width = 12,
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
                                      plotlyOutput("plot")
                                    )
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
      width = 350,
      
      controlbarMenu(
        id = "menu",
        controlbarItem(
          "Assistant",
          # LLM Models #####
          selectInput(
            "model_gen",
            "Generative AI Model",
            choices = c(
              "Meta-Llama-3",
              # "gpt-3.5-turbo",
              "gemini-pro",
              "microsoft-Phi-3-mini",
              "claude-2.1",
              "claude-instant",
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
                     style = "overflow-y: scroll; height: 180px; display: flex; flex-direction: column;", 
                     uiOutput("chat_history")),
            
            tags$div(id = "chat-input", tags$form(
              textAreaInput(
                    inputId = "prompt",
                    label = "",
                    placeholder = "Type your message here...",
                    width = "100%"
                  ),
              fileInput("file_chat", "Upload (.docx, .pptx)", accept = c(".docx", ".pptx")),
              fluidRow(
                tags$div(
                  style = "margin-left: 1.5em;",
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
    ),
    footer = dashboardFooter(right = "By Soumyadipta Das", left = "2023"), 
    scrollToTop = TRUE
  )