library(shiny)
library(shinydashboardPlus)

# API ####
source("api.R")

# Server scripts ####
source("server_scripts/mongodb_helper.R")

home_tab <- tabItem(tabName = "Home",
                    tabBox(id = "tabbox_1", width = 12,
                           #### Data Upload and visualization Tabpanel ####
                           tabPanel(
                             "Data", icon = icon("database"),
                             fluidPage(
                               selectInput('data_source', 'Select Data Source', 
                                           c('Upload', 'Sample'), selected = 'Upload'),
                               conditionalPanel(
                                 condition = "input.data_source == 'Upload'",
                                 fileInput("file", list(icon("file-csv"),"Upload Your File (.csv supported)"), accept = ".csv"),
                                 shinyFeedback::useShinyFeedback()
                               ),
                               conditionalPanel(
                                 condition = "input.data_source == 'Sample'",
                                 selectInput('sample_data', 'Select Sample Data', 
                                             choices = mongo_list('timeseries', mongo_url)), 
                                 actionButton('load_mongo', 'Load Data', 
                                              icon = icon('cloud-arrow-up')),
                                 br(), br()
                               ),
                               
                               splitLayout(downloadButton("file_template_download", "Download template file"),
                                           actionButton("upload_data", "Upload data", 
                                                        icon = icon('upload'))
                               ),
                               splitLayout("",
                                           p("*Please click after editing (if needed) the table below*", 
                                             style = "color:red")
                               ),
                               br(), #br(),
                               uiOutput("time_var"),
                               uiOutput("info_data"),
                               fluidRow(
                                 box(title = "Edit Data", collapsible = TRUE, status = "primary", solidHeader = TRUE, 
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
)