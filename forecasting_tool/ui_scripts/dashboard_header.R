library(shiny)
library(shinydashboardPlus)

# dashboard header ####
dashboard_header <- dashboardHeader(
  leftUi = tagList(
    tags$li(class = "dropdown", style = "margin-top:5px;",
            actionButton("home", "", icon = icon("home"), 
                         no_outline = TRUE, style = "minimal")
    )
  ),
  title = uiOutput("header_ui"),
  titleWidth = 200, 
  ##### Dropdown menu for messages #####
  dropdownMenu(type = "tasks", badgeStatus = "success",
               headerText = "Upcoming/Ongoing Features -",
               messageItem(from = 'Soumyadipta Das',
                           message = "Model Completed!",
                           icon = icon("flag-checkered")
               )
  ),
  userOutput("user"),
  controlbarIcon = shiny::icon("hire-a-helper")
)