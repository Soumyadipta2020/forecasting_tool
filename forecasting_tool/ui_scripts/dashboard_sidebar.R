library(shiny)
library(shinydashboardPlus)

dashboard_sidebar <- dashboardSidebar(minified = TRUE, #collapsed = TRUE,
                                      use_theme(dashboard_sidebar_theme),
                                      ##### Sidebar menu #####
                                      sidebarMenu(
                                        id = "sidebar",
                                        ###### menuitem ######
                                        menuItem("Home", tabName = "Home", icon = icon("home")),
                                        menuItem("Forecasting", tabName = "Forecasting", icon = icon("chart-line")),
                                        # menuItem("Your Location", tabName = "Users", icon = icon("map-location-dot")),
                                        menuItem("About", tabName = "About", icon = icon("circle-info"))
                                      )
)