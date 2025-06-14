# UI ####
ui <- shinydashboardPlus::dashboardPage(
  title = "AI Forecasting App",
  preloader = list(),
  skin = "blue",
  # options = list(sidebarExpandOnHover = TRUE),
  
  ## dashboard header ####
  header = dashboard_header,
  
  ## Sidebar ####
  sidebar = dashboard_sidebar,
  
  ## Dashboard Body ####
  body = dashboardBody(
    useWaiter(),
    shinyjs::useShinyjs(),
    use_copy(),
    use_theme(dashboard_body_theme),
    tags$script(HTML("$('body').addClass('fixed');")),
    # tags$head(
    #   tags$script(src="getIP.js")
    # ),
    ui_design,
    
    tabItems(
      ### Home tab ####
      home_tab,
      ### Forecasting tab ####
      forecasting_tab,
      # About Section ####
      about_section
    )
  ),
  # right sidebar ####
  controlbar = right_control_bar,
  
  footer = dashboardFooter(right = tags$a(href = "https://sites.google.com/view/soumyadipta-das", "Soumyadipta Das"), 
                           left = list(tags$img(src='brand_logo.png', 
                                                height = '25', width ='25'), "© 2023")), 
  scrollToTop = TRUE
)