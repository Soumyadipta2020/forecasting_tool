#### dashboard sidebar theme ####
dashboard_sidebar_theme <- create_theme( 
  adminlte_sidebar(
    # dark_bg = "#006666",
    # dark_hover_bg = "#202020"
  )
)

#### dashboard theme ####
dashboard_body_theme <- create_theme(
  theme = "cosmo",
  bs_vars_button(
    default_color = "#FFF",
    default_bg = "#0066cc",
    # default_border = "white",
    border_radius_base = "10px"
  ),
  bs_vars_tabs(
    border_color = "black",
    active_link_hover_bg = "#CCE5FF"
  ),
  output_file = NULL
)

#### user panel theme ####
myDashboardUser <- function (..., name = NULL, image = NULL, title = NULL, subtitle = NULL, 
                             footer = NULL) 
{
  if (!is.null(title)) {
    line_1 <- paste0(name, " - ", title)
  }
  else {
    line_1 <- name
  }
  if (!is.null(subtitle)) {
    user_text <- shiny::tags$p(line_1, shiny::tags$small(subtitle))
    user_header_height <- NULL
  }
  else {
    user_text <- shiny::tags$p(line_1)
    user_header_height <- shiny::tags$script(
      shiny::HTML("$(\".user-header\").css(\"height\", \"145px\")")
    )
  }
  userTag <- shiny::tagList(
    shiny::tags$head(
      shiny::tags$script("$(function() {\n
                           $('.dashboard-user').on('click', function(e){\n
                           e.stopPropagation();\n
                           });\n
                           });\n
                           ")),
    # we need to add an id and the class `action-button` to this link
    shiny::tags$a(id = "user_dropdown",
                  href = "#",
                  class = "dropdown-toggle action-button",
                  `data-toggle` = "dropdown",
                  shiny::tags$img(src = image,
                                  class = "user-image",
                                  alt = "User Image"),
                  shiny::tags$span(class = "hidden-xs",
                                   name)
    ),
    shiny::tags$ul(class = "dropdown-menu dashboard-user", 
                   shiny::tags$li(class = "user-header",
                                  if (!is.null(user_header_height)) user_header_height,
                                  shiny::tags$img(src = image, 
                                                  class = "img-circle",
                                                  alt = "User Image",
                                                  style="border:red"),
                                  user_text,
                                  style="background:#0066cc"), 
                   if (length(list(...)) > 0) 
                     shiny::tags$li(class = "user-body", ...),
                   if (!is.null(footer)) 
                     shiny::tags$li(class = "user-footer", footer)
    )
  )
  userTag
}