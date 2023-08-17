library(fresh)

dashboard_header_theme <- create_theme(
  adminlte_color(
    light_blue = "#E7FF6E"
  )
)

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
    default_bg = "#112446",
    default_border = "#112446",
    border_radius_base = "15px"
  ),
  bs_vars_tabs(
    border_color = "black",
    active_link_hover_bg = "#CCE5FF"
  ),
  output_file = NULL
)

