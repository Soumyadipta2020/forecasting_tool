library(shiny)

ui_design <- tags$head(tags$style(HTML('
      
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
        
         ')))