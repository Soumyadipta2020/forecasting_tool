# Server
server <- function(input, output, session) {
  
  shinyjs::runjs("$('#myModal').modal('show');")
  observeEvent(input$reload, {
    session$reload()
  })
  
  # observeEvent(input$exit, {
  #   stopApp()
  # })
  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  
  #### Dashboard user window ####
  output$user <- renderUser({
    myDashboardUser(
      name = "Soumyadipta Das", 
      image = "self.jpg", 
      title = "Consultant II @ EXL",
      subtitle = "Author", 
      # footer = p("The footer", class = "text-center"),
      fluidRow(
        dashboardUserItem(
          width = 6,
          socialButton(
            href = "https://sites.google.com/view/soumyadipta-das",
            icon = icon("user")
          )
        ),
        dashboardUserItem(
          width = 6,
          socialButton(
            href = "https://www.linkedin.com/in/soumyadipta-das/",
            icon = icon("linkedin-in")
          )
        ),
        dashboardUserItem(
          width = 6,
          socialButton(
            href = "mailto:soumyadipta_das@consultant.com",
            icon = icon("envelope")
          )
        ),
        dashboardUserItem(
          width = 6,
          socialButton(
            href = "https://github.com/Soumyadipta2020/",
            icon = icon("square-github")
          )
        )
      )
    )
  })
  
  #### File template download ####
  output$file_template_download <- downloadHandler(
    filename = function(){
      paste("Sample data", ".csv", sep = "")
    },
    content = function(file){
      temp = read.csv("timeseries demo.csv", header = TRUE)
      write.csv(temp, file, row.names = FALSE)
    }
  )
  #### data upload ####
  data_old <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    
    ##### column type checking #####
    col_type_check <- ((sum(1*((as.vector(sapply(df, class)))=="numeric")) + 
                          sum(1*((as.vector(sapply(df, class)))=="integer"))) >= ncol(df) - 1)
    if(!col_type_check){
      shinyFeedback::hideFeedback("file")
      shinyFeedback::feedbackDanger("file", !col_type_check, "Invalid file format. Please download the template.")
      req(col_type_check)
    } else{
      shinyFeedback::hideFeedback("file")
      shinyFeedback::feedbackSuccess("file", col_type_check, "Successfully Uploaded.")
      req(col_type_check)
      
      #### render data table #####
      output$uploaded_data <- renderDT(df, editable = TRUE, filter = "top", selection = 'none', rownames = FALSE)
      
      df1 <<- df
      
      #### data edit step note #####
      observeEvent(input$uploaded_data_cell_edit, {
        info = input$uploaded_data_cell_edit
        str(info)
        i = info$row
        j = info$col + 1
        v = info$value
        df1[i, j] <<- v
        output$uploaded_data <- renderDT(df1, editable = TRUE, filter = "top", selection = 'none', rownames = FALSE)
        temp <- data.frame(row = i, col = j, value = v)
        data_edit <<- rbind(data_edit, temp)
      })
      
      return(df)
    }
  })
  #### main data ####
  data <- reactive({
    df <- data_old()
    req(input$upload_data != 0)
    ##### modification of data as per edit #####
    data_edit_1 <- data_edit %>% tidyr::drop_na()
    if(nrow(data_edit_1) != 0){
      for(k in 1:nrow(data_edit_1)){
        df[data_edit_1[k, 1], data_edit_1[k, 2]] <- data_edit_1[k, 3]
      }
    }
    #### Outlier treatment #####
    for(i in 2:ncol(df)){
      value = df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
      df[,i][df[,i] %in% value] = median(df[,i])
    }
    
    return(df)
  })
  
  #### graph y variable ####
  output$response_variable_graph <- renderUI({
    req(data_old())
    selectInput("y_variable_graph", "Select Y Variable",
                choices = colnames(data_old()))
  })
  #### graph x variable ####
  output$x_variable_graph <- renderUI({
    req(data_old())
    selectInput("x_variables_graph", "Select X Variables",
                choices = setdiff(colnames(data_old()), input$y_variable_graph))
  })
  #### graph data ####
  data_graph <- reactive({
    temp <- data_old() %>% select(input$x_variables_graph, input$y_variable_graph) %>% 
      rename(Var1 = input$x_variables_graph, Var2 = input$y_variable_graph)
    return(temp)
  })
  #### graph ####
  output$vis_data <- renderEcharts4r({
    req(data_graph(), input$x_variables_graph, input$y_variable_graph)
    #### time series plot ####
    data_graph() %>% 
      e_charts(x = Var1) %>% 
      e_line(Var2) %>% 
      e_toolbox() %>%
      e_toolbox_feature(
        feature = "magicType",
        type = list("line", "bar")
      ) %>% 
      e_toolbox_feature(feature = "dataView") %>% 
      e_toolbox_feature(feature = "dataZoom") %>%
      e_toolbox_feature(feature = "restore") %>%
      e_toolbox_feature(feature = "saveAsImage")
    
  })
  
  #### model y variable ####
  output$response_variable <- renderUI({
    req(data())
    selectInput("response_variable", "Select Response Variable",
                choices = colnames(data()))
  })
  #### model x variables ####
  output$x_variables <- renderUI({
    req(data(), input$response_variable, input$data_type, input$upload_data)
    if (input$data_type == "Non-Time Series") {
      selectInput("x_variables", "Select X Variables",
                  choices = setdiff(colnames(data()), input$response_variable),
                  multiple = TRUE)
    } else {
      NULL
    }
  })
  #### transform to ts ####
  tsData <- reactive({
    req(data(), input$response_variable)
    if (input$data_type == "Time Series") {
      if (input$seasonal) {
        if (!is.null(input$seasonal_period)) {
          ts(data()[[input$response_variable]], frequency = input$seasonal_period)
        } else {
          freq <- forecast::findfrequency(data()[[input$response_variable]])
          if (is.null(freq)) {
            freq <- 12  # Default seasonal period
          }
          ts(data()[[input$response_variable]], frequency = freq)
        }
      } else {
        ts(data()[[input$response_variable]], frequency = 1)
      }
    } else {
      NULL
    }
  })
  #### update y variable choices ####
  observe({
    choices <- colnames(data())
    updateSelectInput(session, "response_variable", choices = choices)
  })
  #### forecasting function ####
  forecastData <- reactive({
    if (input$data_type == "Time Series") {
      req(tsData(), input$model)
      model <- switch(input$model,
                      "ARIMA" = auto.arima(tsData()),
                      "SARIMA" = auto.arima(tsData(), seasonal = TRUE),
                      "ARCH" = {
                        spec <- ugarchspec(variance.model = list(model = "sGARCH"))
                        fit <- ugarchfit(spec, data = tsData())
                        fitted(fit)
                      },
                      "GARCH" = {
                        spec <- ugarchspec(variance.model = list(model = "gjrGARCH"))
                        fit <- ugarchfit(spec, data = tsData())
                        fitted(fit)
                      },
                      "LSTM" = lstm_forecast(tsData(), input$horizon),
                      "AutoML" = automl_forecast(tsData(), input$horizon),
                      "ETS" = forecast::forecast(tsData(), h = input$horizon)
      )
      
      if (!is.null(model)) {
        forecast_values <- forecast(model, h = input$horizon)
        forecast_values
      }
    } 
    
    if (input$data_type == "Non-Time Series") {
      req(!is.null(input$response_variable), input$x_variables, input$model1)
      
      model <- switch(input$model1,
                      "Linear Regression" = {
                        model_fit <- lm(data = data(),
                                        formula = as.formula(paste(input$response_variable, "~", paste(input$x_variables, collapse = " + "))))
                        model_fit
                      },
                      "GLM" = {
                        model_fit <- glm(data = data(),
                                         formula = as.formula(paste(input$response_variable, "~", paste(input$x_variables, collapse = " + "))),
                                         family = gaussian)
                        model_fit
                      },
                      "Logistic Regression" = {
                        model_fit <- glm(data = data(),
                                         formula = as.formula(paste(input$response_variable, "~", paste(input$x_variables, collapse = " + "))),
                                         family = binomial)
                        model_fit
                      },
                      "LASSO" = {
                        model_fit <- glmnet(x = as.matrix(data()[input$x_variables]), 
                                            y = data()[input$response_variable][,1], alpha = 1)
                        model_fit
                      },
                      "Ridge Regression" = {
                        model_fit <- glmnet(x = as.matrix(data()[input$x_variables]), 
                                            y = data()[input$response_variable][,1], alpha = 0)
                        model_fit
                      }
      )
      
      if (!is.null(model)) {
        # Check if forecast function exists
        if (input$model1 %in% c("Linear Regression", "GLM", "Logistic Regression")){
          forecast_values <- predict(model, newdata = data.frame(data()[input$x_variables]))
          forecast_values <- as.numeric(forecast_values)
        } else if (input$model1 %in% c("Classification", "LASSO", "Ridge Regression")) {
          # For non-time series data, use the fitted model to get forecasted values
          forecast_values <- predict(model, newx = as.matrix(data()[input$x_variables]))
          print(forecast_values)
          forecast_values <- as.numeric(forecast_values)
        }
        
      } else{
        NULL
      }
    } 
    
    return(forecast_values)
  })
  #### forecast ####
  observeEvent(input$forecast, {
    forecastData()
  })
  
  #### render forecast graph ####
  output$plot <- renderPlotly({
    req(data(), input$response_variable, input$forecast, forecastData())
    
    if(input$data_type == "Time Series"){
      plot_data <- data.frame(
        x = time(tsData()),
        y = as.numeric(tsData()),
        type = "scatter",
        mode = "lines",
        name = "Data"
      )
    }
    
    if(input$data_type == "Non-Time Series"){
      plot_data <- data.frame(
        x = dplyr::row_number(data()),
        y = data()[input$response_variable][,1],
        type = "scatter",
        mode = "lines",
        name = "Data"
      )
      
    }
    
    
    if (!is.null(forecastData())) {
      
      if(input$data_type == "Time Series"){
        forecast_values <- forecastData()$mean
        forecast_length <- length(forecast_values)
        forecast_dates <- seq(time(tsData())[length(tsData())] + 1/12, by = 1/12, length.out = forecast_length)
        
        forecast_plot_data <- data.frame(
          x = c(time(tsData()), forecast_dates),
          y = c(as.numeric(tsData()), forecast_values),
          type = "scatter",
          mode = "lines",
          name = "Forecast"
        )
      } 
      if(input$data_type == "Non-Time Series"){
        forecast_values <- forecastData()
        
        forecast_plot_data <- data.frame(
          x = dplyr::row_number(data()),
          y = forecast_values,
          type = "scatter",
          mode = "lines",
          name = "Forecast"
        )
        
      }
      
      
      plot_ly() %>%
        add_trace(data = plot_data, x = ~x, y = ~y, type = "scatter", mode = "lines", name = "Data") %>%
        add_trace(data = forecast_plot_data, x = ~x, y = ~y, type = "scatter", mode = "lines", name = "Forecast") %>%
        layout(
          title = "Actual Data and Forecast",
          xaxis = list(title = "Time"),
          yaxis = list(title = "Value")
        )
    } else {
      plot_ly() %>%
        add_trace(data = plot_data, x = ~x, y = ~y, type = "scatter", mode = "lines", name = "Data") %>%
        layout(
          title = "Actual Data",
          xaxis = list(title = "Time"),
          yaxis = list(title = "Value")
        )
    }
  })
  
  #### download forecast ####
  output$download <- downloadHandler(
    filename = function(){
      paste0("forecast_", if_else(input$data_type == "Time Series", input$model, input$model1), ".csv")
    },
    content = function(file) {
      if (!is.null(forecastData())) {
        if(input$data_type == "Time Series"){
          forecast_values <- forecastData()$mean
          forecast_dates <- seq(time(tsData())[length(tsData())] + 1/12, by = 1/12, length.out = input$horizon)
          forecast_df <- data.frame(Date = forecast_dates, Forecast = forecast_values)
          write.csv(forecast_df, file, row.names = FALSE)
        } 
        
        if(input$data_type == "Non-Time Series"){
          forecast_values <- forecastData()
          forecast_seq <- seq(1, by = 1, length.out = length(forecastData()))
          forecast_df <- data.frame(Sequence = forecast_seq, Forecast = forecast_values)
          write.csv(forecast_df, file, row.names = FALSE)
        }
      }
    }
    
  )
}
