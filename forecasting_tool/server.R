# Server
server <- function(input, output, session) {
  # dashboard header title ####
  output$header_ui <- renderUI({
    if (input$sidebarCollapsed) {
      # "FT"
      tags$img(src = "brand_logo.png", height = "35", width = "35")
    } else {
      HTML(paste(
        "<span style='font-size: 16px;'>", "AI Forecasting App", "</span>",
        "<span class='version-badge' style='border-radius: 10px; font-size: small; background-color: #545454;'>",
        "&nbsp; v.0.03.3 &nbsp;", "</span>"
      ))
    }
  })

  # home button ####
  observeEvent(input$home, {
    updateTabItems(session, "sidebar", selected = "Home")
    updateTabsetPanel(session, "tabbox_1", selected = "Data")
  })

  #### Dashboard user window ####
  output$user <- renderUser({
    myDashboardUser(
      name = "Soumyadipta Das",
      image = "self.jpg",
      title = "Consultant II @ EXL",
      subtitle = "Author",
      fluidRow(
        dashboardUserItem(
          width = 6,
          socialButton(
            href = "https://sites.google.com/view/soumyadipta-das",
            icon = icon("id-card")
          )
        ),
        dashboardUserItem(
          width = 6,
          socialButton(
            href = "https://www.linkedin.com/in/soumyadipta-das/",
            icon = icon("linkedin-in", lib = "font-awesome")
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
            icon = icon("square-github", lib = "font-awesome")
          )
        )
      )
    )
  })

  #### File template download ####
  output$file_template_download <- downloadHandler(
    filename = function() {
      paste("Sample data", ".csv", sep = "")
    },
    content = function(file) {
      temp <- read.csv("data/timeseries demo.csv", header = TRUE)
      write.csv(temp, file, row.names = FALSE)
    }
  )
  
  #### Data upload ####
  data_primary <- reactive({
    if (input$data_source == "Upload") {
      req(input$file)
      df <- read.csv(input$file$datapath)

      ##### column type checking #####
      col_type_check <- ((sum(1 * ((as.vector(sapply(df, class))) == "numeric")) +
        sum(1 * ((as.vector(sapply(df, class))) == "integer"))) >= ncol(df) - 1)
      if (!col_type_check) {
        shinyFeedback::hideFeedback("file")
        shinyFeedback::feedbackDanger("file", !col_type_check, "Invalid file format. Please download the template.")
      }
      req(col_type_check)
      shinyFeedback::hideFeedback("file")
      shinyFeedback::feedbackSuccess("file", col_type_check, "Successfully Uploaded.")
    } else if (input$data_source == "Sample") {
      req(input$load_mongo)
      df <- mongo_read(input$sample_data, "timeseries", mongo_url)
    }

    return(df)
  })

  #### Data edit ####
  data_old <- reactive({
    withProgress(message = "Loading....", {
      df <- data_primary()

      output$time_var <- renderUI({
        selectInput("time_variable", "Select Time Variable",
          choices = colnames(df),
          selected = colnames(df)[1]
        )
      })

      # Identify numeric columns in the Dataframe
      numeric_columns <- names(df)[sapply(df, is.numeric)]

      # Initialize datatable
      dt <- datatable(df, editable = TRUE, filter = "top", selection = "none", rownames = FALSE)
      # Apply conditional formatting to each numeric column
      for (col_name in numeric_columns) {
        brks <- seq(min(df[[col_name]], na.rm = TRUE),
          max(df[[col_name]], na.rm = TRUE),
          length.out = 10
        )
        clrs <- colorRampPalette(c("#40a2ed", "#40edae"))(length(brks) + 1)
        dt <- dt %>%
          formatStyle(
            columns = col_name,
            backgroundColor = styleInterval(brks, clrs)
          )
      }

      #### render data table #####
      output$uploaded_data <- renderDT(dt)

      df1 <<- df

      #### data edit step note #####
      observeEvent(input$uploaded_data_cell_edit, {
        info <- input$uploaded_data_cell_edit
        str(info)
        i <- info$row
        j <- info$col + 1
        v <- info$value
        df1[i, j] <<- v

        # Initialize datatable
        dt <- datatable(df1, editable = TRUE, filter = "top", selection = "none", rownames = FALSE)
        # Apply conditional formatting to each numeric column
        for (col_name in numeric_columns) {
          brks <- seq(min(df1[[col_name]], na.rm = TRUE),
            max(df1[[col_name]], na.rm = TRUE),
            length.out = 10
          )
          clrs <- colorRampPalette(c("#40a2ed", "#40edae"))(length(brks) + 1)
          dt <- dt %>%
            formatStyle(
              columns = col_name,
              backgroundColor = styleInterval(brks, clrs)
            )
        }

        #### render data table #####
        output$uploaded_data <- renderDT(dt)

        temp <- data.frame(row = i, col = j, value = v)
        data_edit <<- rbind(data_edit, temp)
      })

      return(df)
    })
  })

  #### Data info ####
  output$info_data <- renderUI({
    req(data_old())
    list(
      valueBox(
        value = nrow(data_old()),
        subtitle = "Observations",
        icon = shiny::icon("tower-observation"),
        color = "yellow",
        width = 3
      ),
      valueBox(
        value = ncol(data_old()),
        subtitle = "Variables",
        icon = shiny::icon("square-root-variable"),
        color = "green",
        width = 3
      ),
      valueBox(
        value = paste(round(sum(is.na(data_old())) / (nrow(data_old()) * ncol(data_old())) * 100, 1), "%"),
        subtitle = "Missing %",
        icon = shiny::icon("percentage"),
        color = "red",
        width = 3
      ),
      valueBox(
        value = length(select_if(data_old(), is.numeric)),
        subtitle = "Numeric Columns",
        icon = shiny::icon("list-ol"),
        color = "orange",
        width = 3
      )
    )
  })

  #### main data ####
  data <- eventReactive(input$upload_data, {
    req(input$time_variable)
    df <- data_old()
    df <- df[order(df[[input$time_variable]]), ]
    req(input$upload_data != 0)
    ##### modification of data as per edit #####
    data_edit_1 <- data_edit %>% tidyr::drop_na()
    if (nrow(data_edit_1) != 0) {
      for (k in 1:nrow(data_edit_1)) {
        df[data_edit_1[k, 1], data_edit_1[k, 2]] <- data_edit_1[k, 3]
      }
    }
    #### Outlier treatment ######
    for (i in 1:ncol(df)) {
      value <- df[, i][df[, i] %in% boxplot.stats(df[, i])$out]
      df[, i][df[, i] %in% value] <- median(df[, i], na.rm = TRUE)
    }

    #### Missing value imputation ######
    for (i in 1:ncol(df)) {
      value <- which(is.na(df[, i]))
      df[, i][value] <- median(df[, i], na.rm = TRUE)
    }

    return(df)
  })

  #### graph y variable ####
  output$response_variable_graph <- renderUI({
    req(data_old())
    selectInput("y_variable_graph", "Select Y Variable",
      choices = colnames(data_old())
    )
  })
  #### graph x variable ####
  output$x_variable_graph <- renderUI({
    req(data_old())
    selectInput("x_variables_graph", "Select X Variables",
      choices = setdiff(colnames(data_old()), input$y_variable_graph)
    )
  })
  #### graph data ####
  data_graph <- reactive({
    withProgress(message = "Loading....", {
      temp <- data_old() %>%
        select(input$x_variables_graph, input$y_variable_graph) %>%
        rename(Var1 = input$x_variables_graph, Var2 = input$y_variable_graph)
      return(temp)
    })
  })
  #### graph ####
  output$vis_data <- renderEcharts4r({
    req(data_graph(), input$x_variables_graph, input$y_variable_graph)
    ##### time series plot #####
    data_graph() %>%
      e_charts(x = Var1, darkMode = TRUE) %>%
      e_line(Var2, color = "#026efa") %>%
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

  #### summary stat vars ####
  output$vars_stat <- renderUI({
    req(data())
    selectInput("vars_stat_selected", "Select Variables",
      choices = colnames(data()), multiple = TRUE, selected = NULL
    )
  })

  #### Summary stat ####
  data_prep_stat <- eventReactive(c(input$vars_stat_selected, input$upload_data), {
    withProgress(message = "Loading....", {
      req(!is.null(data()))
      n <- nrow(data())
      if (is.null(input$vars_stat_selected)) {
        temp <- select_if(data(), is.numeric)
        temp_cols <- colnames(temp)
        temp_sum <- temp %>%
          summarise_all(~ sum(.x, na.rm = TRUE)) %>%
          mutate(type = "Sum")
        temp_mean <- temp %>%
          summarise_all(~ mean(.x, na.rm = TRUE)) %>%
          mutate(type = "Mean")
        temp_obs <- temp %>%
          summarise_all(~ sum(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE)) %>%
          mutate(type = "Observations")
        temp_median <- temp %>%
          summarise_all(~ median(.x, na.rm = TRUE)) %>%
          mutate(type = "Median")
        temp_mode <- temp %>%
          summarise_all(~ getmode(.x, na = TRUE)) %>%
          mutate(type = "Mode")
        temp_sd <- temp %>%
          summarise_all(~ sd(.x, na = TRUE)) %>%
          mutate(type = "Standard Deviation")
        temp_var <- temp %>%
          summarise_all(~ var(.x, na = TRUE)) %>%
          mutate(type = "Variance")
        temp_max <- temp %>%
          summarise_all(~ max(.x, na.rm = TRUE)) %>%
          mutate(type = "Maximum")
        temp_min <- temp %>%
          summarise_all(~ min(.x, na.rm = TRUE)) %>%
          mutate(type = "Minimum")
        temp_skew <- temp %>%
          summarise_all(~ skewness(.x, na.rm = TRUE)) %>%
          mutate(type = "skewness")
        temp_kurt <- temp %>%
          summarise_all(~ kurtosis(.x, na.rm = TRUE)) %>%
          mutate(type = "Kurtosis")
        temp_iqr <- temp %>%
          summarise_all(~ IQR(.x, na.rm = TRUE)) %>%
          mutate(type = "Interquartile Range")
        temp <- temp_sum %>%
          bind_rows(
            temp_obs, temp_mean, temp_median, temp_mode, temp_sd,
            temp_var, temp_max, temp_min, temp_skew, temp_kurt, temp_iqr
          ) %>%
          select(type, all_of(temp_cols))
      } else {
        temp <- data() %>%
          select(input$vars_stat_selected) %>%
          select_if(is.numeric)
        temp_sum <- temp %>%
          summarise_all(~ sum(.x, na.rm = TRUE)) %>%
          mutate(type = "Sum")
        temp_mean <- temp %>%
          summarise_all(~ mean(.x, na.rm = TRUE)) %>%
          mutate(type = "Mean")
        temp_obs <- temp %>%
          summarise_all(~ sum(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE)) %>%
          mutate(type = "Observations")
        temp_median <- temp %>%
          summarise_all(~ median(.x, na.rm = TRUE)) %>%
          mutate(type = "Median")
        temp_mode <- temp %>%
          summarise_all(~ getmode(.x, na = TRUE)) %>%
          mutate(type = "Mode")
        temp_sd <- temp %>%
          summarise_all(~ sd(.x, na = TRUE)) %>%
          mutate(type = "Standard Deviation")
        temp_var <- temp %>%
          summarise_all(~ var(.x, na = TRUE)) %>%
          mutate(type = "Variance")
        temp_max <- temp %>%
          summarise_all(~ max(.x, na.rm = TRUE)) %>%
          mutate(type = "Maximum")
        temp_min <- temp %>%
          summarise_all(~ min(.x, na.rm = TRUE)) %>%
          mutate(type = "Minimum")
        temp_skew <- temp %>%
          summarise_all(~ skewness(.x, na.rm = TRUE)) %>%
          mutate(type = "skewness")
        temp_kurt <- temp %>%
          summarise_all(~ kurtosis(.x, na.rm = TRUE)) %>%
          mutate(type = "Kurtosis")
        temp_iqr <- temp %>%
          summarise_all(~ IQR(.x, na.rm = TRUE)) %>%
          mutate(type = "Interquartile Range")
        temp <- temp_sum %>%
          bind_rows(
            temp_obs, temp_mean, temp_median, temp_mode, temp_sd,
            temp_var, temp_max, temp_min, temp_skew, temp_kurt, temp_iqr
          ) %>%
          select(type, input$vars_stat_selected)
      }
      return(temp)
    })
  })

  output$summary_stat_table <- renderDT(
    data_prep_stat(),
    filter = "top", selection = "none", rownames = FALSE
  )

  #### Summary Stat download ####
  output$summary_stat_download <- downloadHandler(
    filename = function() {
      paste("summary_stat", ".csv", sep = "")
    },
    content = function(file) {
      temp <- data_prep_stat()
      write.csv(temp, file, row.names = FALSE)
    }
  )

  #### sumarry stat visualization ####
  summary_stat_fig <- eventReactive(c(
    input$vars_stat_selected, input$upload_data,
    input$summary_stat_plot_type
  ), {
    withProgress(message = "Loading....", {
      req(!is.null(data()))

      if (is.null(input$vars_stat_selected)) {
        temp <- select_if(data(), is.numeric)
        temp_cols <- colnames(temp)
      } else {
        temp <- data() %>% select(input$vars_stat_selected)
        temp_cols <- colnames(temp)
      }

      nvar <- ncol(temp)

      if (input$summary_stat_plot_type == "Boxplot") {
        fig <- plot_ly(y = temp[, 1], type = "box", quartilemethod = "linear", name = temp_cols[1])
        if (nvar > 1) {
          for (i in 2:nvar) {
            fig <- fig %>% add_trace(y = temp[, i], quartilemethod = "inclusive", name = temp_cols[i])
          }
        }
        fig <- fig %>% layout(title = "Boxplot")
      } else if (input$summary_stat_plot_type == "Violin Plot") {
        temp <- temp %>% pivot_longer(cols = temp_cols, names_to = "Vars", values_to = "values")
        fig <- temp %>%
          plot_ly(
            x = ~Vars,
            y = ~values,
            split = ~Vars,
            type = "violin",
            box = list(
              visible = T
            ),
            meanline = list(
              visible = T
            )
          ) %>%
          layout(title = "Violin Plot")
      } else if (input$summary_stat_plot_type == "Histogram") {
        fig <- plot_ly(alpha = 0.5)
        for (i in 1:nvar) {
          fig <- fig %>% add_histogram(x = temp[, i], name = temp_cols[i])
        }
        fig <- fig %>%
          layout(barmode = "overlay") %>%
          layout(title = "Histogram")
      }
    })


    return(fig)
  })
  output$summary_stat_vis <- renderPlotly(summary_stat_fig())


  #### model y variable ####
  output$response_variable <- renderUI({
    req(data())
    selectInput("response_variable", "Select Response Variable",
      choices = colnames(data())
    )
  })
  #### model x variables ####
  output$x_variables <- renderUI({
    req(data(), input$response_variable, input$data_type, input$upload_data)
    if (input$data_type == "Non-Time Series") {
      selectInput("x_variables", "Select X Variables",
        choices = setdiff(colnames(data()), input$response_variable),
        multiple = TRUE
      )
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
            freq <- 12 # Default seasonal period
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
    withProgress(message = "Loading...", {
      if (input$data_type == "Time Series") {
        req(tsData(), input$model)
        model <- switch(input$model,
          "ARIMA" = auto.arima(tsData()),
          "SARIMA" = auto.arima(tsData(), seasonal = TRUE),
          "ARFIMA" = arfima_forecast(tsData(), input$horizon),
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
          # "LSTM" = lstm_forecast(tsData(), input$horizon),
          "AutoML" = automl_forecast(tsData(), input$horizon),
          "ETS" = forecast::forecast(tsData(), h = input$horizon),
          "GRNN" = grnn_forecasting(tsData(), h = input$horizon),
          "Neural Network" = forecast::nnetar(tsData(), lambda = "auto"),
          "State Space ARIMA" = smooth::auto.ssarima(tsData(),
            h = input$horizon,
            initial = "backcasting",
            ic = "BICc"
          ),
          "Prophet" = prophet(data.frame(ds = c(1:length(tsData())), y = tsData()))
          # uncertainty.samples = 10,
          # mcmc.samples = 10)
        )

        if (!is.null(model)) {
          if (input$model == "Prophet") {
            future <- make_future_dataframe(model, periods = input$horizon)
            forecast_values <- (predict(model, future))$yhat
          } else if (input$model == "GRNN") {
            forecast_values <- c(tsData(), model$prediction)
          } else if (input$model == "Neural Network") {
            forecast_values <- forecast(model, h = input$horizon)
            forecast_values <- c(tsData(), forecast_values$mean)
          } else if (input$model == "State Space ARIMA") {
            forecast_values <- c(model$y, model$forecast)
          } else if (input$model == "ARFIMA") {
            forecast_values <- c(tsData(), model)
          } else {
            forecast_values <- forecast(model, h = input$horizon)
            forecast_values <- c(fitted(model), forecast_values$mean)
          }
        }
      }

      if (input$data_type == "Non-Time Series") {
        req(!is.null(input$response_variable), input$x_variables, input$model1)

        model <- switch(input$model1,
          "Linear Regression" = {
            model_fit <- lm(
              data = data(),
              formula = as.formula(paste(input$response_variable, "~", paste(input$x_variables, collapse = " + ")))
            )
            model_fit
          },
          "GLM" = {
            model_fit <- glm(
              data = data(),
              formula = as.formula(paste(input$response_variable, "~", paste(input$x_variables, collapse = " + "))),
              family = gaussian
            )
            model_fit
          },
          "Logistic Regression" = {
            model_fit <- glm(
              data = data(),
              formula = as.formula(paste(input$response_variable, "~", paste(input$x_variables, collapse = " + "))),
              family = binomial
            )
            model_fit
          },
          "LASSO" = {
            cv_model <- cv.glmnet(
              x = data.matrix(data()[input$x_variables]),
              y = data()[input$response_variable][, 1],
              alpha = 1
            )
            best_lambda <- cv_model$lambda.min
            model_fit <- glmnet(
              x = as.matrix(data()[input$x_variables]),
              y = data()[input$response_variable][, 1],
              alpha = 1, lambda = best_lambda
            )
            model_fit
          },
          "Ridge Regression" = {
            cv_model <- cv.glmnet(
              x = data.matrix(data()[input$x_variables]),
              y = data()[input$response_variable][, 1],
              alpha = 0
            )
            best_lambda <- cv_model$lambda.min
            model_fit <- glmnet(
              x = as.matrix(data()[input$x_variables]),
              y = data()[input$response_variable][, 1],
              alpha = 0, lambda = best_lambda
            )
            model_fit
          }
        )

        if (!is.null(model)) {
          # Check if forecast function exists
          if (input$model1 %in% c("Linear Regression", "GLM", "Logistic Regression")) {
            forecast_values <- predict(model, newdata = data.frame(data()[input$x_variables]))
            forecast_values <- as.numeric(forecast_values)
          } else if (input$model1 %in% c("Classification", "LASSO", "Ridge Regression")) {
            # For non-time series data, use the fitted model to get forecasted values
            forecast_values <- predict(model, newx = as.matrix(data()[input$x_variables]))
            print(forecast_values)
            forecast_values <- as.numeric(forecast_values)
          }
        } else {
          NULL
        }
      }

      return(list(Forecast = forecast_values, Model = model))
    })
  })
  #### forecast ####
  observeEvent(input$forecast, {
    forecastData()
  })

  #### render forecast graph ####
  output$plot <- renderPlotly({
    withProgress(message = "Updating", {
      req(data(), input$response_variable, input$forecast, forecastData())

      if (input$data_type == "Time Series") {
        plot_data <- data.frame(
          x = time(tsData()),
          y = as.numeric(tsData()),
          type = "scatter",
          mode = "lines",
          name = "Data"
        )
      }

      if (input$data_type == "Non-Time Series") {
        plot_data <- data.frame(
          x = dplyr::row_number(data()),
          y = data()[input$response_variable][, 1],
          type = "scatter",
          mode = "lines",
          name = "Data"
        )
      }


      if (!is.null(forecastData())) {
        if (input$data_type == "Time Series") {
          forecast_values <- forecastData()$Forecast
          forecast_length <- input$horizon
          forecast_dates <- seq(time(tsData())[length(tsData())] + 1 / 12, by = 1 / 12, length.out = forecast_length)

          forecast_plot_data <- data.frame(
            x = c(time(tsData()), forecast_dates),
            y = forecast_values,
            type = "scatter",
            mode = "lines",
            name = "Forecast"
          )
        }
        if (input$data_type == "Non-Time Series") {
          forecast_values <- forecastData()$Forecast

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
  })

  #### download forecast ####
  output$download <- downloadHandler(
    filename = function() {
      paste0("forecast_", if_else(input$data_type == "Time Series", input$model, input$model1), ".csv")
    },
    content = function(file) {
      if (!is.null(forecastData())) {
        if (input$data_type == "Time Series") {
          forecast_values <- forecastData()$Forecast
          forecast_dates <- seq(1, by = 1, length.out = length(forecast_values))
          forecast_df <- data.frame(
            Date = forecast_dates,
            Actuals = c(tsData(), rep(NA, times = input$horizon)),
            Forecast = forecast_values
          )
          write.csv(forecast_df, file, row.names = FALSE)
        }

        if (input$data_type == "Non-Time Series") {
          forecast_values <- forecastData()$Forecast
          forecast_seq <- seq(1, by = 1, length.out = length(forecast_values))
          forecast_df <- data.frame(
            Sequence = forecast_seq,
            Actuals = c(data()[input$response_variable][, 1]),
            Forecast = forecast_values
          )
          write.csv(forecast_df, file, row.names = FALSE)
        }
      }
    }
  )

  #### Display fitted model ####
  output$fitted_model <- renderPrint({
    req(!is.null(forecastData()))
    summary(forecastData()$Model)
  })


  #### Model Accuracy ####
  output$model_accuracy <- renderUI({
    req(!is.null(forecastData()))
    req(input$forecast != 0)
    y <- data()[input$response_variable][, 1]
    len_y <- length(y)
    forecast_values <- forecastData()$Forecast[1:len_y]

    list(
      valueBox(
        value = paste(round(MLmetrics::MAPE(forecast_values, y) * 100, 1), "%", sep = " "),
        subtitle = "MAPE",
        icon = shiny::icon("star-half-stroke"),
        color = ifelse(MLmetrics::MAPE(forecast_values, y) < 0.1, "green",
          ifelse(MLmetrics::MAPE(forecast_values, y) >= 0.1 &
            MLmetrics::MAPE(forecast_values, y) < 0.2, "yellow",
          "red"
          )
        ),
        width = 4
      ),
      valueBox(
        value = paste(round(MLmetrics::RMSPE(forecast_values, y) * 100, 1), "%", sep = " "),
        subtitle = "RMSPE",
        icon = shiny::icon("square-root-variable"),
        color = ifelse(MLmetrics::RMSPE(forecast_values, y) < 0.1, "green",
          ifelse(MLmetrics::RMSPE(forecast_values, y) >= 0.1 &
            MLmetrics::RMSPE(forecast_values, y) < 0.2, "yellow",
          "red"
          )
        ),
        width = 4
      ),
      valueBox(
        value = paste(round(MLmetrics::R2_Score(forecast_values, y) * 100, 1), "%", sep = " "),
        subtitle = "R2 Score",
        icon = shiny::icon("window-restore"),
        color = ifelse(MLmetrics::R2_Score(forecast_values, y) > 0.85, "green",
          ifelse(MLmetrics::R2_Score(forecast_values, y) <= 0.85 &
            MLmetrics::R2_Score(forecast_values, y) > 0.7, "yellow",
          "red"
          )
        ),
        width = 4
      )
    )
  })

  # Chat ####
  rv <- reactiveValues()
  rv$chat_history <- NULL

  observeEvent(input$chat, {
    req(input$prompt != "")

    ##### uploaded file validation #####
    if (!is.null(input$file_chat$datapath)) {
      file_type <- str_sub(input$file_chat$datapath, start = -4)
      supported_type <- c(".docx", ".pptx")
      match <- sum(str_detect(supported_type, file_type))

      modal_test <- supported_type[1]
      for (i in 2:length(supported_type)) {
        modal_test <- paste(modal_test, supported_type[i], sep = ", ")
      }

      if (match == 0) {
        showModal(modalDialog(
          title = "Uploaded file type error:",
          paste(
            "The uploaded file type should be with extensions:",
            modal_test
          ),
          footer = tagList(tagList(modalButton("close")))
        ))
        reset("file_chat")
        reset("prompt")
      }

      req(match > 0)
    }

    ##### Spinner #####
    w <- Waiter$new(
      id = "chat-history",
      html = spin_3(),
      color = transparent(.5)
    )
    w$show()

    prompt <- input$prompt

    ##### read uploaded files #####
    if (!is.null(input$file_chat$datapath)) {
      if (str_detect(input$file_chat$datapath, ".docx") == TRUE) {
        sample_data <- read_docx(input$file_chat$datapath)
        content <- docx_summary(sample_data)
        temp_text <- paste(content$text, collapse = " ")
        prompt <- paste(prompt, temp_text, sep = "  -  ")
      } else if (str_detect(input$file_chat$datapath, ".pptx") == TRUE) {
        sample_data <- read_pptx(input$file_chat$datapath)
        content <- pptx_summary(sample_data)
        temp_text <- paste(content$text, collapse = " ")
        prompt <- paste(prompt, temp_text, sep = "  -  ")
      }
    } else if (!is.null(input$file$datapath)) {
      # a <- data_old()
      # col_len <- length(colnames(a))
      # temp <- colnames(a)[1]
      # if(col_len > 1){
      #   for(i in 2:col_len){
      #     temp <- paste(temp, colnames(a)[i], sep = ",")
      #   }
      # }
      # temp <- paste0(temp,"\n")
      # for(i in 1:nrow(a)){
      #   for(j in 1:col_len){
      #     temp <- paste0(temp, a[i,j], sep = ",")
      #   }
      #   temp <- paste0(temp,"\n")
      # }
      #
      # prompt <- paste(prompt, temp, sep = "  -  ")
    }

    ##### connecting with LLM's #####
    if (input$model_gen == "gpt-3.5-turbo") {
      response <- chat(
        prompt,
        history = rv$chat_history,
        system_prompt = "general",
        api_key = openai_api_key,
        temp = input$temperature
      )
    } else if (input$model_gen == "gemini-pro") {
      response <-
        gemini(
          prompt,
          temperature = input$temperature,
          api_key = gemini_api_key,
          max_retries = 10
        )
    } else if (input$model_gen == "claude-2.1") {
      response <-
        create_completion_anthropic(
          prompt,
          key = claude_api_key,
          model = "claude-2.1",
          # history = rv$chat_history,
        )
    } else if (input$model_gen == "claude-instant") {
      response <-
        create_completion_anthropic(
          prompt,
          key = claude_api_key,
          model = "claude-instant-1.2",
          # history = rv$chat_history,
        )
    } else if (input$model_gen == "google-gemma-7b-it") {
      response <-
        create_completion_huggingface(
          model = "google/gemma-1.1-7b-it",
          history = rv$chat_history,
          prompt = prompt,
          token = hugging_api_key
        )[[1]][[1]]
    } else if (input$model_gen == "Mixtral-v0.1") {
      response <-
        create_completion_huggingface(
          model = "mistralai/Mixtral-8x7B-Instruct-v0.1",
          history = rv$chat_history,
          prompt = prompt,
          token = hugging_api_key,
          max_new_tokens = 10000
        )[[1]][[1]]
    } else if (input$model_gen == "Mistral-v0.3") {
      response <-
        create_completion_huggingface(
          model = "mistralai/Mistral-7B-Instruct-v0.3",
          history = rv$chat_history,
          prompt = prompt,
          token = hugging_api_key,
          max_new_tokens = 10000
        )[[1]][[1]]
    } else if (input$model_gen == "Meta-Llama-3.2") {
      response <-
        create_completion_huggingface(
          model = "meta-llama/Llama-3.2-3B-Instruct",
          history = rv$chat_history,
          prompt = prompt,
          token = hugging_api_key,
          max_new_tokens = 1000
        )[[1]][[1]]
    } else if (input$model_gen == "Phi-3.5-mini") {
      response <-
        create_completion_huggingface(
          model = "microsoft/Phi-3.5-mini-instruct",
          history = rv$chat_history,
          prompt = prompt,
          token = hugging_api_key,
          max_new_tokens = 1000
        )[[1]][[1]]
    } else if (input$model_gen == "Yi-1.5") {
      response <-
        create_completion_huggingface(
          model = "01-ai/Yi-1.5-34B-Chat",
          history = rv$chat_history,
          prompt = prompt,
          token = hugging_api_key,
          max_new_tokens = 1000
        )[[1]][[1]]
    } else if (input$model_gen == "HuggingFaceTB") {
      response <-
        create_completion_huggingface(
          model = "HuggingFaceTB/SmolLM2-1.7B-Instruct",
          history = rv$chat_history,
          prompt = prompt,
          token = hugging_api_key,
          max_new_tokens = 1000
        )[[1]][[1]]
    }


    response <- gsub(" \nAssistant:\n", "", response)

    ##### update history & render #####
    rv$chat_history <-
      update_history(rv$chat_history, input$prompt, response)

    output$chat_history <-
      renderUI(map(rv$chat_history, \(x) markdown(
        glue("<h3>{x$role}:</h3> \n\n{x$content}")
      )))

    w$hide()

    ##### modal for completion #####
    showModal(modalDialog(
      title = "",
      "Generation complete!",
      footer = tagList(actionButton("close_win", "Close"))
    ))
    Sys.sleep(1)
    click("close_win")
  })

  #### Update page after completion ####
  observeEvent(input$close_win, {
    removeModal()
    shinyjs::runjs(jscode_1)
    reset("prompt")

    ##### reset uploaded file #####
    reset("file_chat")
  })
  #### chat history ####
  observe({
    req(input$clipbtn)
    temp <- rv$chat_history
    final <- data.frame()
    for (i in 1:length(temp)) {
      user_out <-
        data.frame(output = paste0(print(temp[i][[1]]$role), "\n"))
      content_out <-
        data.frame(output = paste0(print(temp[i][[1]]$content), "\n"))
      final <- final %>% bind_rows(user_out, content_out)
    }

    #### copy button ####
    CopyButtonUpdate(
      session,
      id = "clipbtn",
      label = "Copy",
      icon = icon("clipboard"),
      text = as.character(final$output)
    )
  })


  #### Clear history ####
  observeEvent(input$remove_chatThread, {
    output$chat_history <- renderUI({
      return(NULL)
    })
    rv$chat_history <- NULL
    updateTextInput(session, "prompt", value = "")
  })

}
