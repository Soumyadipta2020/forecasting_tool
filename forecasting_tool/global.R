rm(list=ls())

#### Library ####
library(shiny)
library(httr2)
library(purrr)
library(glue)
library(jsonlite)
library(httr)
library(gptstudio)
# install.packages("remotes")
# remotes::install_github("deepanshu88/shinyCopy2clipboard")
library(shinyCopy2clipboard)
# remotes::install_github("mlverse/tft")
# library(tft)
library(prophet)
library(sass)
library(markdown)
library(waiter)
library(shinyjs)
library(dplyr)
library(officer)
library(stringr)
library(openai)
library(plotly)
library(forecast)
library(rugarch)
library(reticulate)
library(h2o)
# library(tensorflow)
# library(keras)
library(shinythemes)
library(randomForest)
library(rpart)
library(glmnet)
library(Matrix)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(fresh)
library(echarts4r)
library(shinymanager)
library(moments)
library(tidyr)
library(shinyWidgets)
library(leaflet)
library(tsfgrnn)

source("helper.R")

# js scroll code ####
jscode_1 <- '
      var container = document.getElementById("chat-history");
      if (container) {
        container.scrollTop = container.scrollHeight;
      }
    '

# API ####
openai_api_key <<- ""
gemini_api_key <<- ""
claude_api_key <<-
  ""
hugging_api_key <<- ""

#### data edit ####
data_edit <<- data.frame(row = NA, col = NA, value = NA)

#### Theme ####
# dashboard_header_theme <- create_theme(
#   theme = "default",
#   adminlte_color(
#     light_blue = "#E7FF6E"
#   )
# )

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
    # default_border = "#112446",
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



#### mode ####
getmode <- function(v, na = TRUE) {
  if(na == TRUE){
    v <- v[!is.na(v)]
  }
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#### Forecasting functions ####
#### LSTM ####
lstm_forecast <- function(ts_data, horizon) {
  library(keras)
  
  # Normalize the data
  normalized_data <- scale(ts_data)
  
  # Split data into train and test sets
  train_data <- normalized_data[1:(length(normalized_data) - horizon)]
  test_data <- normalized_data[(length(normalized_data) - horizon + 1):length(normalized_data)]
  
  # Prepare the training data
  train_x <- train_y <- list()
  for (i in 1:(length(train_data) - horizon)) {
    train_x[[i]] <- matrix(train_data[i:(i + horizon - 1)], nrow = horizon, ncol = 1)
    train_y[[i]] <- train_data[(i + horizon)]
  }
  
  train_x <- array_reshape(train_x, c(length(train_x), horizon, 1))
  train_y <- unlist(train_y)
  
  # Define the LSTM model architecture
  model <- keras_model_sequential()
  model %>%
    layer_lstm(units = 50, input_shape = c(horizon, 1)) %>%
    layer_dense(units = 1)
  
  # Compile the model
  model %>% compile(
    loss = "mean_squared_error",
    optimizer = optimizer_adam()
  )
  
  # Train the model
  model %>% fit(
    train_x, train_y,
    epochs = 100,
    batch_size = 32,
    verbose = 0
  )
  
  # Make predictions for the test set
  test_x <- array_reshape(test_data, dim = c(length(test_data) / horizon, horizon, 1))
  predicted_values <- model %>% predict(test_x)
  
  # Denormalize the predicted values
  denormalized_values <- predicted_values * sd(ts_data) + mean(ts_data)
  
  # Create the forecast object
  forecast_values <- ts(denormalized_values, frequency = 12)
  
  return(forecast_values)
}

#### AutoML ####
automl_forecast <- function(ts_data, horizon) {
  # Convert the time series data to a data frame
  data_df <- data.frame(Date = as.numeric(time(ts_data)), Value = as.numeric(ts_data))
  
  # Initialize the H2O cluster
  h2o.init()
  
  # Convert the data frame to an H2O frame
  h2o_df <- as.h2o(data_df, destination_frame = "ts_data")
  
  # Set the target variable
  target <- "Value"
  
  # Train AutoML model
  aml <- h2o.automl(x = setdiff(colnames(h2o_df), target),
                    y = target,
                    training_frame = h2o_df,
                    max_runtime_secs = 300,
                    max_models = 10)
  
  # Generate predictions for the future horizon
  forecast_df <- data.frame(Date = seq(time(ts_data)[length(ts_data)] + 1/12, by = 1/12, length.out = horizon))
  forecast_h2o <- as.h2o(forecast_df, destination_frame = "forecast_data")
  forecast_predictions <- h2o.predict(aml@leader, forecast_h2o)
  
  # Convert the predictions to a time series object
  forecast_values <- ts(as.vector(forecast_predictions$predict), frequency = 12)
  
  # Shut down the H2O cluster
  h2o.shutdown(prompt = FALSE)
  
  return(forecast_values)
}

#### ARFIMA FOrecast ####
arfima_forecast <- function(x, h){
  {
    arfima_fit <- auto.arima(x, seasonal = FALSE, stepwise = FALSE, 
                             approximation = FALSE, allowdrift = FALSE)
    
    # Extract AR and MA orders from the fitted model
    ar_order <- arimaorder(arfima_fit)[1]  # AR order
    ma_order <- arimaorder(arfima_fit)[3]  # MA order
    
    # Create dynamic ARFIMA specification
    spec <- arfimaspec(mean.model = list(armaOrder = c(ar_order, ma_order),
                                         include.mean = TRUE, arfima = TRUE))
    
    # Fit the model using the dynamic specification
    garch_fit <- arfimafit(spec = spec, data = x)
    
    # Generate forecasts directly using arfimaforecast
    forecast_values <- arfimaforecast(garch_fit, n.ahead = h)
    
    # Extract the forecasted values as a vector
    forecast_vector <- as.vector(forecast_values@forecast$seriesFor)
    return(forecast_vector)
  }
}