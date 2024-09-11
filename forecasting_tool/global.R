# remove warning
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
library(fontawesome)

source("helper.R")
source("theme.R")
source("api.R")
source("functions.R")
source("mongodb_helper.R")

# js scroll code ####
jscode_1 <- '
      var container = document.getElementById("chat-history");
      if (container) {
        container.scrollTop = container.scrollHeight;
      }
    '

#### data edit ####
data_edit <<- data.frame(row = NA, col = NA, value = NA)

