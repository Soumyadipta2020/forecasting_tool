rm(list=ls())

#### Library ####
library(shiny)
library(plotly)
library(forecast)
library(rugarch)
library(reticulate)
library(h2o)
library(tensorflow)
library(keras)
library(shinythemes)
library(dplyr)
library(randomForest)
library(rpart)
library(glmnet)
library(Matrix)
library(shinydashboard)
library(dashboardthemes)



app <- shinyApp(ui, server)

# app$staticPaths <- list(`/` = structure(list(
#   path = file.path(getwd(), "www"),
#   options = structure(list(
#     indexhtml = FALSE, fallthrough = TRUE,
#     html_charset = NULL, headers = NULL, validation = NULL,
#     exclude = FALSE
#   ), class = "staticPathOptions", normalized = TRUE)
# ), class = "staticPath"))
# 
# runApp(app)
