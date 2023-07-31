library(shinyjs)
library(shiny)
library(bs4Dash)
library(tidyverse)
library(rmarkdown)
library(shinycssloaders)
library(fresh)
library(shinyTime)
library(shinyWidgets)
library(googlesheets4)
library(googledrive)
library(here)
library(lubridate)
library(waiter)
library(stringi)
library(hover)

# Immediately set authorization for google drive
drive_auth(path = "data/thermal-cathode-310719-1445194b99c7.json")
gs4_auth(token = drive_token()) # REMEMBER YOU JUST CHANGED THIS

# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# which fields are mandatory
fieldsMandatory <- c("pm", "curriculum", "site", "content", "calls_count")



# invalidate_function <- function() {
#   validate(
#     need(input$time_1 != input$time_2, session$sendCustomMessage(type = 'errormessage',
#                                                                  message = 'Please enter non-duplicate times'))
#   )
#   req(input$time_1 != input$time_2)
# }





