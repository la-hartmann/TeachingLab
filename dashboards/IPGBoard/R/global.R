library(shiny)
library(shiny.router)
library(shiny.semantic)
suppressPackageStartupMessages(library(tidyverse))
library(tlShiny)
library(shinycssloaders)
library(lubridate)
library(scales)
library(ggtext)
library(showtext)
font_add(family = "Calibri", regular = "www/Calibri.ttf")
library(gt)
library(glue)
library(shinyWidgets)
library(Cairo)
library(grDevices)
library(shinyjs)

options(spinner.color = "#04ABEB")

# NAs dataframe
na_df <- tlShiny::na_df

ipg_forms <- readr::read_rds("data/ipg_data.rds")

###### ADD FILTER FOR TEACHER NAMES THAT THEY HAVE TO BE AFTER TODAY ######
###### MAKE TEACHER FILTER REACTIVE #######
ipg_plot_select_names <- readr::read_rds("data/ipg_plot_select_names.rds")
ipg_text_select_names <- readr::read_rds("data/ipg_text_select_names.rds")
