### Diagnostic Completion Dashboard ###
library(shiny)
library(bslib)
library(tidyverse)
library(gt)
library(thematic)
library(showtext)
font_add(family = "Calibri", regular = "www/Calibri.ttf")
library(patchwork)
library(glue)
library(shinyWidgets)
library(tlShiny)

# Builds theme object to be supplied to ui
my_theme <- bs_theme(
  bootswatch = "cerulean",
  base_font = font_google("Calibri")
) %>%
  bs_add_rules(sass::sass_file("www/styles.scss"))

# Let thematic know to use the font from bs_lib
thematic_shiny(font = "auto")

### All Partners ###
partners <- readr::read_rds("data/tl_partners.rds")
## Vector of unique partners ##
unique_partners <- partners$`SITES (Educator, End of Session, End of Course, Knowledge Assessments, Attendance sheet)`

### Diagnostic Survey Data ###
diagnostic <- readr::read_rds("data/diagnostic.rds")
### Knowledge Assessments Data ###
knowledge_assessments <- readr::read_rds("data/knowledge_assessments.rds")
### End of Course Survey 2022 ###
course_survey <- readr::read_rds("data/course_survey_21_22.rds")


