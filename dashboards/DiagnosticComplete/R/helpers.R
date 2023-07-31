### Diagnostic Completion Dashboard ###

library(shiny)
library(bslib)
library(tidyverse)
library(surveymonkey)
library(gt)
library(thematic)
library(showtext)
font_add(family = "Calibri", regular = "www/Calibri.ttf")
library(patchwork)
library(glue)
library(shinyWidgets)
library(TeachingLab)
library(bookdown)
library(ggiraph)
library(tidytext)

# Builds theme object to be supplied to ui
my_theme <- bs_theme(
  bootswatch = "cerulean",
  base_font = font_google("Calibri")
) %>%
  bs_add_rules(sass::sass_file("www/styles.scss"))

# Let thematic know to use the font from bs_lib
thematic_shiny(font = "auto")

### Diagnostic Completion Dashboard ###
partner_data <- readr::read_rds("data/diagnostic.rds")

# Some cities have multiple stations but we only want users to see unique cities
unique_partners <- levels(partner_data$your_site_district_parish_network_or_school)

# We start with a random city in the back button and have a random city jump button
get_random_partner <- function(){ sample(unique_partners, 1) }

# Add a nicely styled and centered label above a given input
labeled_input <- function(id, label, input) {
  div(
    id = id,
    span(label, style = "font-size: small;"),
    input
  )
}

# Text grob with color matching current theme
themed_text <- function(text) {
  grid::textGrob(
    text,
    gp = grid::gpar(col = thematic::thematic_get_option("fg"))
  )
}