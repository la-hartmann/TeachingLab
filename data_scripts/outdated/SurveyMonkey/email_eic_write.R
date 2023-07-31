library(tidyverse)
library(googlesheets4)
library(surveymonkey)

options(sm_oauth_token = Sys.getenv("knowledge_token"))

data <- surveymonkey::fetch_survey_obj(506073626) |>
  surveymonkey::parse_survey()

sheet_read <- read_sheet("https://docs.google.com/spreadsheets/d/1XSw5VeBwFE-ZLAj_kgbvkCTpQfWxziYROWkQeUCqpzM/edit#gid=0")

sheet_write <- data %>%
  select(Email = `To what email should we send your Starbucks gift card?`) |>
  arrange(match(Email, sheet_read$Email))

googlesheets4::range_write(data = sheet_write,
                           col_names = F,
                           ss = "https://docs.google.com/spreadsheets/d/1XSw5VeBwFE-ZLAj_kgbvkCTpQfWxziYROWkQeUCqpzM/edit#gid=0",
                           range = glue::glue("A2:A{length(sheet_write$Email) + 1}"))
