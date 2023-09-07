library(googlesheets4)
library(tidyverse)

#### Get Sites from Automation Sheet ####
sites_sheet <- read_sheet("https://docs.google.com/spreadsheets/d/11jlo9UeWxZGwunhDb24hZBwAKc5b8ZKM9AYNWZaUyZY/edit#gid=492986067",
                          sheet = "FY23 Automation (Sites + Courses)")

#### Get Participant Feedback Survey ####
participant_feedback <- qualtRics::fetch_survey(surveyID = "SV_djt8w6zgigaNq0C", 
                                     verbose = TRUE)

fy23_sites <- sites_sheet |>
  pull(`Site in Survey`) |>
  unique() |>
  sort()

fy23_sites |>
  purrr::keep( ~ !str_detect(.x, "D11|D27|D9")) |>
  clipr::write_clip()

fy23_sites |>
  purrr::keep( ~ str_detect(.x, "D11")) |>
  clipr::write_clip()

fy23_sites |>
  purrr::keep( ~ str_detect(.x, "D9")) |>
  clipr::write_clip()

fy23_sites |>
  purrr::keep( ~ str_detect(.x, "D27")) |>
  clipr::write_clip()


unique(participant_feedback$Site)
