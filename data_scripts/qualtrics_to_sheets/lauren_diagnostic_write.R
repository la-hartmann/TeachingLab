library(googlesheets4)
library(tidyverse)

followup_survey <- TeachingLab::get_followup_educator(year = "22_23", update = TRUE)

#### REMINDER: ADD SPECIFIC SHEET WHEN YOU HAVE INTERNET ####
data_to_write <- followup_survey |>
  dplyr::filter(site == "NY_D11") |> 
  dplyr::select(RecordedDate, Finished, initials, district11)

data_to_write |>
  googlesheets4::range_write(ss = "1ltHzd47A0aaN60JcBgLw2CQgstwzPrRN6yQKGkXY3eA",
                             reformat = F,
                             col_names = T,
                             sheet = "data",
                             range = glue::glue("A1:D{nrow(data_to_write) + 1}"))

