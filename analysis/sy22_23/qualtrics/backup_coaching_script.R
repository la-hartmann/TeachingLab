#### Backup script since the coaching automation seems to keep failing ####
library(googlesheets4)
library(qualtRics)
library(tidyverse)
session_survey <- qualtRics::fetch_survey(surveyID = "SV_djt8w6zgigaNq0C", 
                                          verbose = FALSE)

data_to_update <- session_survey |>
  filter(Course == "Coaching" & Finished == TRUE & RecordedDate >= as.Date("2022-10-18")) |>
  # janitor::remove_empty("cols") |>
  select(Coach, 
         Q67_1, Q67_2, Q67_3, Q67_4, Q67_5, 
         coach_additonal_feed, coach_gone_well, been_better_coach,
         Course, Site, Date = RecordedDate, `Content area`)

data_to_update |>
  googlesheets4::range_write(ss = "1fvy2NTZXs3zuIi9BE1XZP_0MZW7_7I_r8lrbjaqbtnU",
                             sheet = "Sheet3",
                             col_names = FALSE,
                             reformat = FALSE,
                             range = glue::glue("A63:M100"))
