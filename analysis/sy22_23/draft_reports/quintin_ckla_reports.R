library(qualtRics)
library(TeachingLab)
library(tidyverse)

participant_feedback <- fetch_survey(surveyID = "SV_djt8w6zgigaNq0C", 
                                     verbose = TRUE,
                                     force_request = TRUE)

ckla_feedback <- participant_feedback |>
  filter(str_detect(Course, "CKLA") & Finished == TRUE)

ckla_feedback$last_session_or_not |>
  table()

partners <- "All Partners"

rmarkdown::render(params = list(partner = partners),
                  input = here::here("analysis/sy22_23/Reports/provide_data_pdf_report_session.Rmd"),
                  output_dir = here::here("analysis/sy22_23/Reports/reports_generated"))

rmarkdown::render(params = list(partner = partners),
                  input = here::here("analysis/sy22_23/Reports/provide_data_pdf_course.Rmd"),
                  output_dir = here::here("analysis/sy22_23/Reports/reports_generated"))
