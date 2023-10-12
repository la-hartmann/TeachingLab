library(tidyverse)
library(jsonlite)
library(reticulate)

### Run python script to get monday json ###
reticulate::source_python(here::here("data_scripts/monday.com/get_all_teacher_names_coaching_log.py"))

### Read in Monday JSON ###
initial_df <- jsonlite::fromJSON(here::here("data/monday/fy24_teachers_coaching_log.json"))

initial_df$data$boards$items_page$items[[1]]$column_values |>
  unlist() -> teachers

ipg_teacher_options <- metadata(surveyID = "SV_0BSnkV9TVXK1hjw", questions = "teacher_select")

final_teacher_choices <- purrr::map_chr(1:length(ipg_teacher_options$questions$QID472$choices), ~ ipg_teacher_options$questions$QID472$choices[[.x]]$choiceText)

### Difference between teachers and teacher choices
setdiff(teachers, final_teacher_choices) |>
  sort() |>
  clipr::write_clip()

ipg_coach_options <- metadata(surveyID = "SV_0BSnkV9TVXK1hjw", questions = "coach")
final_coach_choices <- purrr::map_chr(1:length(ipg_coach_options$questions$QID469$choices), ~ ipg_coach_options$questions$QID469$choices[[.x]]$choiceText)

### Difference between coaches and coach choices
setdiff(fac_board$Facilitator, final_coach_choices) |>
  sort() |>
  clipr::write_clip()
