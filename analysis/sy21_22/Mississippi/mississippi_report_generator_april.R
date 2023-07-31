library(tidyverse)
library(rmarkdown)
library(googlesheets4)
library(fidelius)
devtools::load_all()
library(TeachingLab)

## Update the data sources before running ##
get_coaching_feedback(update = TRUE)
get_ipg_forms(update = TRUE)
get_lesson_analysis(update = TRUE)

## Get right google auth ##
googlesheets4::gs4_auth()

## Get List of Teachers and Which Surveys they have filled out ##
## This sheet is updated from "data_scripts/mississippi_sheet_tracker" ##
# actual_teacher_names <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1RwTgvE_vp7dqxbRSAXXPiN_t3AVfVtN0fNoGDz49HlY/edit#gid=0",
#                                                   sheet = 1
# )

## Filter down teachers for fewer reports ##
# teachers <- actual_teacher_names |>
#   filter(`Classroom obs` == TRUE) |>
#   pull(Name) |>
#   sort() |>
#   prepend("All")
teachers <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1eAUdWEzc0R7WzpztOvutRp2FYDtpks37LSFV8sq8ezg/edit#gid=739665772",
                                      sheet = "Mississippi Reports April") |>
  pull(Teacher)

## Get vector of passwords for form generation ##
passwords <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1eAUdWEzc0R7WzpztOvutRp2FYDtpks37LSFV8sq8ezg/edit#gid=739665772",
                                       sheet = "Mississippi Reports April") |>
  pull(Key)

## Render list of reports into folder ##
purrr::walk(teachers[1], ~ rmarkdown::render(
  input = here::here("analysis/sy21_22/Mississippi/mississippi_report_april.Rmd"),
  output_file = paste0("april_report_", str_replace_all(tolower(.x), " ", "_")),
  output_dir = here::here("analysis/sy21_22/Mississippi/Reports/AprilReports"),
  params = list(teacher = .x)
))

files_gen <- list.files(here::here("analysis/sy21_22/Mississippi/Reports/AprilReports"),
                        full.names = T
)

# files_gen <- c(files_gen[-2], files_gen[2])

purrr::walk2(files_gen[1], passwords[1], ~ fidelius::charm(
  input = .x,
  password = .y,
  hint = "Check the google spreadsheet!",
  style = fidelius::stylize(font_family = "Calibri",
                            title_color = "#04abeb", btn_bg_color = "#04abeb", modal_title_color = "#04abeb",
                            btn_hover_color = "#43464d"),
  bundle = F
))

walk(files_gen[1], 
     ~ file.copy(from = .x, to = "~/Teaching Lab/Coding/teachinglab.github.io/Reports/mississippi_reports/April/",
                 overwrite = T))
