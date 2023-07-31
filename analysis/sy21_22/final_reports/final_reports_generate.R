### Load Libraries ###
# library(fidelius)
library(googlesheets4)
library(pagecryptr)
library(rmarkdown)
library(tidyverse)
devtools::load_all()
library(TeachingLab)

### Get passwords from google sheet ###
googlesheets4::gs4_auth()
passwords <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1eAUdWEzc0R7WzpztOvutRp2FYDtpks37LSFV8sq8ezg/edit#gid=1431261393",
                                       sheet = "2021-2022 Final Reports",
                                       range = "B:B") |>
  pull(Key)
passwords <- passwords[-10]

### Get Sites List ###
# sites <- TeachingLab::get_current_partner_sites(update = FALSE, condense = TRUE) |>
#   purrr::prepend("All Partners")

### UPDATE ALL DATA ###
### REMINDER TO LOOK INTO PULLING DATA FROM GLOBAL ENVIRONMENT FOR PARAMETRIZED REPORTS ###
update_surveys <- function (update) {
  
  TeachingLab::get_session_survey(update = update)
  TeachingLab::get_course_survey(update = update)
  TeachingLab::get_diagnostic_survey(update = update)
  TeachingLab::get_knowledge_assessments(update = update)
  TeachingLab::get_coaching_feedback(update = update)
  TeachingLab::get_ongoing_coaching(update = update)
  TeachingLab::get_ipg_forms(update = update)
  
  return("Surveys Updated")
}

options(sm_oauth_token = Sys.getenv("session_token"))

update_surveys(update = FALSE)

### List to iterate over for rmd creation ###
# params_list <- list(
#   partner = c(sites, sites),
#   matched = c(rep("unmatched", length(sites)), rep("matched", length(sites)))
# )

params_list_final <- TeachingLab::get_course_survey() |> 
  filter(date_created >= as.Date("2021-07-01")) |>
  pull(`Select your site (district, parish, network, or school).`) |> 
  unique() |> 
  sort() |> 
  purrr::prepend("All Partners")

### Generate Reports ###
purrr::walk(params_list_final, ~ TeachingLab:::partner_file_remove(
  partner = .x,
  input = here::here("analysis/sy21_22/final_reports/2021-2022_report.Rmd"),
  output_dir = here::here("analysis/sy21_22/final_reports/reports")
))
### D11 Math Report ###
params_list_final <- c("District 11 Math")
purrr::walk(params_list_final, ~ TeachingLab:::partner_file_remove(
  partner = .x,
  input = here::here("analysis/sy21_22/final_reports/2021-2022_report_D11_Math.Rmd"),
  output_dir = here::here("analysis/sy21_22/final_reports/reports")
))
### Rochester Report ###
# params_list_final <- c("Rochester City School District - District-wide")
# purrr::walk(params_list_final, ~ TeachingLab:::partner_file_remove(
#   partner = .x,
#   input = here::here("analysis/sy21_22/final_reports/2021-2022_report_Rochester.Rmd"),
#   output_dir = here::here("analysis/sy21_22/final_reports/reports")
# ))

### Files location
# files_gen <- list.files(here::here("analysis/sy21_22/final_reports/reports"),
#                         full.names = T
# )
# 
# files_gen <- files_gen[-1] |>
#   append(files_gen[1], after = 10)

files_gen <- list.files(here::here("analysis/sy21_22/final_reports/encrypted_reports"),
                        full.names = T
)

### Password protect ###
# purrr::walk2(files_gen, passwords,
#              ~ pagecryptr::pagecryptr(file = .x,
#                                       password = .y,
#                                       out_file = .x)) |>
#   suppressWarnings()
# pagecryptr(file = "~/Teaching Lab/Coding/teachinglab.github.io/Reports/2022Reports/Final/final_report_math_delaware_department_of_education__de.html",
#            password = "69kdtzer",
#            out_file = "~/Teaching Lab/Coding/teachinglab.github.io/Reports/2022Reports/Final/final_report_math_delaware_department_of_education__de.html")
# purrr::walk2(files_gen, passwords, ~ fidelius::charm(
#   input = .x,
#   password = .y,
#   hint = "Check the google spreadsheet!",
#   style = fidelius::stylize(font_family = "Calibri",
#                             title_color = "#04abeb", btn_bg_color = "#04abeb", modal_title_color = "#04abeb",
#                             btn_hover_color = "#43464d"),
#   bundle = F
# ))

### Output to website folder Teaching Lab ###
purrr::walk(files_gen, 
     ~ file.copy(from = .x, to = "~/Teaching Lab/Coding/teachinglab.github.io/Reports/2022Reports/Final/",
                 overwrite = T))


