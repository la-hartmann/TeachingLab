library(tidyverse)
library(rmarkdown)
library(fidelius)
library(googlesheets4)

partners_d27 <- c("All District 27 Sites",
                  "NYC District 27 - PS 104, NY",
                  "NYC District 27 - PS/MS 183, NY",
                  "NYC District 27 - PS 306, NY",
                  "NYC District 27 - MS 210, NY")

googlesheets4::gs4_auth()

passwords <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1eAUdWEzc0R7WzpztOvutRp2FYDtpks37LSFV8sq8ezg/edit#gid=1435973886",
                                       sheet = "ANA Passwords") |>
  pull(Key)

## Render list of reports into folder ##
purrr::walk(partners_d27[2:5], ~ rmarkdown::render(
  input = here::here("analysis/sy21_22/ANAs/ANA_template.Rmd"),
  output_file = paste0("may_report_", str_replace_all(tolower(.x), c(" " = "_",
                                                                     "-" = "",
                                                                     "," = ""))),
  output_dir = here::here("analysis/sy21_22/ANAs/Reports"),
  params = list(site = .x)
))

files_gen <- list.files(here::here("analysis/sy21_22/ANAs/Reports"),
                        full.names = T
) |>
  purrr::keep( ~ stringr::str_detect(.x, "\\.html"))

purrr::walk2(files_gen[2:5], passwords[2:5], ~ fidelius::charm(
  input = .x,
  password = .y,
  hint = "Check the google spreadsheet!",
  style = fidelius::stylize(font_family = "Calibri",
                            title_color = "#04abeb", btn_bg_color = "#04abeb", modal_title_color = "#04abeb",
                            btn_hover_color = "#43464d"),
  bundle = F
))

walk(files_gen[1:5], 
     ~ file.copy(from = .x, to = "~/Teaching Lab/Coding/teachinglab.github.io/Reports/ana_reports/",
                 overwrite = T))

