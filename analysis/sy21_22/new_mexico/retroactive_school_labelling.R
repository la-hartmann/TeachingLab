library(readxl)

df <- read_excel("~/Downloads/TIDs without schools 2022.xlsx")

df$tid


check <- read_rds(here::here("data/sy21_22/new_mexico_data/stored_ids.rds")) |>
  mutate(id = str_replace_all(id, "administrator", "admin"))

schools_sheet <- read_sheet("https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=0")

df |>
  left_join(check, by = c("tid" = "id")) |>
  left_join(schools_sheet |> select(`Participant Name`, `School Name`, `District`), by = c("name" = "Participant Name")) |>
  select(-`Work email`, -name) |>
  sheet_write()

view(check)
