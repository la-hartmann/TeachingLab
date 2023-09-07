library(googlesheets4)
library(googledrive)

#### D11 STUFF ####
eic_crosswalk_d11 <- read_sheet("https://docs.google.com/spreadsheets/d/1R4_8UjgVCaxcFXFXeEpJJL_gFohqqNJTfgokn-ZO0Ik/edit#gid=1984402811",
  sheet = "D11"
)

teacher_educator_ids_d11 <- eic_crosswalk_d11 |>
  pull(`Teacher code in Educator survey`) |>
  na.omit() |>
  tolower()

teacher_student_ids_d11 <- eic_crosswalk_d11 |>
  pull(`Teacher code(s) in Ss survey`) |>
  na.omit() |>
  tolower()

googledrive::drive_download(
  file = as_id("1U0ekJy1QkHGrJW95kxIUyR-BEQ7pZI2e"),
  path = here::here("data/csv_save/Follow Up Educator Survey- EIC SY2021-22.csv")
)
googledrive::drive_download(
  file = as_id("1uFwZGzr-7Yt3U0OUTg7i4nWGk1NCT-BB"),
  path = here::here("data/csv_save/EIC Student Survey SY21-22.csv")
)

eic_educator_survey <- readr::read_csv(here::here("data/csv_save/Follow Up Educator Survey- EIC SY2021-22.csv"))
eic_educator_survey |>
  mutate(id = str_replace_all(
    tolower(paste0(
      `Please write in your 3 initials. If you do not have a middle initial, please write X.(This is used to link the diagnostic and follow-up surveys, but is kept confidential.)`,
      `Please write in your four-digit birthday (MMDD).(This is used to link the diagnostic and follow-up surveys, but is kept confidential.)`
    )),
    "o",
    "0"
  )) |>
  # filter(id %in% teacher_educator_ids_d11) |>
  filter(id == "aje0610") |>
  googlesheets4::write_sheet()

eic_student_survey <- readr::read_csv(here::here("data/csv_save/EIC Student Survey SY21-22.csv"))

eic_student_survey |>
  mutate(id = str_replace_all(
    tolower(`Please ask your teacher for their teacher code.`),
    "o",
    "0"
  )) |>
  googlesheets4::write_sheet()

#### ROCHESTER STUFF ####
eic_crosswalk_rochester <- read_sheet("https://docs.google.com/spreadsheets/d/1R4_8UjgVCaxcFXFXeEpJJL_gFohqqNJTfgokn-ZO0Ik/edit#gid=1984402811",
  sheet = "Rochester"
)

teacher_educator_ids_rochester <- eic_crosswalk_rochester |>
  pull(`Teacher code in Educator survey`) |>
  na.omit() |>
  tolower()

teacher_student_ids_rochester <- eic_crosswalk_rochester |>
  pull(`Teacher code in Ss survey`) |>
  na.omit() |>
  tolower()

eic_educator_survey |>
  mutate(id = tolower(paste0(
    `Please write in your 3 initials. If you do not have a middle initial, please write X.(This is used to link the diagnostic and follow-up surveys, but is kept confidential.)`,
    `Please write in your four-digit birthday (MMDD).(This is used to link the diagnostic and follow-up surveys, but is kept confidential.)`
  ))) |>
  # filter(id %in% teacher_educator_ids_rochester) |>
  filter(id == "rjf0202") |>
  googlesheets4::write_sheet()
