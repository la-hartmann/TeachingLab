library(googlesheets4)
library(tidyverse)

eic_student_survey <- readr::read_csv(here::here("data/csv_save/EIC Student Survey SY21-22.csv")) |>
  slice(-1) |>
  drop_na(`What is the name of your district and school?`) |>
  mutate(`What is the name of your district and school?` = ifelse(`What is the name of your district and school?` == "Other (please specify)",
                                                                  `...13`,
                                                                  `What is the name of your district and school?`)) |>
  mutate(`Please ask your teacher for their teacher code.` = tolower(`Please ask your teacher for their teacher code.`),
         `What is the name of your district and school?` = case_when(`Please ask your teacher for their teacher code.` == "dam1010" ~ "Rochester City School District - School 12",
                                                                     `Please ask your teacher for their teacher code.` == "amw0516" ~ "Rochester City School District - Monroe Lower",
                                                                     `Please ask your teacher for their teacher code.` == "jal0426" ~ "Rochester City School District - North West",
                                                                     T ~ as.character(`What is the name of your district and school?`)),
         d11_rochester = ifelse(str_detect(`What is the name of your district and school?`, "11"),
                                "District 11",
                                "Rochester"))

separated_student_surveys <- eic_student_survey |>
  split(eic_student_survey$d11_rochester)


#### D11 STUFF ####
eic_crosswalk_d11 <- read_sheet("https://docs.google.com/spreadsheets/d/1R4_8UjgVCaxcFXFXeEpJJL_gFohqqNJTfgokn-ZO0Ik/edit#gid=1984402811",
                                sheet = "D11"
)

d11_teacher_ids <- eic_crosswalk_d11 |>
  dplyr::select(TeacherID, `Teacher code(s) in Ss survey`) |>
  tidyr::drop_na(`Teacher code(s) in Ss survey`) |>
  dplyr::mutate(`Teacher code(s) in Ss survey` = tolower(`Teacher code(s) in Ss survey`),
                to = stringr::str_split(`Teacher code(s) in Ss survey`, ", ")) |>
  tidyr::unnest(to) |>
  group_by(TeacherID) |>
  mutate(row = paste0("id", row_number())) |>
  pivot_wider(names_from = row, values_from = to)

separated_student_surveys$`District 11` |>
  left_join(d11_teacher_ids |> select(TeacherID, id1), by = c("Please ask your teacher for their teacher code." = "id1")) |>
  left_join(d11_teacher_ids |> select(TeacherID2 = TeacherID, id2) |> drop_na(id2), by = c("Please ask your teacher for their teacher code." = "id2")) |>
  left_join(d11_teacher_ids |> select(TeacherID3 = TeacherID, id3) |> drop_na(id3), by = c("Please ask your teacher for their teacher code." = "id3")) |>
  left_join(d11_teacher_ids |> select(TeacherID4 = TeacherID, id4) |> drop_na(id4), by = c("Please ask your teacher for their teacher code." = "id4")) |>
  left_join(d11_teacher_ids |> select(TeacherID5 = TeacherID, id5) |> drop_na(id5), by = c("Please ask your teacher for their teacher code." = "id5")) |>
  dplyr::mutate(TeacherID = coalesce(TeacherID, TeacherID2, TeacherID3, TeacherID4, TeacherID5)) |>
  relocate(TeacherID, .before = 10) |>
  select(-c(1:9), -TeacherID2, -TeacherID3, -TeacherID4, -TeacherID5, -`...13`) |>
  drop_na(TeacherID) |>
  googlesheets4::write_sheet()

#### ROCHESTER STUFF ####
eic_crosswalk_rochester <- read_sheet("https://docs.google.com/spreadsheets/d/1R4_8UjgVCaxcFXFXeEpJJL_gFohqqNJTfgokn-ZO0Ik/edit#gid=1984402811",
                                      sheet = "Rochester"
)

rochester_teacher_ids <- eic_crosswalk_rochester |>
  dplyr::select(TeacherID, `Teacher code in Ss survey`) |>
  tidyr::drop_na(`Teacher code in Ss survey`) |>
  dplyr::mutate(`Teacher code in Ss survey` = tolower(`Teacher code in Ss survey`),
                to = stringr::str_split(`Teacher code in Ss survey`, ", ")) |>
  tidyr::unnest(to) |>
  group_by(TeacherID) |>
  mutate(row = paste0("id", row_number())) |>
  pivot_wider(names_from = row, values_from = to)

separated_student_surveys$Rochester |>
  left_join(rochester_teacher_ids |> select(TeacherID, id1), by = c("Please ask your teacher for their teacher code." = "id1")) |>
  left_join(rochester_teacher_ids |> select(TeacherID2 = TeacherID, id2) |> drop_na(id2), by = c("Please ask your teacher for their teacher code." = "id2")) |>
  left_join(rochester_teacher_ids |> select(TeacherID3 = TeacherID, id3) |> drop_na(id3), by = c("Please ask your teacher for their teacher code." = "id3")) |>
  left_join(rochester_teacher_ids |> select(TeacherID4 = TeacherID, id4) |> drop_na(id4), by = c("Please ask your teacher for their teacher code." = "id4")) |>
  left_join(rochester_teacher_ids |> select(TeacherID5 = TeacherID, id5) |> drop_na(id5), by = c("Please ask your teacher for their teacher code." = "id5")) |>
  left_join(rochester_teacher_ids |> select(TeacherID6 = TeacherID, id6) |> drop_na(id6), by = c("Please ask your teacher for their teacher code." = "id6")) |>
  dplyr::mutate(TeacherID = coalesce(TeacherID, TeacherID2, TeacherID3, TeacherID4, TeacherID5, TeacherID6)) |>
  relocate(TeacherID, .before = 10) |>
  select(-c(1:9), -TeacherID2, -TeacherID3, -TeacherID4, -TeacherID5, -TeacherID6, -`...13`) |>
  drop_na(TeacherID) |>
  googlesheets4::write_sheet()

