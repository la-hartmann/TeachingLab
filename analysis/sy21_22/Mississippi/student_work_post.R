library(surveymonkey)
library(tidyverse)
library(googledrive)
library(httr)
library(RCurl)

student_work_samples <- surveymonkey::fetch_survey_obj(311633359) |>
  surveymonkey::parse_survey()

student_work_samples_final <- student_work_samples |>
  group_by(respondent_id) |>
  summarise_all(coalesce_by_column) |>
  filter(!is.na(`What is the teacher's name for whom you are submitting student work samples?`)) |>
  filter(date_created >= as.Date("2022-05-01")) |>
  mutate(grade = coalesce(`Please select the grade level of the student work being submitted. - 3`,
                          `Please select the grade level of the student work being submitted. - 4`,
                          `Please select the grade level of the student work being submitted. - 5`,
                          `Please select the grade level of the student work being submitted. - 6`,
                          `Please select the grade level of the student work being submitted. - 7`,
                          `Please select the grade level of the student work being submitted. - 8`,
                          `Please select the grade level of the student work being submitted. - 9`,
                          `Please select the grade level of the student work being submitted. - 10`,
                          `Please select the grade level of the student work being submitted. - 11`,
                          `Please select the grade level of the student work being submitted. - 12`,
                          `Please select the grade level of the student work being submitted. - 3_2`,
                          `Please select the grade level of the student work being submitted. - 4_2`,
                          `Please select the grade level of the student work being submitted. - 5_2`,
                          `Please select the grade level of the student work being submitted. - 6_2`,
                          `Please select the grade level of the student work being submitted. - 7_2`,
                          `Please select the grade level of the student work being submitted. - 8_2`,
                          `Please select the grade level of the student work being submitted. - 9_2`,
                          `Please select the grade level of the student work being submitted. - 10_2`)) |>
  select(`What is the teacher's name for whom you are submitting student work samples?`,
         download_url,
         `Please upload a single file of your student work here, without any identifying information._2`,
         grade)

# html <- content(GET('https://www.surveymonkey.com'), "text")
# 
# viewstate <- as.character(sub('.*id="_VIEWSTATE" value="([0-9a-zA-Z+/=]*).*', '\\1', html))
# 
# params <- list(
#   'username' = 'TeachingLabResearch',
#   'password' = 'Research2019',
#   'login' = 'Log In',
#   '_VIEWSTATE' = viewstate
# )
# 
# httr::POST('https://www.surveymonkey.com', body = params)
# 
# html <- download.file(student_work_samples_final$download_url[1],
#                       here::here("Downloads/student_work/test.docx"))

# map2(
#   student_work_samples_final$download_url[1],
#   student_work_samples_final$`What is the teacher's name for whom you are submitting student work samples?`[1],
#   ~ download.file(url = .x, destfile = paste0(here::here("Downloads/student_work_mississippi/Post/"), .y),
#                   method = "libcurl")
# )

# student_work_samples_final$download_url %>%
#   clipr::write_clip()
# datapasta::vector_paste_vertical()

### Note to self to use above workflow (clipr::write_clip to get the actual urls to search in chrome) ###
# file_downloads <- student_work_samples_final$download_url

# teacher_names <- unique(as.character(student_work_samples$`What is the teacher's name for whom you are submitting student work samples?`)) %>%
#   purrr::keep( ~ !is.na(.x))

# map(teacher_names, 
#     ~ googledrive::drive_mkdir(name = .x, path = as_id("1c8CDPrydbnOatvwJW4dG10vSrTJLz6Fx")))

folder_names <- paste0(student_work_samples_final$`What is the teacher's name for whom you are submitting student work samples?`,
                       "_",
                       student_work_samples_final$grade)

map2(folder$id, folder_names, ~ googledrive::drive_mkdir(name = .y, path = .x))

folder_files <- tibble::tibble(fold_names = folder_names,
               files = list.files(here::here("Downloads/student_work_mississippi/Post"), full.names = T))

folder <- googledrive::drive_ls(path = as_id("1c8CDPrydbnOatvwJW4dG10vSrTJLz6Fx")) %>%
  filter(name == "Post")

### BEFORE RUNNING THIS MATCH ALL THE FILES TO THE CORRECT FOLDER SOMEHOW ###
map2(folders$id, files, ~ googledrive::drive_upload(media = .y, 
                                                    path = .x,
                                                    overwrite = T))

folder_names <- tibble::tibble(
  old_name = student_work_samples_final$`What is the teacher's name for whom you are submitting student work samples?`,
  new_name = paste0(student_work_samples_final$`What is the teacher's name for whom you are submitting student work samples?`,
                    "_",
                    student_work_samples_final$grade)
)

folders <- googledrive::drive_ls(path = as_id("1c8CDPrydbnOatvwJW4dG10vSrTJLz6Fx")) %>%
  filter(name != "Mississippi Math Student Work Sample Scoring")

file_rename_in_folder <- folders %>%
  left_join(folder_names, by = c("name" = "old_name"))

map2(file_rename_in_folder$id, 
     file_rename_in_folder$new_name, ~ googledrive::drive_rename(file = .x, name = .y))

