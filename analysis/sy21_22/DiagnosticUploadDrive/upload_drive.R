# library(tidyverse)
# library(googledrive)
# library(surveymonkey)
# 
# # surveymonkey::browse_surveys() -> surveys
# 
# diagnostic <- surveymonkey::fetch_survey_obj(id = 306944493) %>%
#   surveymonkey::parse_survey()
# 
# diagnostic_with_code <- diagnostic %>%
#   dplyr::mutate(teacher_code = paste0(`Please write in your 3 initials. If you do not have a middle initial, please write X.<br>(This is used to link the diagnostic and follow-up surveys, but is kept confidential.)<br><br>`,
#                               `Please write in your four-digit birthday (MMDD).<br>(This is used to link the diagnostic and follow-up surveys, but is kept confidential.)`)) %>%
#   dplyr::mutate(teacher_code = replace(teacher_code, str_detect(teacher_code, "@"), NA)) %>%
#   dplyr::filter(!is.na(teacher_code) & !str_detect(teacher_code, "Test|test"))
# 
# # List/count of all current ids
# diagnostic_with_code$teacher_code %>%
#   purrr::keep( ~ !is.na(.x)) %>%
#   purrr::keep( ~ !str_detect(.x, "Test|test")) %>%
#   length()
# 
# #### UPLOAD TO DRIVE ####
# 
# drive_folder <- drive_find(type = "folder", pattern = "TL/Assistments")
# drive_folder$id -> drive_id
# 
# diagnostic_with_code %>%
#   write_csv(here::here("data-raw/diagnostic_2021.csv"))
# 
# drive_upload(media = here::here("data-raw/diagnostic_2021.csv"), name = paste0(Sys.Date(), "tl_data.csv"), 
#              path = as_id(drive_id))
