library(tidyverse)
library(surveymonkey)
library(TeachingLab)
library(googlesheets4)
library(rcanvas)
library(rdrop2) # Token should be saved
library(tuber)
token <- readr::read_rds(here::here("tokens/dropbox_token.rds"))

## Pull all New Mexico surveys and deidentify
nm_survey <- surveymonkey::fetch_survey_obj(315553653) %>%
  surveymonkey::parse_survey() %>%
  dplyr::group_by(respondent_id) %>%
  dplyr::summarise_all(TeachingLab::coalesce_by_column) %>%
  dplyr::mutate(`Work email` = stringr::str_to_lower(`Work email`),
                `Work email` = stringr::str_replace_all(`Work email`, c("mallory.gee@clovis-school.org" = "mallory.gee@clovis-schools.org",
                                                                        "jamie.lopez@ratonschools.com" = "jamie.hephner@ratonschools.com")))

deidentified_data <- nm_survey %>%
  dplyr::mutate(id = case_when(str_detect(`Which of the following best describes your primary role? <br><br>`, "Teacher") ~ 
                                 paste0("teacher_", cumsum(replace_na(str_detect(`Which of the following best describes your primary role? <br><br>`, "Teacher") == T, 0))),
                               str_detect(`Which of the following best describes your primary role? <br><br>`, "Administrator") ~ 
                                 paste0("administrator_", cumsum(replace_na(str_detect(`Which of the following best describes your primary role? <br><br>`, "Administrator") == T, 0))),
                               str_detect(`Which of the following best describes your primary role? <br><br>`, "Coach") ~ 
                                 paste0("coach_", cumsum(replace_na(str_detect(`Which of the following best describes your primary role? <br><br>`, "Coach") == T, 0))),
                               T ~ "unclear")) %>%
  select(-c(1:9))

## Get a list of names, emails, and role
names_and_emails <- read_sheet("https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=0",
                               sheet = 1,
                               range = "A:C"
) %>%
  mutate(`Email` = stringr::str_to_lower(Email))

## Store just ids and emails
stored_ids <- deidentified_data %>%
  dplyr::select(id, `Work email`) %>%
  left_join(names_and_emails %>% select(Email, name = `Participant Name`), by = c("Work email" = "Email"))

## Store ids
stored_ids %>%
  readr::write_rds(here::here("data/sy21_22/new_mexico_data/stored_ids.rds"))

set_canvas_domain("https://nmped.instructure.com/")
course_gradebook <- get_course_gradebook(course_id = 1821)

canvas_videos <- course_gradebook %>%
  filter(assignment_name == "Classroom Observation Video Recording" & !is.na(submitted_at)) %>%
  select(user.name, url = preview_url, attachments) %>%
  mutate(download_link = map(row_number(), ~ attachments[[.x]]$url),
         download_link = strsplit(as.character(download_link), ",")) %>%
  unnest() %>%
  mutate(download_link = str_remove_all(download_link, "c\\(\\\"|\\\"| "))

canvas_videos_renamed <- canvas_videos %>%
  mutate(user.name = str_to_title(user.name),
         user.name = case_when(
           str_detect(str_to_lower(user.name), 
                      "bark") == T ~ "Tommy Barksdale",
           str_detect(str_to_lower(user.name), 
                      "badd") == T ~ "Rajasekhar Badditi",
           str_detect(str_to_lower(user.name), 
                      "ong") == T ~ "Rita Kwong",
           str_detect(str_to_lower(user.name), 
                      "antos") == T ~ "Maria Santos",
           str_detect(str_to_lower(user.name), 
                      "tiff") == T ~ "Tiffany Portiz",
           str_detect(str_to_lower(user.name), 
                      "cisn") == T ~ "Maria Dolores Cisneros",
           str_detect(str_to_lower(user.name), 
                      "lop") == T ~ "Jamie Lopez",
           str_detect(str_to_lower(user.name), 
                      "vig") == T ~ "Thomas Vigil",
           str_detect(str_to_lower(user.name), 
                      "cote") == T ~ "Sue Cote",
           str_detect(str_to_lower(user.name), 
                      "tiz") == T ~ "Tiffany Portiz",
           str_detect(str_to_lower(user.name), 
                      "bandl") == T ~ "Raj Bandla",
           str_detect(str_to_lower(user.name), 
                      "aqui") == T ~ "Gilbert Aquino",
           str_detect(str_to_lower(user.name), 
                      "trej") == T ~ "Elisa Trejo",
           str_detect(str_to_lower(user.name), 
                      "marj|marije") == T ~ "Marjie Lyn Pineda",
           str_detect(str_to_lower(user.name), 
                      "alis|alasaisis") == T ~ "Josiah Alisasis",
           TRUE ~ as.character(user.name)
         ),
         video_name = paste0("teacher_", str_replace_all(user.name, teacher_replace_vector))) %>%
  filter(!is.na(readr::parse_number(video_name)))

options(timeout = 100000)
walk2(canvas_videos_renamed$download_link, 1:nrow(canvas_videos_renamed), ~ download.file(url = .x,
                                                                                          destfile = paste0(here::here("videos/video_"), .y, ".mp4")))
## Desktop app Oauth
# yt_oauth(app_id = "831082437753-adff4nsh50kvn2nh71pn8be9gnl5tvjs.apps.googleusercontent.com",
#          app_secret = "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",
#          scope = "upload_and_manage_own_videos")

myapp <- httr::oauth_app("google", 
                         key = "831082437753-adff4nsh50kvn2nh71pn8be9gnl5tvjs.apps.googleusercontent.com", 
                         secret = "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")

scope <- "ssl"

scope_url <- switch(
  scope,
  ssl = "https://www.googleapis.com/auth/youtube.force-ssl",
  basic = "https://www.googleapis.com/auth/youtube",
  own_account_readonly = "https://www.googleapis.com/auth/youtube.readonly",
  upload_and_manage_own_videos ="https://www.googleapis.com/auth/youtube.upload",
  partner_audit = "https://www.googleapis.com/auth/youtubepartner-channel-audit",
  partner =  "https://www.googleapis.com/auth/youtubepartner"
)

google_token <- httr::oauth2.0_token(httr::oauth_endpoints("google"), 
                                     myapp,
                                     scope = scope_url)

options(google_token = google_token)

# yt_search("Barack Obama")


# upload_video(file = here::here("videos/video_1.mp4"),
#              snippet = list(title = "test",
#                             description = "test description"))

canvas_videos_renamed$video_name <- str_replace_all(canvas_videos_renamed$video_name,
                                                    "video_",
                                                    "teacher_")

# map(canvas_videos_renamed$video_name, ~ tuber::upload_video(file = paste0("/Users/dunk/Teaching Lab/Coding/TeachingLab/videos/", str_replace_all(.x, "teacher", "video"), ".mp4"),
#                                                                               snippet = list(
#                                                                                 title = .x)
#                                                                               ))

#### Note to self: The YouTube API rate limits you if you try to upload all of these videos at once,
### the saved dataframe here will allow you to view the "key" of actual names to 
### the "video_number" format

### List of the youtube videos created

videos <- tibble::tibble(number = c(17, 7, 5, 12, 13, 4, 11, 1, 6, 6, 4, 15, 14, 10, 9),
                         video_link = c("https://youtu.be/3S5AMuR1aKA",
                                        "https://youtu.be/KiPBPV08p28",
                                        "https://youtu.be/TolimJwWjMo",
                                        "https://youtu.be/hN2ND8tVZNA", 
                                        "https://youtu.be/OMhUzWQP4OQ",
                                        "https://youtu.be/ZIGMT6hrbdM",
                                        "https://youtu.be/v7L2_AYJkUg",
                                        "https://youtu.be/_HP1BSx5WJA",
                                        "https://youtu.be/v-KLlMXrzFQ",
                                        "https://youtu.be/PQBrp4W8ZrI",
                                        "https://youtu.be/Kf6SvnHYQ-U",
                                        "https://youtu.be/dbrYMJzSfAU",
                                        "https://youtu.be/dbrYMJzSfAU",
                                        "https://youtu.be/z55SNXGMA44",
                                        "https://youtu.be/1nFWt1t2BLQ"))

## Deidentify data and save to a csv
deidentified_data %>%
  mutate(number = ifelse(str_detect(id, "teacher"),
                         readr::parse_number(id),
                         NA)) %>%
  left_join(videos) %>%
  select(-`Work email`, -number) %>%
  readr::write_csv(here::here("data/sy21_22/new_mexico_data/new_mexico_21_22_deidentified.csv"))

## Upload to dropbox
drop_upload(file = here::here("data/sy21_22/new_mexico_data/new_mexico_21_22_deidentified.csv"),
            path = "/New Mexico Deidentified Data (2021-2022)")



## New Mexico Student Survey
nm_student_survey <- surveymonkey::fetch_survey_obj(315708746) %>%
  surveymonkey::parse_survey()

## Deidentify teachers with ids
teacher_deidentify <- stored_ids %>%
  dplyr::filter(str_detect(id, "teacher")) %>%
  dplyr::mutate(number = readr::parse_number(id))

## Get a vector of name = number format to copy paste, you have to add commas manually
copy_and_paste <- tibble::tibble(col = paste0('"', teacher_deidentify$name, '"', ' = ', '"', teacher_deidentify$number, '"')) %>%
  pull(col) %>%
  clipr::write_clip()

## Replacement vector
teacher_replace_vector <- c(
  "Rita Kwong" = "1",
  "Jamie Lopez" = "2",
  "Victoria Naranjo" = "3",
  "Tiffany Portiz" = "4",
  "Raj Bandla" = "5",
  "Tommy Barksdale" = "6",
  "Rajasekhar Badditi" = "7",
  "Kenny Duong" = "8",
  "Stacey Evans" = "9",
  "Gilbert Aquino" = "10",
  "Elisa Trejo" = "11",
  "Marjie Lyn Pineda" = "12",
  "Maria Dolores Cisneros" = "13",
  "Maria Santos" = "14",
  "Thomas Vigil" = "15",
  "Colleen Gallegos" = "16",
  "Sue Cote" = "17",
  "Mallory Gee" = "18",
  "Josiah Alisasis" = "19"
)

## Replace all teacher names after ensuring that they are correctly formatted for replacement
nm_student_deidentified <- nm_student_survey %>%
  select(-`What is your student identification number as provided by your school or teacher?`) %>%
  mutate(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.` = case_when(
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "bark") == T ~ "Tommy Barksdale",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "badd") == T ~ "Rajasekhar Badditi",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "ong") == T ~ "Rita Kwong",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "antos") == T ~ "Maria Santos",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "tiff") == T ~ "Tiffany Portiz",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "cisn") == T ~ "Maria Dolores Cisneros",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "lop") == T ~ "Jamie Lopez",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "vig") == T ~ "Thomas Vigil",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "cote") == T ~ "Sue Cote",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "tiz") == T ~ "Tiffany Portiz",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "bandl") == T ~ "Raj Bandla",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "aqui") == T ~ "Gilbert Aquino",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "trej") == T ~ "Elisa Trejo",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "marj|marije") == T ~ "Marjie Lyn Pineda",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "alis|alasaisis") == T ~ "Josiah Alisasis",
    TRUE ~ as.character(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`)
  )) %>%
  mutate(id = paste0("student_", row_number(), "_teacher_", `What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`)) %>%
  mutate(id = str_replace_all(id, teacher_replace_vector)) %>%
  filter(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.` %!in% c("1", "Mrs. Agino", "Daniel Aguilar")) %>%
  drop_na(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`) %>%
  select(-`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`)

## Store deidentified data as a csv
nm_student_deidentified %>%
  write_csv(here::here("data/sy21_22/new_mexico_data/student_new_mexico_21_22_deidentified.csv"))

## Upload to dropbox
drop_upload(file = here::here("data/sy21_22/new_mexico_data/student_new_mexico_21_22_deidentified.csv"),
            path = "/New Mexico Deidentified Data (2021-2022)")

###### PASSWORD: 8giow4ra #######



