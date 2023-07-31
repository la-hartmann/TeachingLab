library(tidyverse)
library(surveymonkey)
library(TeachingLab)
library(googlesheets4)
library(rcanvas)
library(rdrop2) # Token should be saved
library(tuber)
drop_auth(rdstoken = here::here("tokens/dropbox_token.rds"))

options(sm_oauth_token = Sys.getenv("session_token"))

## Pull all New Mexico surveys ##
nm_survey <- surveymonkey::fetch_survey_obj(315553653) |>
  surveymonkey::parse_survey() |>
  dplyr::group_by(respondent_id) |>
  dplyr::summarise_all(TeachingLab::coalesce_by_column) |>
  dplyr::mutate(
    `Work email` = stringr::str_to_lower(`Work email`),
    `Work email` = stringr::str_replace_all(`Work email`, c(
      "mallory.gee@clovis-school.org" = "mallory.gee@clovis-schools.org",
      "jamie.lopez@ratonschools.com" = "jamie.hephner@ratonschools.com"
    ))
  )

## Get a list of names, emails, and role from google sheet tracker ##
names_and_emails <- read_sheet("https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=0",
  sheet = 1,
  range = "A:C"
) %>%
  mutate(`Email` = stringr::str_to_lower(Email))

## Does something #ISSUE (idk what this does) ##
extra_names_and_emails <- names_and_emails %>%
  filter(str_to_lower(Email) %!in% str_to_lower(nm_survey$`Work email`)) %>%
  rename(`Work email` = Email) %>%
  select(`Work email`) %>%
  drop_na(`Work email`) %>%
  bind_cols(tibble::tibble(`Which of the following best describes your primary role? <br><br>` = c(
    "Teacher",
    "Administrator",
    "Administrator",
    "Administrator",
    "Administrator",
    "Administrator",
    "Teacher",
    "Teacher",
    "Teacher"
  )))

## Replacement vector ##
teacher_replace_vector <- c(
  "Anthony Romero" = "1",
  "P. Amber Mccabe" = "2",
  "Maria Santos" = "3",
  "Nagarajan Bandla" = "4",
  "Rajasekhar Badditi" = "5",
  "Mallory Gee" = "6",
  "Kenny Duong" = "7",
  "Jamie Hephner" = "8",
  "Tommy Barksdale" = "9",
  "Sue Cote" = "10",
  "Colleen Gallegos" = "11",
  "Maria Dolores Cisneros" = "12",
  "Stacey Evans" = "13",
  "Elisa Trejo" = "14",
  "Rita Kwong" = "15",
  "Tiffany Barrion" = "16",
  "Gilbert Aquino" = "17",
  "Victoria Naranjo" = "18",
  "Thomas Vigil" = "19",
  "Marjie Lyn Pineda" = "20",
  "Josiah Alisasis" = "21",
  "George Arguello" = "22",
  "Martha Pena" = "23",
  "Matthew Kelly" = "24"
)

admin_replace_vector <- c(
  "Julie Crum" = "1",
  "Christina Hidalgo" = "2",
  "Rich Greywolf" = "3",
  "Theresa Ambrogi" = "4",
  "Ronda Davis" = "5",
  "Dr. Janet Gladu" = "6",
  "Jami Jacobson" = "7",
  "Tamara Gaudet" = "8",
  "Jennifer Sears" = "9",
  "Tara Byers" = "10",
  "Jennifer Johnson" = "11",
  "Michelle Hopper" = "12",
  "Lisa Myhre" = "13",
  "Justin Macdonald" = "14",
  "Vicki Chavez" = "15",
  "Fred Parker" = "16",
  "Melanie Alfaro" = "17",
  "Alfredo Reyes" = "18",
  "Amber Swinney" = "19",
  "Bryan Simpson" = "20",
  "Adrian Apodaca" = "21",
  "Lorenzo Gonzales" = "22",
  "Randy Merker" = "23",
  "Monica Martinez-Archuleta" = "24",
  "Mike Yara" = "25",
  "Donna Lucero" = "26"
)

## Deidentifies new mexico surveys with arbitrary, random replacement vector ##
deidentified_data <- nm_survey |>
  mutate(`Work email` = str_trim(`Work email`, side = "both"),
         prepost = ifelse(date_created <= as.Date("2022-04-01"), "pre", "post")) |>
  filter(response_status == "completed") |>
  left_join(names_and_emails, by = c("Work email" = "Email")) |>
  # full_join(extra_names_and_emails) |>
  dplyr::mutate(id = case_when(
    `Participant Name` %in% names(teacher_replace_vector) ~ paste0(tolower(Role), "_", str_replace_all(`Participant Name`, teacher_replace_vector)),
    `Participant Name` %in% names(admin_replace_vector) ~ paste0(tolower(Role), "_", str_replace_all(`Participant Name`, admin_replace_vector))
  )) |>
  select(-c(1:10)) |>
  drop_na(id) |>
  relocate(id, .before = 1) |>
  relocate(prepost, .after = id) #|> ### FOLLOWING CODE IS TO VERIFY EXISTENCE OF EVERY ID ###
  # select(c("Participant Name", "Role", id, "Work email")) |>
  # view()

## Set specific canvas domain and get gradebook ##
# set_canvas_domain("https://nmped.instructure.com/")
# course_gradebook <- get_course_gradebook(course_id = 1821)
# course_items <- get_course_items(course_id = 1821, item = "quizzes")

## Grab all canvas videos and unnest download links ##
## Get first canvas video ##

# video_extract <- function(df, assignment) {
#   df |>
#     dplyr::filter(assignment_name == assignment & !is.na(submitted_at)) |>
#     dplyr::select(user.name, url = preview_url, attachments) |>
#     dplyr::mutate(
#       download_link = purrr::map(dplyr::row_number(), ~ attachments[[.x]]$url),
#       download_link = strsplit(as.character(download_link), ",")
#     ) |>
#     tidyr::unnest(cols = download_link) |>
#     mutate(download_link = str_remove_all(download_link, "c\\(\\\"|\\\"| "))
# }
#
# nm_rename <- function(df, name_col) {
#   df |>
#     dplyr::mutate(
#       user.name = stringr::str_to_title( {{ name_col }}),
#       user.name = dplyr::case_when(
#         str_detect(
#           str_to_lower(user.name),
#           "adrian"
#         ) == T ~ "Adrian Apodaca",
#         str_detect(
#           str_to_lower(user.name),
#           "trej"
#         ) == T ~ "Elisa Trejo",
#         str_detect(
#           str_to_lower(user.name),
#           "lson"
#         ) == T ~ "Emily Wilson",
#         str_detect(
#           str_to_lower(user.name),
#           "aqui"
#         ) == T ~ "Gilbert Aquino",
#         str_detect(
#           str_to_lower(user.name),
#           "orras"
#         ) == T ~ "Isla Porras",
#         str_detect(
#           str_to_lower(user.name),
#           "lop|ephner"
#         ) == T ~ "Jamie Hephner",
#         str_detect(
#           str_to_lower(user.name),
#           "alis|alasaisis"
#         ) == T ~ "Josiah Alisasis",
#         str_detect(
#           str_to_lower(user.name),
#           "onald"
#         ) == T ~ "Justin Macdonald",
#         str_detect(
#           str_to_lower(user.name),
#           "cisn"
#         ) == T ~ "Maria Dolores Cisneros",
#         str_detect(
#           str_to_lower(user.name),
#           "antos"
#         ) == T ~ "Maria Santos",
#         str_detect(
#           str_to_lower(user.name),
#           "marj|marije"
#         ) == T ~ "Marjie Lyn Pineda",
#         str_detect(
#           str_to_lower(user.name),
#           "bandl"
#         ) == T ~ "Nagarajan Bandla",
#         str_detect(
#           str_to_lower(user.name),
#           "badd"
#         ) == T ~ "Rajasekhar Badditi",
#         str_detect(
#           str_to_lower(user.name),
#           "ong"
#         ) == T ~ "Rita Kwong",
#         str_detect(
#           str_to_lower(user.name),
#           "cote"
#         ) == T ~ "Sue Cote",
#         str_detect(
#           str_to_lower(user.name),
#           "vig"
#         ) == T ~ "Thomas Vigil",
#         str_detect(
#           str_to_lower(user.name),
#           "tiff"
#         ) == T ~ "Tiffany Barrion",
#         str_detect(
#           str_to_lower(user.name),
#           "bark"
#         ) == T ~ "Tommy Barksdale",
#         TRUE ~ as.character(user.name)
#       ),
#       video_name = paste0("teacher_", str_replace_all(user.name, teacher_replace_vector)),
#       video_name = ifelse(user.name %in% names(admin_replace_vector),
#                           paste0("admin_", str_replace_all(user.name, admin_replace_vector)),
#                           video_name)
#     )
# }
#
# canvas_video_1 <- video_extract(course_gradebook, assignment = "PRE Classroom Observation Video Recording")
# canvas_video_2 <- video_extract(course_gradebook, assignment = "MLR Video Submission")
# canvas_video_3 <- video_extract(course_gradebook, assignment = "POST Classroom Observation Video Recording")
#
# ## Rename canvas video ##
# canvas_video_rename_1 <- canvas_video_1 |>
#   nm_rename(name_col = user.name) |>
#   view()
#
# canvas_video_rename_2 <- canvas_video_2 |>
#   nm_rename(name_col = user.name) |>
#   view()
#
# canvas_video_rename_3 <- canvas_video_3 |>
#   nm_rename(name_col = user.name) |>
#   view()

# options(timeout = 100000)
# walk2(canvas_videos_renamed$download_link, 1:nrow(canvas_videos_renamed), ~ download.file(url = .x,
# destfile = paste0(here::here("videos/video_"), .y, ".mp4")))
## Desktop app Oauth
# yt_oauth(app_id = "831082437753-adff4nsh50kvn2nh71pn8be9gnl5tvjs.apps.googleusercontent.com",
#          app_secret = "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",
#          scope = "upload_and_manage_own_videos")

# myapp <- httr::oauth_app("google",
#                          key = "831082437753-adff4nsh50kvn2nh71pn8be9gnl5tvjs.apps.googleusercontent.com",
#                          secret = "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")

# scope <- "ssl"

# scope_url <- switch(
#   scope,
#   ssl = "https://www.googleapis.com/auth/youtube.force-ssl",
#   basic = "https://www.googleapis.com/auth/youtube",
#   own_account_readonly = "https://www.googleapis.com/auth/youtube.readonly",
#   upload_and_manage_own_videos ="https://www.googleapis.com/auth/youtube.upload",
#   partner_audit = "https://www.googleapis.com/auth/youtubepartner-channel-audit",
#   partner =  "https://www.googleapis.com/auth/youtubepartner"
# )
#
# google_token <- httr::oauth2.0_token(httr::oauth_endpoints("google"),
#                                      myapp,
#                                      scope = scope_url)

# options(google_token = google_token)

# yt_search("Barack Obama")


# upload_video(file = here::here("videos/video_1.mp4"),
#              snippet = list(title = "test",
#                             description = "test description"))

# canvas_videos_renamed$video_name <- str_replace_all(canvas_videos_renamed$video_name,
#                                                     "video_",
#                                                     "teacher_")

# map(canvas_videos_renamed$video_name, ~ tuber::upload_video(file = paste0("/Users/dunk/Teaching Lab/Coding/TeachingLab/videos/", str_replace_all(.x, "teacher", "video"), ".mp4"),
#                                                                               snippet = list(
#                                                                                 title = .x)
#                                                                               ))

#### Note to self: The YouTube API rate limits you if you try to upload all of these videos at once,
### the saved dataframe here will allow you to view the "key" of actual names to
### the "video_number" format

### Get list of the youtube videos created ###
#### FIRST OAUTH WITHOUT TOKEN SETTING ####
yt_oauth(
  app_id = "831082437753-adff4nsh50kvn2nh71pn8be9gnl5tvjs.apps.googleusercontent.com",
  app_secret = "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",
  token = ""
)

deidentified_videos <- tuber::list_channel_videos(
  channel_id = "UCI0AXT77myurvsH8XXEvr5A",
  max_results = 50,
  part = c("snippet")
) |>
  mutate(link = paste0("https://www.youtube.com/watch?v=", snippet.resourceId.videoId))

first_videos <- deidentified_videos |>
  select(video_1 = link, title = snippet.title) |>
  filter(str_count(title, "_") == 1)
second_videos <- deidentified_videos |>
  select(video_2 = link, title = snippet.title) |>
  filter(str_detect(title, "_2$")) |>
  mutate(title = str_remove_all(title, "_[[:digit:]]$"))
third_videos <- deidentified_videos |>
  select(video_3 = link, title = snippet.title) |>
  filter(str_detect(title, "_3$") & title != "teacher_3") |>
  mutate(title = str_remove_all(title, "_[[:digit:]]$"))

# videos_1 <- tibble::tibble(
#   video = list.files(here::here("videos/Pre Classroom Videos"), full.names = T),
#   name = stringr::str_extract(list.files(here::here("videos/Pre Classroom Videos")), "[^\\.]+")
# ) |>
#   mutate(id = ifelse(name %in% names(teacher_replace_vector),
#                      paste0("teacher_", str_replace_all(name, teacher_replace_vector)),
#                      name),
#          id = ifelse(id %in% names(admin_replace_vector),
#                      paste0("admin_", str_replace_all(id, admin_replace_vector)),
#                      id)) |>
#   left_join()
#
# videos_2 <- tibble::tibble(
#   video = list.files(here::here("videos/MLR Videos"), full.names = T),
#   name = stringr::str_extract(list.files(here::here("videos/MLR Videos")), "[^\\.]+")
# ) |>
#   mutate(id = ifelse(name %in% names(teacher_replace_vector),
#                      paste0("teacher_", str_replace_all(name, teacher_replace_vector)),
#                      name),
#          id = ifelse(id %in% names(admin_replace_vector),
#                      paste0("admin_", str_replace_all(id, admin_replace_vector)),
#                      id))
#
# videos_3 <- tibble::tibble(
#   video = list.files(here::here("videos/Post Classroom Videos"), full.names = T),
#   name = stringr::str_extract(list.files(here::here("videos/Post Classroom Videos")), "[^\\.]+")
# ) |>
#   mutate(id = ifelse(name %in% names(teacher_replace_vector),
#                      paste0("teacher_", str_replace_all(name, teacher_replace_vector)),
#                      name),
#          id = ifelse(id %in% names(admin_replace_vector),
#                      paste0("admin_", str_replace_all(id, admin_replace_vector)),
#                      id)) |>
#   view()

## Deidentify data and save to a csv
deidentified_data |>
  left_join(first_videos, by = c("id" = "title")) |>
  left_join(second_videos, by = c("id" = "title")) |>
  left_join(third_videos, by = c("id" = "title")) |>
  select(-`Participant Name`) |>
  readr::write_csv(here::here("data/sy21_22/new_mexico_data/new_mexico_21_22_deidentified.csv"))


## Upload to dropbox
drop_upload(
  file = here::here("data/sy21_22/new_mexico_data/new_mexico_21_22_deidentified.csv"),
  path = "/New Mexico Deidentified Data (2021-2022)",
  mode = "overwrite"
)



## New Mexico Student Survey
nm_student_survey <- surveymonkey::fetch_survey_obj(315708746) |>
  surveymonkey::parse_survey()

## Replace all teacher names after ensuring that they are correctly formatted for replacement
nm_student_deidentified <- nm_student_survey |>
  mutate(
    teacher = `What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`,
    teacher = str_trim(teacher, side = "both"),
    teacher = case_when(
      str_detect(
        str_to_lower(teacher),
        "agui"
      ) == T ~ "Daniel Aguilar",
      str_detect(
        str_to_lower(teacher),
        "trej"
      ) == T ~ "Elisa Trejo",
      str_detect(
        str_to_lower(teacher),
        "aqui"
      ) == T ~ "Gilbert Aquino",
      str_detect(
        str_to_lower(teacher),
        "lop|jamie"
      ) == T ~ "Jamie Hephner",
      str_detect(
        str_to_lower(teacher),
        "alis|alasaisis"
      ) == T ~ "Josiah Alisasis",
      str_detect(
        str_to_lower(teacher),
        "cisn"
      ) == T ~ "Maria Dolores Cisneros",
      str_detect(
        str_to_lower(teacher),
        "antos"
      ) == T ~ "Maria Santos",
      str_detect(
        str_to_lower(teacher),
        "marj|marije"
      ) == T ~ "Marjie Lyn Pineda",
      str_detect(
        str_to_lower(teacher),
        "badd"
      ) == T ~ "Rajasekhar Badditi",
      str_detect(
        str_to_lower(teacher),
        "bandl"
      ) == T ~ "Nagarajan Bandla",
      str_detect(
        str_to_lower(teacher),
        "ong"
      ) == T ~ "Rita Kwong",
      str_detect(
        str_to_lower(teacher),
        "cote"
      ) == T ~ "Sue Cote",
      str_detect(
        str_to_lower(teacher),
        "vig"
      ) == T ~ "Thomas Vigil",
      str_detect(
        str_to_lower(teacher),
        "tiz"
      ) == T ~ "Tiffany Barrion",
      str_detect(
        str_to_lower(teacher),
        "bark|tommy"
      ) == T ~ "Tommy Barksdale",
      TRUE ~ as.character(teacher)
    )
  ) |>
  ### Dealing with duplicate ids 1:20 from Teachers Rajasekhar Badditi and Jamie Hephner ###
  mutate(`What is your student identification number as provided by your school or teacher?` = ifelse(teacher == "Rajasekhar Badditi",
                                                                                                      as.character(as.numeric(`What is your student identification number as provided by your school or teacher?`) * 100),
                                                                                                      `What is your student identification number as provided by your school or teacher?`),
         `What is your student identification number as provided by your school or teacher?` = ifelse(teacher == "Jamie Hephner",
                                                                                                      as.character(as.numeric(`What is your student identification number as provided by your school or teacher?`) * 101),
                                                                                                      `What is your student identification number as provided by your school or teacher?`)) |>
  filter(teacher %!in% c("1", "Mrs. Agino", "Daniel Aguilar") & 
           response_status == "completed" &
           !is.na(as.numeric(`What is your student identification number as provided by your school or teacher?`))) |>
  drop_na(teacher, `What is your student identification number as provided by your school or teacher?`) |>
  group_by(`What is your student identification number as provided by your school or teacher?`) |>
  mutate(student_id = paste0("student_", dplyr::cur_group_id()),
         prepost = ifelse(date_created <= as.Date("2022-04-01"), "pre", "post")) |>
  mutate(teacher_id = ifelse(teacher %in% names(teacher_replace_vector),
                     paste0("teacher_", str_replace_all(teacher, teacher_replace_vector)),
                     "")) |>
  write_csv(here::here("data/sy21_22/new_mexico_data/identified_student_new_mexico_21_22.csv")) |>
  # select(student_id, teacher_id, prepost, teacher,
  #        `What is your student identification number as provided by your school or teacher?`) |>
  # view() ### FOR INSPECTING IDS ###
  select(
    -c(survey_id, collector_id, respondent_id, date_created, date_modified, response_status, ip_address,
       teacher, 
       `What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`,
       `What is your student identification number as provided by your school or teacher?`)
  ) |>
  relocate(student_id, .before = 1) |>
  relocate(teacher_id, .after = student_id) |>
  relocate(prepost, .after = teacher_id)

## Store deidentified data as a csv
nm_student_deidentified |>
  write_csv(here::here("data/sy21_22/new_mexico_data/student_new_mexico_21_22_deidentified.csv"))



## Upload to dropbox
drop_upload(
  file = here::here("data/sy21_22/new_mexico_data/student_new_mexico_21_22_deidentified.csv"),
  path = "/New Mexico Deidentified Data (2021-2022)",
  mode = "overwrite"
)

###### PASSWORD: 8giow4ra #######
