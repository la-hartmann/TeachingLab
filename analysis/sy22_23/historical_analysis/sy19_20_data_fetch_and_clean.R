library(googledrive)
library(googlesheets4)
library(tidyverse)

sy19_20_session_survey_urls <- googledrive::drive_ls(as_id("1mQw9maNpBgP8H54Wtb46Gi-H9RwbnNSW"))

final_list <- sy19_20_session_survey_urls |>
  dplyr::filter(str_detect(name, "2019") & !str_detect(name, "Jill"))

get_spreadsheet_data <- function(id) {
  
  Sys.sleep(3)
  
  id <- googlesheets4::as_sheets_id(id)

  sheet_props <- googlesheets4::sheet_properties(id)

  if (!any(str_detect(sheet_props$name, "SY1920_Form Responses"))) {
    sheet <- "Form Responses"
  } else {
    sheet <- "SY1920_Form Responses"
  }

  initial <- googlesheets4::read_sheet(
    ss = id,
    sheet = sheet,
    skip = 3,
    range = "B:F",
    col_names = c("Timestamp", "Score", "Select the date for this training.", "Please select the focus of your professional learning", "Please select the professional training session you attended today.")
  )

  second <- googlesheets4::read_sheet(
    ss = id,
    sheet = sheet,
    skip = 3,
    range = "KK:LG",
    col_names = c("site", "role", "grade", "I am satisfied with the overall quality of today’s professional learning session.", "Today’s topic was relevant for my role.", "The activities of today’s session were well-designed to help me learn.", "What is the learning from this professional learning that you are most excited about trying out?", "How likely are you to apply this learning to your practice in the next 4-6 weeks?", "facilitator1", "S/he facilitated the content clearly.", "S/he effectively built a community of learners.", "Did you have a second facilitator?", "facilitator2", "S/he facilitated the content clearly_2", "S/he effectively built a community of learners_2", "Overall, what went well in this professional learning?", "Which activities best supported your learning?", "What could have improved your experience?", "How likely are you to recommend this professional learning to a colleague or friend?", "Why did you choose this rating?", "Do you have additional comments?", "If you would like to speak to our team further about your experience with the Teaching Lab professional learning please share your email address and we will reach out to you shortly.", "Do you give us permission to include your feedback in promotional materials?")
  )

  if (nrow(second) != nrow(initial)) {
    times <- nrow(initial) - nrow(second)
    print(times)
    second <- second |>
      dplyr::add_row(`Do you give us permission to include your feedback in promotional materials?` = rep(NA, times))
  }

  final <- initial |>
    dplyr::bind_cols(second) |>
    mutate(across(where(is.list), ~ as.character(.x)))

  final
}

session_survey_19_20 <- list_rbind(map(final_list$id[-20], ~ get_spreadsheet_data(.x)))

session_survey_19_20_cleaned <- session_survey_19_20 |>
  slice(-c(1:4)) |>
  mutate(Timestamp = as_datetime(as.numeric(Timestamp)),
         across(c(`I am satisfied with the overall quality of today’s professional learning session.`,
                  `Today’s topic was relevant for my role.`,
                  `The activities of today’s session were well-designed to help me learn.`,
                  `What is the learning from this professional learning that you are most excited about trying out?`,
                  `How likely are you to apply this learning to your practice in the next 4-6 weeks?`,
                  `S/he facilitated the content clearly.`,
                  `S/he facilitated the content clearly_2`,
                  `S/he effectively built a community of learners.`,
                  `S/he effectively built a community of learners_2`), ~ stringr::str_replace_all(.x, c("1" = "(1) Strongly disagree",
                                                                                                        "2" = "(2) Disagree",
                                                                                                        "3" = "(3) Neither agree nor disagree",
                                                                                                        "4" = "(4) Agree",
                                                                                                        "5" = "(5) Strongly agree"))),
         across(where(is.character), ~ na_if(.x, "NULL"))) |>
  select(-c(Score, `Select the date for this training.`)) |>
  rename(course = `Please select the professional training session you attended today.`,
         RecordedDate = Timestamp,
         fac_feedback_2 = `S/he facilitated the content clearly.`,
         fac_feedback_3 = `S/he effectively built a community of learners.`,
         fac_feedback_2_2 = `S/he facilitated the content clearly_2`,
         fac_feedback_2_3 = `S/he effectively built a community of learners_2`,
         course_feedback_14 = `I am satisfied with the overall quality of today’s professional learning session.`,
         course_feedback_8 = `Today’s topic was relevant for my role.`,
         course_feedback_3 = `The activities of today’s session were well-designed to help me learn.`,
         learning_excited_try = `What is the learning from this professional learning that you are most excited about trying out?`,
         course_feedback_12 = `How likely are you to apply this learning to your practice in the next 4-6 weeks?`,
         second_fac_q = `Did you have a second facilitator?`,
         overall_went_well = `Overall, what went well in this professional learning?`,
         best_activities = `Which activities best supported your learning?`,
         been_better_course = `What could have improved your experience?`,
         nps = `How likely are you to recommend this professional learning to a colleague or friend?`,
         why_this_nps = `Why did you choose this rating?`,
         course_add_comments = `Do you have additional comments?`,
         email = `If you would like to speak to our team further about your experience with the Teaching Lab professional learning please share your email address and we will reach out to you shortly.`,
         promo_permission = `Do you give us permission to include your feedback in promotional materials?`,
         focus = `Please select the focus of your professional learning`)

session_survey_19_20_final <- session_survey_19_20_cleaned |>
  dplyr::select(RecordedDate, course, focus, role, grade, email, site, contains("fac"), promo_permission)

write.csv(session_survey_19_20_final, here::here("data/sy19_20/session_survey.csv"))

course_survey_19_20_final <- session_survey_19_20_cleaned |>
  dplyr::select(RecordedDate, course, focus, role, grade, email, site, contains("course"), 
                learning_excited_try, best_activities, been_better_course, overall_went_well,
                nps, why_this_nps, promo_permission)

write.csv(course_survey_19_20_final, here::here("data/sy19_20/course_survey.csv"))

### Why is skip argument just not working???? ###
# check <- range_read("https://docs.google.com/spreadsheets/d/1Gi6pwCuD01JBLPUYOmS6rJ_9UPR05XviiCAGkpaWwBQ/edit#gid=0",
#   sheet = "SY1920_Form Responses",
#   skip = 3,
#   col_names = c("site", "role", "grade", "I am satisfied with the overall quality of today’s professional learning session.", "Today’s topic was relevant for my role.", "The activities of today’s session were well-designed to help me learn.", "What is the learning from this professional learning that you are most excited about trying out?", "How likely are you to apply this learning to your practice in the next 4-6 weeks?", "facilitator1", "S/he facilitated the content clearly.", "S/he effectively built a community of learners.", "Did you have a second facilitator?", "facilitator2", "S/he facilitated the content clearly_2", "S/he effectively built a community of learners_2", "Overall, what went well in this professional learning?", "Which activities best supported your learning?", "What could have improved your experience?", "How likely are you to recommend this professional learning to a colleague or friend?", "Why did you choose this rating?", "Do you have additional comments?", "If you would like to speak to our team further about your experience with the Teaching Lab professional learning please share your email address and we will reach out to you shortly.", "Do you give us permission to include your feedback in promotional materials?"),
#   range = "KK:LG"
# )
