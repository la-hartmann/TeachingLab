# library(magrittr)
# ####### This is a script specifically designed for the AMS dashboards created for Ijun ######
# ####### ALL it does is filter for AMS sites and writes that to the dashboards data file ######
# 
# 
# options(sm_oauth_token = Sys.getenv("course_token"))
# 
# surveymonkey_course <- surveymonkey::fetch_survey_obj(id = 308116695) |>
#   surveymonkey::parse_survey()
# 
# course_survey <- surveymonkey_course |>
#   # Make data column a date type column
#   dplyr::mutate(
#     date_created = lubridate::date(date_created),
#     `Select the date for this session. - \n    Date / Time\n` = lubridate::date(lubridate::mdy(`Select the date for this session. - \n    Date / Time\n`))
#   ) |>
#   # Add dataframe rows from prior to 21-22
#   # dplyr::bind_rows(old_df) |>
#   # Coalesce old date column with new
#   dplyr::mutate(date_created = dplyr::coalesce(
#     date_created,
#     `Select the date for this session. - \n    Date / Time\n`
#   )) |>
#   # Make NPS numeric and fix non-numerics
#   dplyr::mutate(
#     `On a scale of 0-10, how likely are you to recommend this course to a colleague or friend?` =
#       readr::parse_number(as.character(`On a scale of 0-10, how likely are you to recommend this course to a colleague or friend?`))
#   ) |>
#   # Coalesce all select your course columns
#   dplyr::mutate(`Select your course.` = dplyr::coalesce(
#     `Select your course.`,
#     `Select your course._2`,
#     `Select your course._3`,
#     `Select your course._4`,
#     `Select your course._5`,
#     `Select your course._6`
#   )) |>
#   # Coalesce what went well in the course
#   dplyr::mutate(`Overall, what went well in this course?` = dplyr::coalesce(
#     `Overall, what went well in this course?`,
#     `Overall, what went well in this course?_2`
#   )) |>
#   # Coalesce what could have been better in the course
#   dplyr::mutate(`Overall, what could have been better in this course?` = dplyr::coalesce(
#     `Overall, what could have been better in this course?`,
#     `Overall, what could have been better in this course?_2`
#   )) |>
#   # Coalesce learning from the course excited about
#   dplyr::mutate(`What is the learning from this course that you are most excited about trying out?` = dplyr::coalesce(
#     `What is the learning from this course that you are most excited about trying out?`,
#     `What is the learning from this course that you are most excited about trying out?_2`
#   )) |>
#   # Coalesce best activities supporting learning
#   dplyr::mutate(`Which activities best supported your learning in this course?` = dplyr::coalesce(
#     `Which activities best supported your learning in this course?`,
#     `Which activities best supported your learning in this course?_2`
#   )) |>
#   # Coalesce additional comments, concerns, or questions
#   dplyr::mutate(`Feel free to leave us any additional comments, concerns, or questions.` = dplyr::coalesce(
#     `Feel free to leave us any additional comments, concerns, or questions.`,
#     `Feel free to leave us any additional comments, concerns, or questions._2`
#   )) |>
#   # Probably redundant, check later
#   dplyr::mutate(`date_created` = as.Date(`date_created`)) |>
#   # Fix Pointe Coupee
#   dplyr::mutate(`Select your site (district, parish, network, or school).` = stringr::str_replace_all(`Select your site (district, parish, network, or school).`, "Pt. Coupee Parish", "Pointe Coupee Parish")) |>
#   # Remove extra parts of names so they will be the same
#   dplyr::mutate(`Select your site (district, parish, network, or school).` = stringr::str_remove_all(
#     `Select your site (district, parish, network, or school).`,
#     ", PA/DE|, LA|, PA|, CA|, SC|, VT|, IL|, NY|, NE|, MS|, RI"
#   )) |>
#   # Make Rochester all the same name regardless of school
#   dplyr::mutate(`Select your site (district, parish, network, or school).` = ifelse(stringr::str_detect(`Select your site (district, parish, network, or school).`, "Rochester"),
#                                                                                     "Rochester City School District",
#                                                                                     as.character(`Select your site (district, parish, network, or school).`)
#   )) |>
#   # Get rid of random -999 in responses
#   dplyr::mutate(`How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.` = dplyr::na_if(`How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.`, "-999")) |>
#   # Fix agree/not agree formatting
#   dplyr::mutate(dplyr::across(c(
#     `How much do you agree with the following statements about this course? - I am satisfied with the overall quality of this course.`,
#     `How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.`,
#     `How much do you agree with the following statements about this course? - The independent online work activities were well-designed to help me meet the learning targets.`,
#     `How much do you agree with the following statements about this course? - I felt a sense of community with the other participants in this course. even though we were meeting virtually.`,
#     `How much do you agree with the following statements about this course? - I will apply what I have learned in this course to my practice in the next 4-6 weeks.`
#   ), ~ stringr::str_replace_all(
#     .x,
#     c(
#       "(?<! )Strongly agree" = "(5) Strongly agree",
#       "(?<! )Agree" = "(4) Agree",
#       "(?<! )Neither agree nor disagree" = "(3) Neither agree nor disagree",
#       "(?<! )Disagree" = "(2) Disagree",
#       "(?<! )Strongly disagree" = "(1) Strongly disagree"
#     )
#   ))) |>
#   # Add no response to data if it is NA
#   dplyr::mutate(dplyr::across(c(
#     `How much do you agree with the following statements about this course? - I am satisfied with the overall quality of this course.`,
#     `How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.`,
#     `How much do you agree with the following statements about this course? - The independent online work activities were well-designed to help me meet the learning targets.`,
#     `How much do you agree with the following statements about this course? - I felt a sense of community with the other participants in this course. even though we were meeting virtually.`,
#     `How much do you agree with the following statements about this course? - I will apply what I have learned in this course to my practice in the next 4-6 weeks.`
#   ), ~ dplyr::na_if(.x, "No Response"))) |>
#   ###### Make it select just the necessary columns to reduce data input to dashboards
#   dplyr::select(
#     date_created, # Date
#     `Select your site (district, parish, network, or school).`, # Site
#     `Select your role.`, # Role
#     `Select the content area for today's professional learning session.`, # Content area
#     `Select your course.`, # Course
#     `Overall, what went well in this course?`, # Qualitative feedback
#     `Overall, what could have been better in this course?`, # Qualitative feedback
#     `What is the learning from this course that you are most excited about trying out?`, # Qualitative feedback
#     `Which activities best supported your learning in this course?`, # Qualitative feedback
#     `Feel free to leave us any additional comments, concerns, or questions.`, # Qualitative feedback
#     ###### Quantitative feedback #####
#     `How much do you agree with the following statements about this course? - I am satisfied with the overall quality of this course.`,
#     `How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.`,
#     `How much do you agree with the following statements about this course? - The independent online work activities were well-designed to help me meet the learning targets.`,
#     `How much do you agree with the following statements about this course? - I felt a sense of community with the other participants in this course. even though we were meeting virtually.`,
#     `How much do you agree with the following statements about this course? - The strategies I’ve learned in this course will improve my instruction.`,
#     `How much do you agree with the following statements about this course? - The strategies I’ve learned in this course will improve my coaching or supervision of teachers.`,
#     `How much do you agree with the following statements about this course? - The strategies I’ve learned in the course are easy to implement.`,
#     `How much do you agree with the following statements about this course? - I will apply what I have learned in this course to my practice in the next 4-6 weeks.`,
#     `How much do you agree with the following statements about this course? - This course has supported me in being responsive to students' backgrounds, cultures, and points of view.`,
#     # NPS
#     `On a scale of 0-10, how likely are you to recommend this course to a colleague or friend?`,
#     # Grades
#     `What grade(s) do you teach, support, and/or lead? You can select more than one. - K`,
#     `What grade(s) do you teach, support, and/or lead? You can select more than one. - 1`,
#     `What grade(s) do you teach, support, and/or lead? You can select more than one. - 2`,
#     `What grade(s) do you teach, support, and/or lead? You can select more than one. - 3`,
#     `What grade(s) do you teach, support, and/or lead? You can select more than one. - 4`,
#     `What grade(s) do you teach, support, and/or lead? You can select more than one. - 5`,
#     `What grade(s) do you teach, support, and/or lead? You can select more than one. - 6`,
#     `What grade(s) do you teach, support, and/or lead? You can select more than one. - 7`,
#     `What grade(s) do you teach, support, and/or lead? You can select more than one. - 8`,
#     `What grade(s) do you teach, support, and/or lead? You can select more than one. - 9`,
#     `What grade(s) do you teach, support, and/or lead? You can select more than one. - 10`,
#     `What grade(s) do you teach, support, and/or lead? You can select more than one. - 11`,
#     `What grade(s) do you teach, support, and/or lead? You can select more than one. - 12`
#   )

# options(sm_oauth_token = options(sm_oauth_token = Sys.getenv("session_token")))
# 
# surveymonkey_session <- surveymonkey::fetch_survey_obj(id = 308115193) |>
#   surveymonkey::parse_survey()
# 
# session_survey <- surveymonkey_session |>
#   dplyr::mutate(date_created = lubridate::date(date_created)) |>
#   dplyr::mutate(`Select your course.` = dplyr::coalesce(
#     `Select your course.`,
#     `Select your course._2`,
#     `Select your course._3`,
#     `Select your course._4`,
#     `Select your course._5`,
#     `Select your course._6`
#   )) |>
#   dplyr::mutate(Date = lubridate::ymd(date_created)) |>
#   # Fix this cluttering of names the others result in a bunch of different formats
#   dplyr::mutate(dplyr::across(c(
#     "Select the name of your facilitator.", "Select the name of your facilitator. - Other (please specify)",
#     "Select the name of your facilitator._2", "Select the name of your facilitator. - Other (please specify)_2",
#     "Select the name of your facilitator._3", "Select the name of your facilitator. - Other (please specify)_3"
#   ), ~ dplyr::na_if(.x, "Name"))) |>
#   dplyr::mutate(
#     Facilitator = dplyr::coalesce(
#       `Select the name of your facilitator.`,
#       `Select the name of your facilitator._2`,
#       `Select the name of your facilitator._3`,
#       `Select the name of your facilitator._4`,
#       `Select the name of your facilitator._5`,
#       `Select the name of your facilitator._6`,
#       `Select the name of your facilitator. - Other (please specify)`,
#       `Select the name of your facilitator. - Other (please specify)_2`,
#       `Select the name of your facilitator. - Other (please specify)_3`,
#       `Select the name of your facilitator. - Other (please specify)_4`,
#       `Select the name of your facilitator. - Other (please specify)_5`,
#       `Select the name of your facilitator. - Other (please specify)_6`
#     ),
#     Facilitation_Feedback = dplyr::coalesce(
#       `What additional feedback do you have about their facilitation skills?`,
#       `What additional feedback do you have about their facilitation skills?_2`,
#       `What additional feedback do you have about their facilitation skills?_3`,
#       `What additional feedback do you have about their facilitation skills?_4`,
#       `What additional feedback do you have about their facilitation skills?_5`,
#       `What additional feedback do you have about their facilitation skills?_6`
#     )
#   ) |>
#   dplyr::mutate(
#     `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated.` =
#       dplyr::coalesce(
#         `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated.`,
#         `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated._2`,
#         `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated._3`,
#         `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated._4`,
#         `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated._5`,
#         `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated._6`
#       )
#   ) |>
#   dplyr::mutate(
#     `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.` =
#       dplyr::coalesce(
#         `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.`,
#         `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly._2`,
#         `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly._3`,
#         `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly._4`,
#         `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly._5`,
#         `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly._6`
#       )
#   ) |>
#   dplyr::mutate(
#     `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.` =
#       dplyr::coalesce(
#         `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.`,
#         `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community._2`,
#         `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community._3`,
#         `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community._4`,
#         `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community._5`,
#         `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community._6`
#       )
#   ) |>
#   dplyr::mutate(
#     `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.` =
#       dplyr::coalesce(
#         `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.`,
#         `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session._2`,
#         `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session._3`,
#         `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session._4`,
#         `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session._5`,
#         `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session._6`
#       )
#   ) |>
#   dplyr::mutate(
#     `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs.` =
#       dplyr::coalesce(
#         `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs.`,
#         `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs._2`,
#         `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs._3`,
#         `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs._4`,
#         `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs._5`,
#         `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs._6`
#       )
#   ) |>
#   dplyr::mutate(`Select your site (district, parish, network, or school).` = ifelse(stringr::str_detect(`Select your site (district, parish, network, or school).`, "Rochester"),
#                                                                                     "Rochester City School District",
#                                                                                     as.character(`Select your site (district, parish, network, or school).`)
#   )) |>
#   dplyr::select(
#     Facilitator, # Facilitator
#     Date, # Date
#     `Select your site (district, parish, network, or school).`, # Site
#     `Select your role.`, # Role
#     `Select the content area for today’s professional learning session.`, # Content area
#     `Select your course.`, # Course
#     ###### Quantitative feedback #####
#     `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated.`,
#     `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.`,
#     `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.`,
#     `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.`,
#     `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs.`,
#     Facilitation_Feedback, # Qualitative feedback
#     `What went well in today’s session?`,
#     `What could have been better about today’s session?`,
#     # Grades
#     `What grade(s) do you teach, support, and/or lead? You can select more than one. - K`,
#     `What grade(s) do you teach, support, and/or lead? You can select more than one. - 1`,
#     `What grade(s) do you teach, support, and/or lead? You can select more than one. - 2`,
#     `What grade(s) do you teach, support, and/or lead? You can select more than one. - 3`,
#     `What grade(s) do you teach, support, and/or lead? You can select more than one. - 4`,
#     `What grade(s) do you teach, support, and/or lead? You can select more than one. - 5`,
#     `What grade(s) do you teach, support, and/or lead? You can select more than one. - 6`,
#     `What grade(s) do you teach, support, and/or lead? You can select more than one. - 7`,
#     `What grade(s) do you teach, support, and/or lead? You can select more than one. - 8`,
#     `What grade(s) do you teach, support, and/or lead? You can select more than one. - 9`,
#     `What grade(s) do you teach, support, and/or lead? You can select more than one. - 10`,
#     `What grade(s) do you teach, support, and/or lead? You can select more than one. - 11`,
#     `What grade(s) do you teach, support, and/or lead? You can select more than one. - 12`
#   )
# 
# # course_survey <- readr::read_rds(here::here("data/merged/course_surveymonkey.rds"))
# # session_survey <- readr::read_rds(here::here("data/sy21_22/session_survey_21_22data.rds"))
# 
# ams_course_survey <- course_survey |>
#   dplyr::filter(`Select your site (district, parish, network, or school).` %in% c("Cleveland Metropolitan School District, OH",
#                                                                                   "Cleveland Metropolitan School District",
#                                                                            "NYC District 12 - EMST-IS 190",
#                                                                            "NYC District 12 - MS 286 Fannie Lou Hamer",
#                                                                            "NYC District 12 - MS 286",
#                                                                            "NYC District 6 - MS311",
#                                                                            "San Diego Unified School District"))
# ams_session_survey <- session_survey |>
#   dplyr::filter(`Select your site (district, parish, network, or school).` %in% c("Cleveland Metropolitan School District, OH",
#                                                                                   "NYC District 12 - EMST-IS 190",
#                                                                                   "NYC District 12 - EMST-IS 190, N",
#                                                                                   "NYC District 12 - EMST-IS 190, NY",
#                                                                                   "NYC District 12 - MS 286 Fannie Lou Hamer, NY",
#                                                                                   "NYC District 12 - MS 286, NY",
#                                                                                   "NYC District 6 - MS311, NY",
#                                                                                   "San Diego Unified School District, CA")) |>
#   dplyr::mutate(`Select your site (district, parish, network, or school).` = stringr::str_replace_all(`Select your site (district, parish, network, or school).`,
#                                                                                                       "NYC District 12 - MS 286 Fannie Lou Hamer, NY",
#                                                                                                       "NYC District 12 - MS 286, NY"),
#                 `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(`Select your site (district, parish, network, or school).`, "EMST", "NYC District 12 - EMST-IS 190"))
# 
# readr::write_rds(ams_course_survey, here::here("dashboards/AMS_dashboards/MathematicaCourseSurvey/data/course_surveymonkey.rds"))
# readr::write_rds(ams_session_survey, here::here("dashboards/AMS_dashboards/MathematicaSessionSurvey/data/session_survey_21_22data.rds"))
