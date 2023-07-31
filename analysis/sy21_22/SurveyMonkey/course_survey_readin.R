##### COURSE SURVEY #######

### This script should never be run it was a temporary fix ###
# options(sm_oauth_token = "a22Dkw3KTSZB9v.TYV0g2GAV2fRK7dfmQ81WEk1iqnTrcUUQpcksI1fRc44J-H0fcN3OAovcaQRNb38fhScbHpiUJu4vDP-9SZuXuwHNwcNRK035sJ7VjQFPOUnKi3TT")
# old_df <- readr::read_rds("data/sy20_21/course_survey2021data.rds")
# course_survey <- surveymonkey::fetch_survey_obj(id = 308116695) %>%
#   surveymonkey::parse_survey() %>%
#   dplyr::mutate(date_created = lubridate::date(date_created)) %>%
#   dplyr::mutate(`Select your course.` = dplyr::coalesce(`Select your course.`, `Select your course._2`, `Select your course._3`,
#                                                         `Select your course._4`, `Select your course._5`, `Select your course._6`)) %>%
#   dplyr::mutate(`date_created` = as.Date(`date_created`)) %>%
#   dplyr::bind_rows(old_df) %>%
#   dplyr::mutate(`Select your site (district, parish, network, or school).` = stringr::str_replace_all(`Select your site (district, parish, network, or school).`, "Pt. Coupee Parish", "Pointe Coupee Parish")) %>%
#   dplyr::mutate(`Select your site (district, parish, network, or school).` = stringr::str_remove_all(`Select your site (district, parish, network, or school).`,
#                                                                                                      ", PA/DE|, LA|, PA|, CA|, SC|, VT|, IL|, NY|, NE|, MS|, RI")) %>%
#   dplyr::mutate(`How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.` = dplyr::na_if(`How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.`, "-999")) %>%
#   dplyr::mutate(dplyr::across(c(`How much do you agree with the following statements about this course? - I am satisfied with the overall quality of this course.`,
#                                 `How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.`,
#                                 `How much do you agree with the following statements about this course? - The independent online work activities were well-designed to help me meet the learning targets.`,
#                                 `How much do you agree with the following statements about this course? - I felt a sense of community with the other participants in this course. even though we were meeting virtually.`,
#                                 `How much do you agree with the following statements about this course? - I will apply what I have learned in this course to my practice in the next 4-6 weeks.`), ~ stringr::str_replace_all(.x,
#                                                                                                                                                                                                                               c("(?<! )Strongly agree" = "(5) Strongly agree",
#                                                                                                                                                                                                                                 "(?<! )Agree" = "(4) Agree",
#                                                                                                                                                                                                                                 "(?<! )Neither agree nor disagree" = "(3) Neither agree nor disagree",
#                                                                                                                                                                                                                                 "(?<! )Disagree" = "(2) Disagree",
#                                                                                                                                                                                                                                 "(?<! )Strongly disagree" = "(1) Strongly disagree")))) %>%
#   dplyr::mutate(`How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.` = dplyr::na_if(`How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.`, "-999")) %>%
#   dplyr::mutate(across(c(`How much do you agree with the following statements about this course? - I am satisfied with the overall quality of this course.`,
#                          `How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.`,
#                          `How much do you agree with the following statements about this course? - The independent online work activities were well-designed to help me meet the learning targets.`,
#                          `How much do you agree with the following statements about this course? - I felt a sense of community with the other participants in this course. even though we were meeting virtually.`,
#                          `How much do you agree with the following statements about this course? - I will apply what I have learned in this course to my practice in the next 4-6 weeks.`), ~ dplyr::na_if(.x, "No Response")))
