library(TeachingLab)
library(tidyverse)

course_survey <- TeachingLab::get_course_survey()

course_survey |>
  dplyr::filter(date_created >= as.Date("2021-07-01") &
                  # stringr::str_detect(`Select your site (district, parish, network, or school).`,
                  #                     "McNairy|Delaware") &
                  stringr::str_detect(`Select your course.`, "Foundational Skills")) |>
  group_by(`Select your site (district, parish, network, or school).`) |>
  count()
  dplyr::select(`Overall, what went well in this course?`,
                `Overall, what could have been better in this course?`,
                `What is the learning from this course that you are most excited about trying out?`,
                `Which activities best supported your learning in this course?`,
                `Feel free to leave us any additional comments, concerns, or questions.`) |>
  TeachingLab::quote_viz(text_col = c("Overall, what went well in this course?",
                                      "Overall, what could have been better in this course?",
                                      "What is the learning from this course that you are most excited about trying out?",
                                      "Which activities best supported your learning in this course?",
                                      "Feel free to leave us any additional comments, concerns, or questions.")) |>
  gt::gtsave(here::here("images/quotes/delaware_mcnairy_2021_22_quotes.html"))

