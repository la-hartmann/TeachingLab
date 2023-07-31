library(tidyverse)

session_survey <- readr::read_rds(here::here("data/sy21_22/session_survey_21_22data.rds"))

lindsay_data <- session_survey %>%
  filter(`Select your site (district, parish, network, or school).` == "Delaware Department of Education, DE" &
           `Select the content area for today’s professional learning session.` == "Math" #&
           # (`Select the name of your facilitator.` == "Amy Youngblood" | 
           #    `Select the name of your facilitator.` == "Spring Mercadel")
         )

lindsay_data %>%
  filter(date_created > as.Date("2022-01-01")) %>%
  select(Facilitator,
         date_created,
         `What additional feedback do you have about their facilitation skills?_3`,
         `What is one thing from today's learning that you plan to take back to your classroom?_2`,
         `What went well in today’s session?_2`,
         `What could have been better about today’s session?_2`) %>%
  dplyr::rename_with( ~ str_remove_all(.x, "_[:digit:]")) %>%
  janitor::remove_empty("rows") %>%
  readr::write_csv(here::here(glue::glue("data_requests/lindsay{Sys.Date()}.csv")))

course_survey <- readr::read_rds(here::here("data/sy21_22/course_survey_21_22.rds"))

lindsay_course <- course_survey %>%
  filter(`Select your site (district, parish, network, or school).` == "Delaware Department of Education, DE" &
           date_created > as.Date("2022-01-01")) %>%
  select(date_created,
         `Overall, what went well in this course?_2`,
         `Overall, what could have been better in this course?_2`,
         `What is the learning from this course that you are most excited about trying out?_2`,
         `Which activities best supported your learning in this course?_2`,
         `Feel free to leave us any additional comments, concerns, or questions._2`) %>%
  dplyr::rename_with( ~ str_remove_all(.x, "_[:digit:]")) %>%
  janitor::remove_empty("rows") %>%
  readr::write_csv(here::here(glue::glue("data_requests/lindsay_course{Sys.Date()}.csv")))
