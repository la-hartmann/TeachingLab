library(tidyverse)
library(TeachingLab)

old_df <- readr::read_rds("data-clean/data-move/dashboard_data/dashboard_data.rds")

old_df %>%
  filter(`Date for the session` <= as.Date("2021-07-01") & `Date for the session` >= as.Date("2020-07-01") & str_detect(
    `District, Parish, Or Network`,
    "11"
  )) %>%
  select(
    `% Satisfied With The Overall Quality Of Today's Professional Learning Session`,
    `% Who Say Today's Topic Was Relevant For My Role`,
    `% Who Say Activities Of Today's Session Were Well-Designed To Help Me Learn`,
    `How Likely Are You To Apply This Learning To Your Practice In The Next 4-6 Weeks?`,
    `S/He Facilitated The Content Clearly`,
    `S/He Effectively Built A Community Of Learners`,
  ) %>%
  mutate(across(everything(), ~ na_if(.x, "No Response"))) %>%
  summarise(across(everything(), ~ round(100 * sum(.x %in% c("Strongly agree", "Agree"), na.rm = T) /
    sum(.x %in% c(
      "Strongly agree", "Agree", "Neither agree nor disagree",
      "Disagree", "Strongly disagree"
    ), na.rm = T)))) %>%
  gt::gt()

old_df %>%
  filter(`Date for the session` <= as.Date("2020-07-01") & str_detect(
    `District, Parish, Or Network`,
    "11"
  )) %>%
  select(
    `% Satisfied With The Overall Quality Of Today's Professional Learning Session`,
    `% Who Say Today's Topic Was Relevant For My Role`,
    `% Who Say Activities Of Today's Session Were Well-Designed To Help Me Learn`,
    `How Likely Are You To Apply This Learning To Your Practice In The Next 4-6 Weeks?`,
    `S/He Facilitated The Content Clearly`,
    `S/He Effectively Built A Community Of Learners`,
    `The independent online work activities were well-designed to help me meet the learning targets.`,
    `The Zoom meeting activities were well-designed to help me meet the learning targets.`,
    `I felt a sense of community with the other participants in this course even though we were meeting virtually.`,
    `This course helped me navigate remote and/or hybrid learning during COVID-19.`
  ) %>%
  mutate(across(everything(), ~ na_if(.x, "No Response"))) %>%
  summarise(across(everything(), ~ round(100 * sum(.x %in% c("Strongly agree", "Agree"), na.rm = T) /
                                           sum(.x %in% c(
                                             "Strongly agree", "Agree", "Neither agree nor disagree",
                                             "Disagree", "Strongly disagree"
                                           ), na.rm = T)))) %>%
  gt::gt()
