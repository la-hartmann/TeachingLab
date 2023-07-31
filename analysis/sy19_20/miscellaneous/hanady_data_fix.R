library(tidyverse)

session_survey <- read_rds(here::here("data/sy21_22/session_survey_21_22data.rds"))

session_survey %>%
  dplyr::filter(between(date_created, as.Date("2021-10-04"), as.Date("2021-10-18"))) -> check

session_survey$`Select your site (district, parish, network, or school). - Other (please specify)` %>%
  unique() %>% 
  sort() %>%
  clipr::write_clip()

course_survey <- read_rds(here::here("data/sy21_22/course_survey_21_22.rds"))

course_survey %>%
  dplyr::filter(between(date_created, as.Date("2021-10-04"), as.Date("2021-10-18"))) -> check2

course_survey$`Select your site (district, parish, network, or school). - Other (please specify)` %>%
  unique() %>%
  sort() %>%
  clipr::write_clip()


library(stringdist)

check <- session_survey %>%
  mutate(leven = stringdist("north andover", 
                            `Select your site (district, parish, network, or school). - Other (please specify)`,
                            method = "lv")) %>%
  filter(leven < 12) %>%
  slice(c(-1, -26, -27))
