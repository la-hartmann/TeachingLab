library(tidyverse)
library(surveymonkey)
library(googlesheets4)

options(sm_oauth_token = "8fm.HdMiwmQe9U6QcX.5M5ORukSRLs5D93Ye3SZoaWqyuUVmQp0fdw-M4kuM3UEGIzJEM3qupXm.xP88FN2raP88iBdkWZaJcraYv-rWp8eFgFtCDJ4ylIcw18buaNIR")

session_survey <- surveymonkey::fetch_survey_obj(id = 308115193) %>%
  surveymonkey::parse_survey() %>%
  dplyr::mutate(date_created = lubridate::date(date_created)) %>%
  dplyr::mutate(`Select your course.` = dplyr::coalesce(`Select your course.`, `Select your course._2`, `Select your course._3`,
                                                        `Select your course._4`, `Select your course._5`, `Select your course._6`)) %>%
  dplyr::mutate(Date = lubridate::mdy(`Select the date for this session. - \n    Date / Time\n`)) %>%
  # Fix this cluttering of names the others result in a bunch of differernt formats
  dplyr::mutate(dplyr::across(c("Select the name of your facilitator.", "Select the name of your facilitator. - Other (please specify)",
                                "Select the name of your facilitator._2", "Select the name of your facilitator. - Other (please specify)_2",
                                "Select the name of your facilitator._3", "Select the name of your facilitator. - Other (please specify)_3"), ~ dplyr::na_if(.x, "Name"))) %>%
  dplyr::mutate(Facilitator = dplyr::coalesce(`Select the name of your facilitator.`,
                                              `Select the name of your facilitator._2`,
                                              `Select the name of your facilitator._3`,
                                              `Select the name of your facilitator. - Other (please specify)_2`,
                                              `Select the name of your facilitator. - Other (please specify)_3`),
                Facilitation_Feedback = dplyr::coalesce(`What additional feedback do you have about their facilitation skills?`,
                                                        `What additional feedback do you have about their facilitation skills?_2`,
                                                        `What additional feedback do you have about their facilitation skills?_3`)) %>%
  dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated.` =
                  dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated.`,
                                  `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated._2`,
                                  `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated._3`)) %>%
  dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.` =
                  dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.`,
                                  `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly._2`,
                                  `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly._3`)) %>%
  dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.` =
                  dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.`,
                                  `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community._2`,
                                  `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community._3`)) %>%
  dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.` =
                  dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.`,
                                  `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session._2`,
                                  `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session._3`)) %>%
  dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs.` =
                  dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs.`,
                                  `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs._2`,
                                  `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs._3`))
# write_rds(session_survey, here::here("analysis/sy21_22/FacilitatorSheets/Data/session_survey.rds"))
# session_survey <- read_rds(here::here("analysis/sy21_22/FacilitatorSheets/Data/session_survey.rds"))

# devtools::load_all()
session_survey %>%
  tidyr::drop_na(Facilitator) %>%
  dplyr::group_by(Facilitator) %>%
  dplyr::summarise(n = dplyr::n(),
            prepared = TeachingLab::percent_agree(`How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.`),
            responded = TeachingLab::percent_agree(`How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs.`),
            facilitated = TeachingLab::percent_agree(`How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.`),
            safe_learning = TeachingLab::percent_agree(`How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.`),
            demonstrated = TeachingLab::percent_agree(`How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated.`)) %>%
  dplyr::mutate(dplyr::across(c(3:7), ~ .x/100)) %>%
  googlesheets4::range_write(
    ss = "https://docs.google.com/spreadsheets/d/1isE5j1XERQHVxTCb01RZXFRLvJRiEW-iH-N-AslMtxo/edit#gid=0",
    data = .,
    sheet = "% Agree/Strongly Agree",
    range = "A2",
    col_names = F,
    reformat = F
  )

session_survey %>%
  tidyr::drop_na(Facilitator) %>%
  dplyr::group_by(Facilitator) %>%
  dplyr::summarise(n = n(),
            prepared = mean(readr::parse_number(as.character(`How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.`)), na.rm = T),
            responded = mean(readr::parse_number(as.character(`How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs.`)), na.rm = T),
            facilitated = mean(readr::parse_number(as.character(`How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.`)), na.rm = T),
            safe_learning = mean(readr::parse_number(as.character(`How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.`)), na.rm = T),
            demonstrated = mean(readr::parse_number(as.character(`How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated.`)), na.rm = T)) %>%
  dplyr::mutate(dplyr::across(c(3:7), ~ round(.x, 2))) %>%
  googlesheets4::range_write(
    ss = "https://docs.google.com/spreadsheets/d/1isE5j1XERQHVxTCb01RZXFRLvJRiEW-iH-N-AslMtxo/edit#gid=0",
    data = .,
    sheet = "Average",
    range = "A2",
    col_names = F,
    reformat = F
  )

session_survey %>%
  write_sheet(ss = "https://docs.google.com/spreadsheets/d/1isE5j1XERQHVxTCb01RZXFRLvJRiEW-iH-N-AslMtxo/edit#gid=0",
              data = .,
              sheet = "Data")



# session_survey %>%
#   filter(is.na(Facilitator)) %>%
#   select(`Select your course.`, Facilitator, `Select the name of your facilitator.`, `Select the name of your facilitator._2`,
#          `Select the name of your facilitator._3`, `Select the name of your facilitator. - Other (please specify)_2`,
#          `Select the name of your facilitator. - Other (please specify)_3`) %>%
#   view()



