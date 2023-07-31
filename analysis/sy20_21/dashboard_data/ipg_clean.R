library(googlesheets4)
library(tidyverse)
library(TeachingLab)
library(gt)
library(ggtext)

ipg_forms_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1L33wVpPERyUQdG8WO3sZiyjnHzPvDL91O4yVUQTN14A/edit#gid=1455024681")

ipg_forms_raw %>%
  mutate(day = lubridate::date(Timestamp)) %>%
  group_by(day) %>%
  summarise(n = n()) %>%
  drop_na(n) %>%
  ggplot() +
  geom_col(aes(x = day, y = n)) +
  geom_point(aes(x = day, y = n), color = "#04abeb", size = 5, alpha = 0.75) +
  labs(title = "Weekly Count of Responses", x = "", y = "") +
  theme_tl()

ipg_forms_date <- ipg_forms_raw %>%
  mutate(day = lubridate::date(Timestamp),
         `Grade Level / Content Area` = ifelse(`Grade Level / Content Area` != "Kindergarten" & !is.na(parse_number(`Grade Level / Content Area`)),
                                               readr::parse_number(`Grade Level / Content Area`),
                                               `Grade Level / Content Area`)) %>%
  janitor::remove_empty(which = "cols") %>%
  mutate(across(!c("day"), ~ as.character(.x))) %>%
  tidyr::pivot_longer(cols = -c(Timestamp,
                                `Timeline of Obs`,
                                `District round`,
                                `Date of Observation`,
                                `Name of Site (Parish, District, Network)`,
                                `Name of School`,
                                `Teacher name`,
                                `Grade Level / Content Area`,
                                `IPG Rubric`,
                                day),
                      names_to = "Question",
                      values_to = "Answer") %>%
  mutate(Question = str_remove_all(Question, "\\.\\.(.*)")) %>%
  drop_na(Answer) %>%
  filter(!is.null(Answer) & Answer != "NULL") %>%
  pivot_wider(names_from = Question,
              values_from = Answer)

readr::write_rds(ipg_forms_date, here::here("dashboards/IPGBoard/data/ipg_data.rds"))
