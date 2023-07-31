library(tidyverse)
devtools::load_all()
library(TeachingLab)
library(googledrive)

diagnostic <- get_diagnostic_survey()
knowledge_assessments <- get_knowledge_assessments()
session_survey <- get_session_survey()
course_survey <- get_course_survey()
coaching_participant_feedback <- get_ongoing_coaching()
ongoing_coaching_feedback <- get_coaching_feedback()
student_survey <- get_student_survey()
family_survey <- get_family_survey()

all_data <- mget(ls())

map(2:9, ~ readr::write_csv(all_data[[.x]], paste0(here::here("data/csv_save/"), names(all_data[.x]), ".csv")))

map(list.files(here::here("data/csv_save"), full.names = T), 
    ~ googledrive::drive_upload(media = .x, path = as_id("1l024KNL3Af3oXnWQkLU53VWJPtegqkw1")))
