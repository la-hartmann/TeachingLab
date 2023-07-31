library(googlesheets4)
library(TeachingLab)
library(tidyverse)

student_survey <- TeachingLab::get_student_survey(year = "22_23")

student_survey |>
  dplyr::filter(eic == TRUE) |>
  janitor::remove_empty("cols")