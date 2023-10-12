library(qualtRics)
library(TeachingLab)
library(tidyverse)
devtools::load_all()

student_survey <- TeachingLab::get_student_survey(update = FALSE, year = "22_23")

eic_student_survey_selected <- student_survey |>
  filter(eic == TRUE) |>
  janitor::remove_empty("cols")

eic_renamed_a1_white <- eic_student_survey_selected |>
  mutate(race = ifelse(race == "White", "White", "BIPOC")) |>
  dplyr::filter(race == "White") |>
  select(contains("growth_mindsets_a1")) |>
  TeachingLab::relabel_qualtrics_df()

eic_renamed_a1_poc <- eic_student_survey_selected |>
  mutate(race = ifelse(race == "White", "White", "BIPOC")) |>
  dplyr::filter(race == "BIPOC") |>
  select(contains("growth_mindsets_a1")) |>
  TeachingLab::relabel_qualtrics_df()

TeachingLab::tl_likert(
  data = eic_renamed_a1_white,
  title = "How much do you agree or disagree with the statements below? (White)",
  string_remove = "How much do you agree or disagree with the statements below\\? - ",
  string_wrap = 25
)

TeachingLab::tl_likert(
  data = eic_renamed_a1_poc,
  title = "How much do you agree or disagree with the statements below? (POC)",
  string_remove = "How much do you agree or disagree with the statements below\\? - ",
  string_wrap = 25
)
