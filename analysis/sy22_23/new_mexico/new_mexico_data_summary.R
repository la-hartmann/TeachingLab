library(qualtRics)
library(tidyverse)


new_mexico_ed_survey <- qualtRics::fetch_survey("SV_3a2OM4f9j85EuyO")
colmap_new_mexico_ed_survey <- tibble::tibble(
  colnames = colnames(new_mexico_ed_survey),
  questions = map_chr(new_mexico_ed_survey, ~ attr(.x, "label"))
)

student_survey <- qualtRics::fetch_survey("SV_9uze2faHuIf3vP8") |>
  filter(State == "New Mexico")
colmap_student_survey <- tibble::tibble(
  colnames = colnames(student_survey),
  questions = map_chr(student_survey, ~ attr(.x, "label"))
)

ipg_forms <- qualtRics::fetch_survey("SV_0BSnkV9TVXK1hjw") |>
  filter(Site == "NM_NM Public Education Department")
colmap_ipg_forms <- tibble::tibble(
  colnames = colnames(ipg_forms),
  questions = map_chr(ipg_forms, ~ attr(.x, "label"))
)


map(ls(), ~ write_csv(x = get(.x), file = paste0("~/Downloads/", .x, ".csv")))
