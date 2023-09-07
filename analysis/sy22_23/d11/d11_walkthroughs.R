library(dplyr)
library(googlesheets4)
library(qualtRics)

ipg_forms <- qualtRics::fetch_survey("SV_0BSnkV9TVXK1hjw")

ipg_forms |>
  select(d11_walkthrough_qs) |>
  write_sheet()