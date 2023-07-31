library(rmarkdown)
library(tidyverse)

rmarkdown::render(
  input = here::here("analysis/sy20_21/SY20-21Report/FinalReport.rmd"),
  output_file = paste0("2021 Report_district11_unmatched"),
  output_dir = here::here("analysis/sy20_21/SY20-21Report/Reports"),
  params = list(partner = "District 11", matched = "unmatched")
)
