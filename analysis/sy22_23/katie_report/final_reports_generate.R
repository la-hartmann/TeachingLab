library(rmarkdown)

rmarkdown::render(
  input = here::here("analysis/sy22_23/KatieReport/katie_report_q1.Rmd"),
  output_file = "katie_report_q1",
  output_dir = here::here("analysis/sy22_23/KatieReport/Reports")
)
