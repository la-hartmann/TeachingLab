library(googlesheets4)
library(TeachingLab)
library(tidyverse)

# educator_survey |>
#   filter(Finished == TRUE & !is.na(ma_dese)) |>
#   group_by(ma_dese) |>
#   count(sort = T) |>
#   ungroup() |>
#   gt::gt() |>
#   TeachingLab::gt_theme_tl()
# 
# sort(unique(educator_survey$ma_dese)) |> clipr::write_clip()

all_sites <- c(
  "All Sites",
  "ABRSD",
  "Agawam",
  "Berkshire Hills",
  "Boston Prep",
  "Dennis-Yarmouth",
  "Greenfield",
  "King Phillip",
  "Milford",
  "Milton",
  "Needham",
  "Northampton",
  "Plainville",
  "Uxbridge",
  "Wakefield",
  "West Springfield"
)

### Generate Reports ###
purrr::walk(
  all_sites[12],
  ~ TeachingLab:::partner_file_remove(
    partner = .x,
    input = here::here("analysis/sy22_23/ongoing_reports/ongoing_partner_reports.Rmd"),
    output_dir = here::here("analysis/sy22_23/ongoing_reports/partner_reports")
  )
)

### Copy to Website ###
file.copy(
  from = list.files(here::here("analysis/sy22_23/ongoing_reports/partner_reports"), full.names = TRUE, "\\.html"),
  to = "~/Teaching Lab/Coding/teachinglab.github.io/Reports/2023Reports/Ongoing",
  overwrite = TRUE
)
