library(googlesheets4)
library(TeachingLab)
library(tidyverse)

all_sites <- c(
  "All Sites",
  "AR_Arkansas DOE",
  "CA_San Diego",
  "CA_Silver Giving",
  "CA_West Contra Costa_Murphy ES",
  "CA_West Contra Costa_WCCUSD",
  "DE_DE Department of Education",
  "IL_Chicago Public Schools_Aggregate",
  "IL_Chicago Public Schools_Network 12",
  # "IL_Chicago Public Schools_Network 4",
  "IL_Chicago Public Schools_Network 7",
  "IL_Kankakee District 111",
  "LA_Calcasieu Parish",
  "LA_Jefferson Davis Parish",
  "LA_Pointe Coupee Parish",
  "NM_NM Public Education Department",
  "NY_Amistad Dual Language",
  "NY_Channel View School for Research",
  "NY_D9",
  "NY_D10_10X386",
  "NY_D27",
  "NY_ESMT_IS190",
  "NY_Fannie Lou Hamer",
  "OH_Cleveland Metro School District",
  "TN_McNairy County Schools",
  "TX_Essence Preparatory Public School",
  "TX_RAISE Rice University",
  "US_City Year",
  "US_Digital Nest",
  "US_Open Enrollment",
  "VT_Danville School",
  "VT_Orleans Central Supervisory Union",
  "WI_WI DPI"
)

eic_sites <- c(
  "NY_D11",
  "NY_Rochester City Schools"
)

eic_sites_specific <- "NY_D11 Math"

d27_sites <- c("NY_D27_27x306", "NY_D27_27x104")

d11_sites <- c("NY_D11_11x076")

### Generate Reports ###
### All ###
purrr::walk(
  "All Sites",
  ~ TeachingLab:::partner_file_remove(
    partner = .x,
    input = here::here("analysis/sy22_23/end_year_report/end_year_report_22_23.Rmd"),
    output_dir = here::here("analysis/sy22_23/end_year_report/partner_reports"),
    matched = "True",
    content_area = "All Content Areas"
  )
)

### EIC Reports ###
purrr::walk(
  "NY_Rochester City Schools",
  ~ TeachingLab:::partner_file_remove(
    partner = .x,
    input = here::here("analysis/sy22_23/end_year_report/end_year_report_22_23_eic.Rmd"),
    output_dir = here::here("analysis/sy22_23/end_year_report/partner_reports")
  )
)

### District 27 ###
purrr::walk2(
  c("NY_D27", "NY_D27"),
  d27_sites,
  ~ TeachingLab:::partner_file_remove(
    partner = .x,
    d27 = .y,
    input = here::here("analysis/sy22_23/end_year_report/end_year_report_22_23.Rmd"),
    output_dir = here::here("analysis/sy22_23/end_year_report/partner_reports")
  )
)

### District 27 ###
purrr::walk2(
  c("NY_D11"),
  d11_sites,
  ~ TeachingLab:::partner_file_remove(
    partner = .x,
    d27 = .y,
    input = here::here("analysis/sy22_23/end_year_report/end_year_report_22_23.Rmd"),
    output_dir = here::here("analysis/sy22_23/end_year_report/partner_reports")
  )
)

### District 11 Math ###
purrr::walk(
  eic_sites_specific,
  ~ TeachingLab:::partner_file_remove(
    partner = stringr::str_remove(.x, " Math"),
    input = here::here("analysis/sy22_23/end_year_report/end_year_report_22_23_eic.Rmd"),
    output_dir = NULL,
    output_file = here::here("analysis/sy22_23/end_year_report/partner_reports/final_report_ny_d11_math.html"),
    content_area = stringr::str_remove(.x, "NY_D11 ")
  )
)

### Check off reports compiled in sheets ###
googlesheets4::range_write(
  ss = "1wDDkKvppnKJfjfDkCDWyIIj-I7m7u6l8RTUYSw4XcvE",
  sheet = "Partners 2022-2023",
  data = tibble::tibble(
    site = sort(c(all_sites, eic_sites, eic_sites_specific)),
    update = "✅",
    date = Sys.Date()
  ),
  range = glue::glue("A2:C{length(sort(c(all_sites, eic_sites, eic_sites_specific))) + 1}"),
  reformat = FALSE,
  col_names = FALSE
)

### Copy to Website ###
file.copy(
  from = list.files(here::here("analysis/sy22_23/end_year_report/partner_reports"), full.names = TRUE, "\\.html"),
  to = "~/Teaching Lab/Coding/teachinglab.github.io/Reports/2023Reports/EndYear",
  overwrite = TRUE
)
