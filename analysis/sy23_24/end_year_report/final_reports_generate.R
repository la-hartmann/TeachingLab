library(googlesheets4)
library(TeachingLab)
library(tidyverse)

site <- c(
  "WI_WI DPI"
)

course <- c(
  "Culturally Responsive Leadership (CRL) Series"
)

devtools::load_all()
### Generate Reports ###
purrr::walk2(
  site,
  course,
  ~ TeachingLab:::partner_file_remove(
    partner = .x,
    course = .y,
    input = here::here("analysis/sy23_24/end_year_report/course_end_year_report_23_24.Rmd"),
    output_dir = here::here("analysis/sy23_24/end_year_report/reports")
  ),
  envir = globalenv()
)

### Check off reports compiled in sheets ###
# googlesheets4::range_write(
#   ss = "1wDDkKvppnKJfjfDkCDWyIIj-I7m7u6l8RTUYSw4XcvE",
#   sheet = "Partners 2022-2023",
#   data = tibble::tibble(
#     site = sort(c(all_sites, eic_sites, eic_sites_specific)),
#     update = "✅",
#     date = Sys.Date()
#   ),
#   range = glue::glue("A2:C{length(sort(c(all_sites, eic_sites, eic_sites_specific))) + 1}"),
#   reformat = FALSE,
#   col_names = FALSE
# )
# 
# ### Copy to Website ###
# file.copy(
#   from = list.files(here::here("analysis/sy22_23/end_year_report/partner_reports"), full.names = TRUE, "\\.html"),
#   to = "~/Teaching Lab/Coding/teachinglab.github.io/Reports/2023Reports/EndYear",
#   overwrite = TRUE
# )
