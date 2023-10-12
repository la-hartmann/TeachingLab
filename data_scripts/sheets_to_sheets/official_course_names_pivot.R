library(googlesheets4)
library(tidyverse)

courses_sites <- read_sheet("11jlo9UeWxZGwunhDb24hZBwAKc5b8ZKM9AYNWZaUyZY",
  sheet = "FY24 Official Course Names"
)

# subsite_selection <- read_sheet("11jlo9UeWxZGwunhDb24hZBwAKc5b8ZKM9AYNWZaUyZY",
#   sheet = "FY24 School/District Selection for Sites"
# )

courses_sites |>
  distinct(`Partner grouping from monday.com`, `Course Name Official`, .keep_all = TRUE) |>
  group_by(`Partner grouping from monday.com`) |>
  mutate(Course = paste0("Course ", row_number())) |>
  ungroup() |>
  pivot_wider(
    id_cols = `Partner grouping from monday.com`,
    values_from = `Course Name Official`,
    names_from = Course,
    values_fn = list
  ) |>
  mutate(across(everything(), as.character),
    across(everything(), ~ na_if(.x, "NULL")),
    Subsite = dplyr::case_when(
      str_detect(`Partner grouping from monday.com`, "MA_DESE") ~ str_extract(`Partner grouping from monday.com`, "(?<=MA_DESE\\s).*$"),
      # str_detect(`Partner grouping from monday.com`, "AR_") ~ str_extract(`Partner grouping from monday.com`, "(?<=AR_).*$"),
      # str_detect(`Partner grouping from monday.com`, "IL_CPS ") ~ str_extract(`Partner grouping from monday.com`, "(?<=IL_CPS\\s).*$"),
      str_detect(`Partner grouping from monday.com`, "NY_D6") ~ str_extract(`Partner grouping from monday.com`, "(?<=NY_D6_).*$"),
    ),
    `Partner grouping from monday.com` = str_remove_all(`Partner grouping from monday.com`, "(?<=MA_DESE).*|(?<=NY_D6).*")
  ) |>
  rename(Site = `Partner grouping from monday.com`) |>
  relocate(Subsite, .after = Site) -> courses_sites_subsite

sheet_range <- paste0("A1:", LETTERS[ncol(courses_sites_subsite)], nrow(courses_sites_subsite) + 1)

courses_sites_subsite |>
  range_write(
    ss = "11jlo9UeWxZGwunhDb24hZBwAKc5b8ZKM9AYNWZaUyZY",
    sheet = "New FY24 Attendance Dashboards Automation",
    range = sheet_range,
    col_names = TRUE,
    reformat = FALSE
  )
