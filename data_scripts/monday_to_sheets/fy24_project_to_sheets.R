library(googlesheets4)
library(reticulate)
library(tidyverse)

import("requests")

# reticulate::source_python(here::here("data_scripts/monday.com/fy24_project_pull.py"))
reticulate::source_python(here::here("data_scripts/monday.com/fy24_courses_pull.py"))
reticulate::source_python(here::here("data_scripts/monday.com/fy24_sites_pull.py"))
reticulate::source_python(here::here("data_scripts/monday.com/sites_courses_monday_final_actually_helpful.py"))

# initial_df <- jsonlite::fromJSON(here::here("data/monday/fy24_project.json"))
courses_df <- jsonlite::fromJSON(here::here("data/monday/fy24_courses.json"))
sites_df <- jsonlite::fromJSON(here::here("data/monday/fy24_sites.json"))
sites_and_courses_df <- jsonlite::fromJSON(here::here("data/monday/fy24_sites_courses.json"))


just_sites_data <- sites_df$data$boards$groups[[1]] |>
  unnest(items) |>
  unnest(column_values) |>
  relocate(title, .before = text)

sites_range <- paste0("A2", ":C", nrow(just_sites_data) + 1)

just_sites_data |>
  range_write(ss = "11jlo9UeWxZGwunhDb24hZBwAKc5b8ZKM9AYNWZaUyZY",
              sheet = "Monday.com sites (reference)",
              range = sites_range,
              col_names = FALSE,
              reformat = FALSE)

just_courses_data <- courses_df$data$boards$items[[1]] |>
  unnest(column_values) |>
  # dplyr::group_by(text, title, name) |>
  # dplyr::mutate(n = dplyr::n()) |>
  # dplyr::filter(n > 1L) |>
  # ungroup() |>
  # select(-n) |>
  distinct(text, title, name, .keep_all = TRUE) |>
  pivot_wider(id_cols = name, names_from = "title", values_from = "text") |>
  mutate(across(c(2, 3), ~ as.character(.x)),
         across(c(2, 3), ~ stringr::str_remove_all(.x, "c\\(\"|\"\\)"))) |>
  separate_rows(`Site Name in Survey`, `Course Name in Survey`, sep = "\",\\s*\"") |>
  relocate(2, .before = 1) |>
  arrange(`Site Name in Survey`)

courses_range <- paste0("A2", ":C", nrow(just_courses_data) + 1)

just_courses_data |>
  range_write(ss = "11jlo9UeWxZGwunhDb24hZBwAKc5b8ZKM9AYNWZaUyZY",
              sheet = "Monday.com Course Names (reference)",
              range = courses_range,
              col_names = FALSE,
              reformat = FALSE)

# data_with_courses <- initial_df$data$boards$groups[[1]] |>
#   # dplyr::mutate(items = ifelse(is.null(unlist(items)), " ", items)) |>
#   # dplyr::filter(str_detect(title, "Blended")) |>
#   # view()
#   unnest(items) |>
#   rename(partner = title, course = name)
# 
# data_without_courses <- initial_df$data$boards$groups[[1]] |>
#   dplyr::mutate(check = purrr::map(items, ~ is.null(unlist(.x)))) |>
#   dplyr::filter(check == TRUE) |>
#   dplyr::mutate(course = " ") |>
#   dplyr::rename(partner = title) |>
#   select(partner, course)
# 
# final_df <- data_with_courses |>
#   bind_rows(data_without_courses) |>
#   arrange(partner) |>
#   relocate(partner, .before = 1)
# 
# check_sheet <- read_sheet("11jlo9UeWxZGwunhDb24hZBwAKc5b8ZKM9AYNWZaUyZY",
#                           sheet = "FY24 Overview Board Automation",
#                           range = "A:D",
#                           col_names = TRUE)
# 
# final_df |>
#   filter(!course %in% check_sheet$Courses | !partner %in% check_sheet$Partner)

### Write anti join here, then write to overwrite the whole sheet after sorting ###


# if (ncol(final_df) <= 26) {
#   end_letter <- LETTERS[ncol(final_df)]
# } else {
#   end_letter <- paste0("A", LETTERS[ncol(final_df) - 26])
# }
# end_number <- nrow(final_df) + 1
# 
# range_write(range = glue::glue("A2:B{end_number}"),
#             data = final_df,
#             ss = "https://docs.google.com/spreadsheets/d/11jlo9UeWxZGwunhDb24hZBwAKc5b8ZKM9AYNWZaUyZY/",
#             sheet = "FY24 Overview Board Automation",
#             col_names = FALSE,
#             reformat = FALSE)


# check_sheet$`Partner name` |> unique() |> sort() |> clipr::write_clip()

# final_sites_and_courses <- sites_and_courses_df$data$boards$groups[[1]] |>
#   unnest(items) |>
#   unnest(column_values, names_sep = c("_2")) |>
#   pivot_wider(names_from = "column_values_2title", values_from = "column_values_2text") |>
#   mutate(across(c(2, 3), ~ str_remove_all(.x, "c\\(|\\)|"))) |>
#   suppressWarnings() |>
#   separate_rows(`Site Name in Survey`, `Course Name in Survey`, sep = ",") |>
#   mutate(across(c(2, 3), ~ str_remove_all(.x, "\""))) |>
#   distinct(title, `Site Name in Survey`, `Course Name in Survey`)

# final_sites_and_courses$`Site Name in Survey` |>
#   unique() |>
#   sort() |>
#   clipr::write_clip()

# courses <- read_sheet("https://docs.google.com/spreadsheets/d/11jlo9UeWxZGwunhDb24hZBwAKc5b8ZKM9AYNWZaUyZY/edit#gid=1263705407",
#            sheet = "Mock up course names")
# 
# courses |>
#   filter(Partner == "MA_DESE") |>
#   pull(`Course Name in Survey (new column in monday.com)`) |>
#   unique() |>
#   sort() |>
#   clipr::write_clip()