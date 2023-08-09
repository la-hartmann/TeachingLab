library(googlesheets4)
library(reticulate)
library(tidyverse)

import("requests")

reticulate::source_python(here::here("data_scripts/monday.com/fy24_project_pull.py"))

initial_df <- jsonlite::fromJSON(here::here("data/monday/fy24_project.json"))

data_with_courses <- initial_df$data$boards$groups[[1]] |>
  # dplyr::mutate(items = ifelse(is.null(unlist(items)), " ", items)) |>
  # dplyr::filter(str_detect(title, "Blended")) |>
  # view()
  unnest(items) |>
  rename(partner = title, course = name)

data_without_courses <- initial_df$data$boards$groups[[1]] |>
  dplyr::mutate(check = purrr::map(items, ~ is.null(unlist(.x)))) |>
  dplyr::filter(check == TRUE) |>
  dplyr::mutate(course = " ") |>
  dplyr::rename(partner = title) |>
  select(partner, course)

final_df <- data_with_courses |>
  bind_rows(data_without_courses) |>
  arrange(partner) |>
  relocate(partner, .before = 1)


# if (ncol(final_df) <= 26) {
#   end_letter <- LETTERS[ncol(final_df)]
# } else {
#   end_letter <- paste0("A", LETTERS[ncol(final_df) - 26])
# }
end_number <- nrow(final_df) + 1

range_write(range = glue::glue("A2:B{end_number}"),
            data = final_df,
            ss = "https://docs.google.com/spreadsheets/d/11jlo9UeWxZGwunhDb24hZBwAKc5b8ZKM9AYNWZaUyZY/",
            sheet = "FY24 Overview Board Automation",
            col_names = FALSE,
            reformat = FALSE)
