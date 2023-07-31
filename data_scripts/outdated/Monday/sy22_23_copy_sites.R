library(dplyr)
library(googlesheets4)
library(reticulate)
library(tidyr)

# path_to_python <- paste0(here::here(), "/data_scripts/monday.com/env")
# use_virtualenv(path_to_python)
reticulate::import("requests")
reticulate::source_python(here::here("data_scripts/monday.com/sy22_23_project.py"))

initial_df <- jsonlite::fromJSON(here::here("data/monday/sy22_23_project.json"))

second_df <- initial_df$data$boards$items[[1]]$column_values %>%
  as.data.frame()

first_column <- initial_df$data$boards$items |>
  as.data.frame() |>
  dplyr::select(name) |>
  dplyr::rename(Sites = name)

final_df <- second_df |>
  dplyr::select(title, contains("text")) |>
  tidyr::pivot_longer(!title) |>
  tidyr::pivot_wider(names_from = "title", values_from = "value") |>
  dplyr::select(-name) |>
  dplyr::bind_cols(first_column) |>
  dplyr::relocate(Sites, .before = 1)

### IF there are more than 26 columns get the range to write by concatenating LETTERS subtracted from 26
### This will ONLY work so long as it doesn't get past the "A(X)" range of google sheets letters which I don't
### foresee happening
if (ncol(final_df) <= 26) {
  end_letter <- LETTERS[ncol(final_df)]
} else {
  end_letter <- paste0("A", LETTERS[ncol(final_df) - 26])
}
end_number <- nrow(final_df) + 1

googlesheets4::range_write(
  range = glue::glue("A1:{end_letter}{end_number}"),
  data = final_df,
  ss = "https://docs.google.com/spreadsheets/d/11jlo9UeWxZGwunhDb24hZBwAKc5b8ZKM9AYNWZaUyZY/edit#gid=538640034",
  sheet = "DG monday.com AUTOMATION",
  col_names = TRUE,
  reformat = FALSE
)
