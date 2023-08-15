library(googlesheets4)
library(reticulate)
library(tidyverse)

import("requests")

reticulate::source_python(here::here("data_scripts/monday.com/monday_facilitators.py"))

initial_df <- jsonlite::fromJSON(here::here("data/monday/monday_facilitators.json"))

fac_coach_data <- initial_df$data$boards$items[[1]] |>
  view()

fac_coach_data$column_values[[1]]


get_monday_board <- function(board_id, first_col_name, py_script = "data_scripts/monday.com/monday_board.py", json_file = "data/monday/monday_board.json") {
  
  ### Run python script and add board id to environment ###
  reticulate::py_run_string(code = paste0("boardId = '", board_id, "'"))
  reticulate::source_python(py_script)
  
  ### Read in Monday JSON ###
  initial_df <- jsonlite::fromJSON(json_file)
  
  ### Get as a data.frame ###
  second_df <- initial_df$data$boards$items[[1]]$column_values |>
    as.data.frame()
  
  ### Get first column separately ###
  first_column <- initial_df$data$boards$items |>
    as.data.frame() |>
    dplyr::select(name) |>
    dplyr::rename({{ first_col_name }} := name)
  
  ### Compose data.frame ###
  final_df <- second_df |>
    dplyr::select(title, dplyr::contains("text")) |>
    tidyr::pivot_longer(!title) |>
    tidyr::pivot_wider(names_from = "title", values_from = "value") |>
    dplyr::select(-name) |>
    dplyr::bind_cols(first_column) |>
    dplyr::relocate({{ first_col_name }}, .before = 1)
  
  return(final_df)
  
}

fac_board <- get_monday_board(board_id = 2208860812, first_col_name = "Facilitator")

fac_board |>
  filter(`Coach Onboarding` %in% c("Done", "Full Time")) |>
  pull(Facilitator) |>
  sort() |>
  clipr::write_clip()
