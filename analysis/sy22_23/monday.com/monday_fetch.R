# @title Get Monday Board Data
# @description Gets monday board data as a tibble
# @param board_id Which monday board id to use when getting information
# @param first_col_name What to name the first column in the dataframe
# @param py_script the location of the python script
# @param json_file the location of the json file of monday data
# @examples
# \dontrun{
# TeachingLab::get_monday_board(board_id = 2208860812, first_col_name = "Facilitator")
# }
# @return Returns a tibble
# get_monday_board <- function(board_id, first_col_name, py_script = "data_scripts/monday.com/monday_board.py", json_file = "data/monday/monday_board.json") {
# 
#   ### Run python script and add board id to environment ###
#   reticulate::py_run_string(code = paste0("boardId = '", board_id, "'"))
#   reticulate::source_python(py_script)
#   
#   ### Read in Monday JSON ###
#   initial_df <- jsonlite::fromJSON(json_file)
#   
#   ### Get as a data.frame ###
#   second_df <- initial_df$data$boards$items[[1]]$column_values |>
#     as.data.frame()
#   
#   ### Get first column separately ###
#   first_column <- initial_df$data$boards$items |>
#     as.data.frame() |>
#     dplyr::select(name) |>
#     dplyr::rename({{ first_col_name }} := name)
#   
#   ### Compose data.frame ###
#   final_df <- second_df |>
#     dplyr::select(title, dplyr::contains("text")) |>
#     tidyr::pivot_longer(!title) |>
#     tidyr::pivot_wider(names_from = "title", values_from = "value") |>
#     dplyr::select(-name) |>
#     dplyr::bind_cols(first_column) |>
#     dplyr::relocate({{ first_col_name }}, .before = 1)
#   
#   return(final_df)
#   
# }
