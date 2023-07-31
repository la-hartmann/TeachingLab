library(MondayR)
library(tidyverse)
library(googlesheets4)
library(reticulate)
# monday_auth()
# 
# # get_all_fac_board_values <- function(board_id) {
# ### Get all columns of data ###
# columns <- monday_query("{boards(ids: 2548765319) { owner{ id } columns { title id }} }")
# 
# item_ids <- monday_query("{boards(ids: 2548765319) { owner{ id } items { id }} }") |>
#   unlist() |>
#   unique()
# 
# ### Get rid of first and last items since they aren't applicable ids (see column data.frame), it is data.boards.id and account_id ###
# item_ids <- item_ids[-1]
# item_ids <- item_ids[-length(item_ids)]
# 
# titles <- columns |>
#   as.data.frame() |>
#   select(contains("title")) |>
#   as.character()
# 
# column_ids <- columns |>
#   as.data.frame() |>
#   select(contains("columns.id")) |>
#   as.character()
# 
# # column_ids <- column_ids[-1]
# 
# get_column_values <- function(item_ids, id) {
#   
#   query_value <- MondayR::monday_query(paste0('{ items(ids: ', paste0("[", paste0(item_ids, collapse = ", "), "]"), ') { id column_values(ids: "', id, '") { value text } } }'))
#   
#   # print(query_value)
#   
#   ### Create empty value vector ###
#   value <- c()
#   ### Create a fake item id length for later ###
#   fake_length <- length(item_ids)
#   
#   ### If the length of item_ids is greater than 25, paginate by requesting 25 new responses as long as there are ### 
#   ### at least 25 responses ###
#   
#   if (length(item_ids) > 25) {
#     
#     while(length(item_ids) > 0) {
#       
#       if (length(item_ids) > 25) {
#         map_length <- 25
#       } else {
#         map_length <- length(item_ids)
#       }
#       
#       value <- append(value, as.character(purrr::map(1:map_length, ~ query_value$data$items[[.x]]$column_values[[1]]$text)))
#       item_ids <- item_ids[-c(1:map_length)]
#       
#     }
#     ### Otherwise just loop over length of item ids and get the item values per column 1
#   } else {
#     value <- purrr::map_chr(1:length(item_ids), ~ query_value$data$items[[.x]]$column_values[[1]]$text)
#   }
#   
#   ### If value doesn't return anything make a fake vector of empties to return ###
#   if (length(value) == 0) {
#     
#     cat(paste0("No data returned for column ", id))
#     
#     value <- c(rep("", 128))
#   }
#   
#   ### Always replace NULLs with empty characters ###
#   value <- str_replace_all(value, "NULL", "")
#   
#   return(value)
#   
# }
# 
# ### Get data frame of item ids by column ###
# entire_board_crossing <- crossing(col_id = column_ids, item_id = item_ids) |>
#   group_by(col_id) |>
#   arrange(match(col_id, column_ids), item_id) |>
#   # arrange(match(item_id, item_ids), .by_group = T) |>
#   # arrange() |>
#   ungroup() |>
#   group_by(item_id) |>
#   mutate(group = row_number()) |>
#   ungroup() |>
#   view()
# 
# ### Iterate columns by number in unique + 1 ###
# iterate_cols <- seq(1, nrow(entire_board_crossing), by = nrow(entire_board_crossing)/length(unique(column_ids)))
# ### Get items by dividing length of board by length of unique columns ###
# iterate_items <- seq(nrow(entire_board_crossing)/length(unique(column_ids)), nrow(entire_board_crossing), 
#                      by = nrow(entire_board_crossing)/length(unique(column_ids)))
# 
# 
# ### Iterate over items and columns to get data frame ###
# final_df <- map2_dfc(iterate_items[2:3], iterate_cols[2:3], ~ {Sys.sleep(10);get_column_values(item_ids = entire_board_crossing$item_id[.y:.x], 
#                                                           id = entire_board_crossing$col_id[.y])}) |>
#   suppressMessages() |>
#   set_names(titles[2:3]) |>
#   print()
# 
# rate <- rate_delay(10, max_times = 5)
# 
# get_column_values(item_ids = entire_board_crossing$item_id[129:257], entire_board_crossing$col_id[129])

# path_to_python <- paste0(here::here(), "/data_scripts/monday.com/env")
# use_virtualenv(path_to_python)
import("requests")
reticulate::source_python(here::here("data_scripts/monday.com/fy22_pipeline_pull.py"))

initial_df <- jsonlite::fromJSON(here::here("data/monday/fy22_pipeline.json"))

second_df <- initial_df$data$boards$items[[1]]$column_values |>
  as.data.frame()

first_column <- initial_df$data$boards$items |>
  as.data.frame() |>
  select(name) |>
  rename(`Open/Active` = name)

final_df <- second_df |>
  select(title, contains("text")) |>
  pivot_longer(!title) |>
  pivot_wider(names_from = "title", values_from = "value") |>
  select(-name) |>
  bind_cols(first_column) |>
  relocate(`Open/Active`, .before = 1) |>
  mutate(across(c("Projected Amt (no passthru)",
                  "Passthru Amt",
                  "Probability (%)"), ~ as.numeric(.x)),
         across(c("Status date changed",
                  "Proposal sent date"), ~ as.Date(.x)),
         `Weighted Amt` = (`Projected Amt (no passthru)` * `Probability (%)`) / 100,
         `Projected+Passthru` = `Projected Amt (no passthru)` + ifelse(!is.na(`Passthru Amt`), `Passthru Amt`, 0))

### IF there are more than 26 columns get the range to write by concatenating LETTERS subtracted from 26
### This will ONLY work so long as it doesn't get past the "A(X)" range of google sheets letters which I don't
### foresee happening
if (ncol(final_df) <= 26) {
  end_letter <- LETTERS[ncol(final_df)]
} else {
  end_letter <- paste0("A", LETTERS[ncol(final_df) - 26])
}
end_number <- nrow(final_df) + 1

range_write(range = glue::glue("A1:{end_letter}{end_number}"),
            data = final_df,
            ss = "https://docs.google.com/spreadsheets/d/1enrxtH-Cpn9OJO4E65-v-Sx2x5KL_03q6GLkVP3erig/edit#gid=576627549",
            sheet = 1,
            col_names = TRUE,
            reformat = FALSE)

