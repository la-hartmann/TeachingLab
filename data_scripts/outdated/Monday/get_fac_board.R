library(qualtRics)
library(reticulate)
library(TeachingLab)
library(tidyverse)

# get_all_fac_board_values <- function(board_id) {
### Get all columns of data ###
# columns <- monday_query("{boards(ids: 2208860812) { owner{ id } columns { title id }} }")
# ### Get rowwise item ids ###
# item_ids <- monday_query("{boards(ids: 2208860812) { owner{ id } items { id }} }") %>%
#   unlist() %>%
#   unique()
# ### Get rid of first and last items since they arent right ###
# item_ids <- item_ids[-1]
# item_ids <- item_ids[-length(item_ids)]
# 
# ### Get titles from columns query ###
# titles <- columns %>%
#   as.data.frame() %>%
#   select(contains("title")) %>%
#   as.character()
# 
# ### Get column ids from columns query ###
# column_ids <- columns %>%
#   as.data.frame() %>%
#   select(contains("columns.id")) %>%
#   as.character()
# 
# get_column_values <- function(item_ids, id) {
#   value <- monday_query(paste0('{
# items(ids: ', item_ids, ') {
#   id
#   column_values(ids: "', id, '") {
#     value
#     text
#   }
# }
# }'))$data$items[[1]]$column_values[[1]]$text
#   return(value)
# }
# 
# first_names <- map(item_ids, ~ get_column_values(item_ids = .x, id = "text44")) %>%
#   as.character()
# 
# last_names <- map(item_ids, ~ get_column_values(item_ids = .x, id = "dup__of_first_name")) %>%
#   as.character()
# 
# # emails <- map(item_ids, ~ get_column_values(item_ids = .x, id = "email")) %>%
# #   as.character()
# 
# status <- map(item_ids, ~ get_column_values(item_ids = .x, id = "status2")) %>%
#   as.character()
# 
# curriculum <- map(item_ids, ~ get_column_values(item_ids = .x, id = "dropdown0")) %>%
#   as.character()
# 
# df <- data.frame(Facilitators = paste0(first_names, " ", last_names),
#            status = status,
#            curriculum = curriculum)
# 
# final_df <- df %>%
#   filter(status == "Done") %>%
#   select(Facilitators, curriculum) %>%
#   separate(curriculum, sep = ", ", into = c("1", "2", "3", "4")) %>%
#   pivot_longer(!Facilitators) %>%
#   drop_na(value) %>%
#   mutate(new_value = 1) %>%
#   pivot_wider(names_from = value, values_from = new_value) %>%
#   select(-name, -`NULL`) %>%
#   dplyr::group_by(Facilitators) %>%
#   dplyr::summarise_all(TeachingLab::coalesce_by_column) %>%
#   dplyr::mutate(across(!c(Facilitators), ~ replace_na(.x, 0))) %>%
#   dplyr::rename(`K-2` = K2) %>%
#   suppressWarnings()
# 
# # return(final_df)
# 
# 
# # idk <- c("CKLA",
# #          "EL",
# #          "Engage/Eureka",
# #          "Guidebooks",
# #          "IM",
# #          "K-2",
# #          "Zearn")
# # }
# 
# # fac_board <- get_all_fac_board_values()
# 
# ## GET OLD FACILITATOR NAMES AND EMAILS ##
# old_facilitator_names_emails_list <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1nqtFK9_HBMBU6XQwCmmPYkhwBXZUj8wmevF2WGYUlaM/edit?ts=5f5a8a55#gid=1933413518",
#                                                 sheet = "Facilitators",
#                                                 range = "D:O") %>%
#   # mutate(Zearn = case_when(Zearn == FALSE ~ 0,
#   #                          Zearn == TRUE ~ 1)) %>%
#   dplyr::rename(Facilitators = 1, Emails = 2) %>%
#   tidyr::drop_na(`Emails`) %>%
#   tidyr::drop_na(Facilitators) %>%
#   dplyr::filter(Facilitators %in% final_df$Facilitators) %>%
#   # dplyr::mutate(dplyr::across(where(is.list), ~ as.double(.x))) %>%
#   # dplyr::rename(Engage = `Engage/Eureka`) %>%
#   dplyr::select(Facilitators, Emails)
# 
# ## Join together after filtering for just current employees from Monday.com list ##
# fac_board <- final_df %>%
#   dplyr::left_join(old_facilitator_names_emails_list) %>%
#   dplyr::mutate(Emails = ifelse(is.na(Emails),
#                                       paste0(stringr::str_replace_all(
#                                         stringr::str_to_lower(Facilitators), " ", "\\."),
#                                         "@teachinglab.org"),
#                                       Emails)) %>%
#   dplyr::relocate(Emails, .after = Facilitators)

get_monday_board <- function(board_id, first_col_name) {
  ### Set up python environment ###
  # path_to_python <- paste0(here::here(), "/data_scripts/monday.com/env")
  # my_env <- reticulate::use_virtualenv(path_to_python)
  # reticulate::import("requests")
  # reticulate::import("os")
  # my_env$boardId <- board_id
  ### ADD TO ENVIRONMENT FOR PYTHON SCRIPT HERE SOMEHOW
  # reticulate::py_run_string("os.environ['boardId'] = 'board_id'")
  ### Run python script to get monday json ###
  reticulate::source_python(here::here("data_scripts/monday.com/monday_facilitators.py"))
  
  ### Read in Monday JSON ###
  initial_df <- jsonlite::fromJSON(here::here("data/monday/monday_facilitators.json"))
  
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
    dplyr::select(title, contains("text")) |>
    tidyr::pivot_longer(!title) |>
    tidyr::pivot_wider(names_from = "title", values_from = "value") |>
    dplyr::select(-name) |>
    dplyr::bind_cols(first_column) |>
    dplyr::relocate({{ first_col_name }}, .before = 1)
}

### Grab Facilitator Monday Board ###
fac_board <- TeachingLab::get_monday_board(board_id = 2208860812, first_col_name = "Facilitator")

### Filter down to any not returning and just get facilitator and role ###
signed_by_role <- fac_board |>
  dplyr::filter(`FY23 Contract` != "Not Returning") |>
  dplyr::select(Facilitator, Role) |>
  tidyr::separate_rows(Role, sep = ", ") |>
  dplyr::arrange(Facilitator)

### Get Qualtrics Survey to Compare ###
participant_feedback <- qualtRics::fetch_survey("SV_djt8w6zgigaNq0C")

### Get all ela facilitators not in current list of all facilitators of participant feedback survey ###
setdiff(c(signed_by_role$Facilitator[signed_by_role$Role == "ELA"]), levels(participant_feedback$Q7)) |>
  view()

ela_facs <- signed_by_role |>
  dplyr::filter(Role == "ELA") |>
  dplyr::distinct(Facilitator) |>
  dplyr::arrange(Facilitator) |>
  clipr::write_clip() |>
  view()

setdiff(c(signed_by_role$Facilitator[signed_by_role$Role == "Math"]), levels(participant_feedback$Q7)) |>
  view()

math_facs <- signed_by_role |>
  dplyr::filter(Role == "Math") |>
  dplyr::distinct(Facilitator) |>
  dplyr::arrange(Facilitator) |>
  clipr::write_clip() |>
  view()

setdiff(c(signed_by_role$Facilitator[signed_by_role$Role == "State"]), levels(participant_feedback$Q7)) |>
  view()

state_facs <- signed_by_role |>
  dplyr::filter(Role == "State") |>
  dplyr::distinct(Facilitator) |>
  dplyr::arrange(Facilitator) |>
  clipr::write_clip() |>
  view()

# fac_board_reformat_1 <- fac_board |>
#   dplyr::filter(`FY23 Status` == "Returning") |>
#   dplyr::select(Facilitators = {{ first_col_name }}, Curriculum) |>
#   dplyr::mutate(Emails = paste0(tolower(str_replace_all(Facilitators, " ", ".")), "@teachinglab.org"),
#                 value = 1) |>
#   tidyr::drop_na(Curriculum) |>
#   tidyr::separate_rows(Curriculum) |>
#   tidyr::pivot_wider(names_from = Curriculum,
#                      names_sep = ", ",
#                      values_fill = list(Curriculum = 0)) |>
#   dplyr::mutate(dplyr::across(dplyr::everything(), ~ ifelse(is.na(.x), 0, .x)))
# 
# readr::write_rds(fac_board_reformat_1, "data/miscellaneous/facilitators_role.rds")
# readr::write_rds(fac_board_reformat_1, "dashboards/Staffing/data/facilitators_role.rds")
# 
# fac_board_reformat_2 <- fac_board |>
#   dplyr::filter(`FY23 Status` == "Returning") |>
#   dplyr::select(Facilitators = {{ first_col_name }}, Role) |>
#   dplyr::mutate(Emails = paste0(tolower(str_replace_all(Facilitators, " ", ".")), "@teachinglab.org"),
#                 value = 1) |>
#   tidyr::drop_na(Role) |>
#   tidyr::separate_rows(Role) |>
#   tidyr::pivot_wider(names_from = Role,
#                      names_sep = ", ",
#                      values_from = value,
#                      values_fill = list(Role = 0)) |>
#   dplyr::mutate(dplyr::across(dplyr::everything(), ~ ifelse(is.na(.x), 0, .x)))
# 
# fac_board_reformat_2 |>
#   filter(Facilitators %!in% old_list_fac) |>
#   # filter(ELA == 1) |>
#   pull(Facilitators) |>
#   sort() |>
#   clipr::write_clip()


