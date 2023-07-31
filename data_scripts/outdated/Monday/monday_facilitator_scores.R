library(dplyr)
library(reticulate)
library(qualtRics)
library(TeachingLab)

### Get's facilitator board in current state ###
fac_board <- TeachingLab::get_monday_board(board_id = 2208860812, first_col_name = "Facilitator")

### Get's participant feedback ###
session_survey <- fetch_survey(surveyID = "SV_djt8w6zgigaNq0C", 
                               verbose = TRUE,
                               force_request = TRUE)

### Just first facilitator reviews ###
facilitator_one_reviews <- session_survey |>
  dplyr::filter(!is.na(Facilitator1)) |>
  dplyr::select(Facilitator = Facilitator1, 
                `They demonstrated deep knowledge of the content they facilitated` = Q8_1, 
                `They facilitated the content clearly` = Q8_2, 
                `They effectively built a safe learning community` = Q8_3, 
                `They were fully prepared for the session` = Q8_4, 
                `They responded to the group’s needs` = Q8_5) |>
  dplyr::mutate(dplyr::across(c(2:6), ~ readr::parse_number(.x))) |>
  dplyr::group_by(Facilitator) |>
  dplyr::summarise(dplyr::across(dplyr::everything(), ~ round(mean(.x, na.rm = T), 1)),
                   n = n()) |>
  na.omit()

### Just second facilitator reviews ###
facilitator_two_reviews <- session_survey |>
  dplyr::filter(!is.na(Facilitator2)) |>
  dplyr::select(Facilitator = Facilitator2, 
                `They demonstrated deep knowledge of the content they facilitated` = Q12_1, 
                `They facilitated the content clearly` = Q12_2, 
                `They effectively built a safe learning community` = Q12_3, 
                `They were fully prepared for the session` = Q12_4, 
                `They responded to the group’s needs` = Q12_5) |>
  dplyr::mutate(dplyr::across(c(2:6), ~ readr::parse_number(.x))) |>
  dplyr::group_by(Facilitator) |>
  dplyr::summarise(dplyr::across(dplyr::everything(), ~ round(mean(.x, na.rm = T), 1)),
                   n = n()) |>
  na.omit()

### All facilitator reviews by binding rows together ###
all_facilitator_reviews <- facilitator_one_reviews |>
  dplyr::bind_rows(facilitator_two_reviews) |>
  dplyr::mutate(Facilitator = as.character(Facilitator)) |>
  dplyr::filter(Facilitator != "Other" & Facilitator != "NA") |>
  dplyr::group_by(Facilitator) |>
  dplyr::summarise(dplyr::across(!n, ~ mean(.x)),
                   dplyr::across(n, ~ sum(.x))) |>
  dplyr::ungroup() |>
  tibble::view()

# reticulate::source_python(here::here("data_scripts/monday.com/monday_board_mutate.py"))

### Add code to make monday board sync with this ###

### Add back to score board with a left join ###
# fac_board_with_scores <- fac_board |>
#   dplyr::left_join(all_facilitator_reviews)

# fac_score_1 <- 

# reticulate::py_run_string(code = "facScore1 = ")
# reticulate::source_python(here::here("data_scripts/monday.com/monday_board.py"))

