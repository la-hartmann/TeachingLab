library(dplyr)
library(purrr)
library(qualtRics)
library(tidyr)

participant_feedback <- qualtRics::fetch_survey(surveyID = "SV_djt8w6zgigaNq0C", 
                                     verbose = TRUE,
                                     force_request = TRUE,
                                     include_display_order = FALSE)

tx_raise_feedback <- participant_feedback |>
  dplyr::filter(site == "TX_RAISE Rice University") |>
  dplyr::select(date = RecordedDate,
         facilitator1,
         end_of_session_tx_1, 
         end_of_session_tx_2, 
         end_of_session_tx_3, 
         end_of_session_tx_4) |>
  tidyr::drop_na(end_of_session_tx_4)

sheet_length <- nrow(tx_raise_feedback) + 1

tx_raise_feedback |>
  purrr::set_names(c("Date",
              "Facilitator",
              "The activities were well-designed to help me meet the learning targets.",
              "The strategies I’ve learned in this session will improve my instruction.",
              "I have applied or will apply what I have learned in this session to my practice.",
              "This session has supported me in being responsive to students' backgrounds, cultures, and points of view.")) |>
  googlesheets4::range_write(ss = "https://docs.google.com/spreadsheets/d/1nic0uhJt3Wi7Df5Romc2GilBj3cFGKALkxPVVJ-i5GE/edit#gid=393168439",
                             sheet = "data",
                             col_names = F,
                             range = glue::glue("A2:F{sheet_length}"))
