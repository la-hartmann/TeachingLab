library(qualtRics)
library(tidyverse)

participant_feedback <- fetch_survey(surveyID = "SV_djt8w6zgigaNq0C", 
                                     verbose = TRUE,
                                     force_request = FALSE)

### Just first facilitator reviews ###
facilitator_one_reviews <- participant_feedback |>
  dplyr::filter(!is.na(fac_feedback_1) & Finished == TRUE) |>
  dplyr::select(Facilitator = facilitator1, 
                `They demonstrated deep knowledge of the content they facilitated` = fac_feedback_1, 
                `They facilitated the content clearly` = fac_feedback_2, 
                `They effectively built a safe learning community` = fac_feedback_3, 
                `They were fully prepared for the session` = fac_feedback_4, 
                `They responded to the group’s needs` = fac_feedback_5,
                # `Additional feedback` = Q9,
                # `What is one thing from today's learning that you plan to take back to your classroom?` = Q14,
                # `What went well in today’s session?` = Q15,
                `What could have been better about today’s session?` = been_better_today,
                Course = course,
                Site = site,
                Date = RecordedDate,
                `Content Area` = content_area)


### Just second facilitator reviews ###
facilitator_two_reviews <- participant_feedback |>
  dplyr::filter(!is.na(fac_feedback_2_1) & Finished == TRUE) |>
  dplyr::select(Facilitator = facilitator2, 
                `They demonstrated deep knowledge of the content they facilitated` = fac_feedback_2_1, 
                `They facilitated the content clearly` = fac_feedback_2_2, 
                `They effectively built a safe learning community` = fac_feedback_2_3, 
                `They were fully prepared for the session` = fac_feedback_2_4, 
                `They responded to the group’s needs` = fac_feedback_2_5,
                # `Additional feedback` = Q13,
                # `What is one thing from today's learning that you plan to take back to your classroom?` = Q14,
                # `What went well in today’s session?` = Q15,
                `What could have been better about today’s session?` = been_better_today,
                Course = course,
                Site = site,
                Date = RecordedDate,
                `Content Area` = content_area)

pos <- function(x) {
  x <- x[!is.na(x)]
  (sum(str_detect(x, "4|5")) / (sum(str_detect(x, "1|2|3")) + sum(str_detect(x, "4|5"))))
}

n_count <- function(x) {
  sum(!is.na(x))
}

all_facilitator_reviews <- facilitator_one_reviews |>
  dplyr::bind_rows(facilitator_two_reviews) |>
  mutate(Facilitator = as.character(Facilitator)) |>
  filter(!is.na(Facilitator)) |>
  select(1:6) |>
  group_by(Facilitator) |>
  summarise(across(everything(), list(n = n_count, pos = pos)))

ave <- function(x) {
  mean(readr::parse_number(x))
}

all_facilitator_reviews2 <- facilitator_one_reviews |>
  dplyr::bind_rows(facilitator_two_reviews) |>
  mutate(Facilitator = as.character(Facilitator)) |>
  filter(!is.na(Facilitator)) |>
  select(1:6) |>
  group_by(Facilitator) |>
  summarise(across(everything(), list(n = n_count, ave = ave)))



googlesheets4::write_sheet(all_facilitator_reviews2,
                           ss = "1HJRGEatzi6hgA8-BmNt9sGBxqkbfT4lZ6Mn6Vb_1rQk",
                           sheet = "all_fac_reviews_average")
