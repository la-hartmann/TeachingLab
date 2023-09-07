library(qualtRics)
library(TeachingLab)
library(tidyverse)

participant_feedback <- qualtRics::fetch_survey(surveyID = "SV_djt8w6zgigaNq0C", 
                                                verbose = TRUE,
                                                force_request = TRUE)

### Just first facilitator reviews ###
facilitator_one_reviews <- participant_feedback |>
  dplyr::filter(!is.na(Facilitator1) & Finished == TRUE) |>
  dplyr::select(Facilitator = Facilitator1, 
                `They demonstrated deep knowledge of the content they facilitated` = Q8_1, 
                `They facilitated the content clearly` = Q8_2, 
                `They effectively built a safe learning community` = Q8_3, 
                `They were fully prepared for the session` = Q8_4, 
                `They responded to the group’s needs` = Q8_5,
                `Additional feedback` = Q9,
                `What is one thing from today's learning that you plan to take back to your classroom?` = Q14,
                `What went well in today’s session?` = Q15,
                `What could have been better about today’s session?` = Q16,
                Course,
                Site,
                Date = RecordedDate,
                `Content Area` = `Content area`)


### Just second facilitator reviews ###
facilitator_two_reviews <- participant_feedback |>
  dplyr::filter(!is.na(Facilitator2) & Finished == TRUE) |>
  dplyr::select(Facilitator = Facilitator2, 
                `They demonstrated deep knowledge of the content they facilitated` = Q12_1, 
                `They facilitated the content clearly` = Q12_2, 
                `They effectively built a safe learning community` = Q12_3, 
                `They were fully prepared for the session` = Q12_4, 
                `They responded to the group’s needs` = Q12_5,
                `Additional feedback` = Q13,
                `What is one thing from today's learning that you plan to take back to your classroom?` = Q14,
                `What went well in today’s session?` = Q15,
                `What could have been better about today’s session?` = Q16,
                Course,
                Site,
                Date = EndDate,
                `Content Area` = `Content area`)


all_facilitator_reviews <- facilitator_one_reviews |>
  dplyr::bind_rows(facilitator_two_reviews) |>
  mutate(Facilitator = as.character(Facilitator)) |>
  filter(!is.na(Facilitator))

googlesheets4::range_write(data = all_facilitator_reviews, 
                           ss = "1TdZKoCOsMMXuaJo_Mn2l1iEtro7oidLKepxcvwiz6eE",
                           col_names = FALSE,
                           reformat = FALSE,
                           range = glue::glue("A2:N{nrow(all_facilitator_reviews) + 1}"))

split_fac_reviews <- split(all_facilitator_reviews, all_facilitator_reviews$Facilitator)

split_fac_reviews[[5]] |>
  googlesheets4::write_sheet("https://docs.google.com/spreadsheets/d/1Skoqtjv2gcm-tCnQu_qtWDd27JzevunCiV0F4jWs0Lo/edit#gid=2109625753")

split_fac_reviews[2:23] |>
  map( ~ googlesheets4::write_sheet(.x))
