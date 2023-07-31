library(googlesheets4)
library(tidyverse)

cps_data <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1QqsTVWRSr5PUnA4WKfZoqhQZkCLUwVk2mi3-9JtkbHU/edit#gid=1659857917",
                                      sheet = 1)

pos_responses <- function(x) {
  
  x <- x[!is.na(x)]
  x <- x[!is.null(x)]
  x <- x[!str_detect(x, "Not Observed|Not observed|NULL")]
  
  x <- (sum(stringr::str_detect(x, "4|5"), na.rm = T)) /
    (sum(!stringr::str_detect(x, "4|5"), na.rm = T) + sum(str_detect(x, "4|5"), na.rm = T))
  
}

#### Overall by Question ####
cps_data |>
  dplyr::filter(`What type of Co-Lab did you participate in today?` == "Universal") |>
  dplyr::select(contains("How much do you agree with the following statements about this session?")) |>
  janitor::remove_empty("cols") |>
  # dplyr::select(-`How much do you agree with the following statements about this session? [This Co-Lab session helped me identify next steps to strengthen my use of Creative Curriculum to meet my students' needs.]`,
  #               -`How much do you agree with the following statements about this session? [This Co-Lab session provided me with the opportunity to actively explore the Creative Curriculum.]`) |>
  dplyr::summarise(across(everything(), ~ pos_responses(.x))) |>
  tidyr::pivot_longer(everything(), names_to = "Question", values_to = "% Positive Responses") |>
  dplyr::mutate(Question = stringr::str_remove_all(Question, "\\.\\.\\.[0-9]"),
                Question = stringr::str_remove_all(Question, "How much do you agree with the following statements about this session\\? \\[|[0-9]|\\]")) |>
  range_write(ss = "https://docs.google.com/spreadsheets/d/1QqsTVWRSr5PUnA4WKfZoqhQZkCLUwVk2mi3-9JtkbHU/edit#gid=1659857917",
              sheet = "Universal Co-Labs",
              range = "A20:B38",
              reformat = F)

#### By Content Area ###
content_area_count <- cps_data |>
  dplyr::mutate(`Content Area` = dplyr::coalesce(`Please select the content area for today’s Co-Lab session.`,
                                                 `Please select the content area for today’s Co-Lab Skyline session.`)) |>
  dplyr::pull(`Content Area`) |>
  unique() |>
  length() + 20

cps_data |>
  dplyr::filter(`What type of Co-Lab did you participate in today?` == "Universal") |>
  dplyr::select(contains("How much do you agree with the following statements about this session?"),
                "Please select the content area for today’s Co-Lab session.",
                "Please select the content area for today’s Co-Lab Skyline session.") |>
  # janitor::remove_empty("cols") |>
  # dplyr::select(-`How much do you agree with the following statements about this session? [This Co-Lab session helped me identify next steps to strengthen my use of Creative Curriculum to meet my students' needs.]`,
  #               -`How much do you agree with the following statements about this session? [This Co-Lab session provided me with the opportunity to actively explore the Creative Curriculum.]`) |>
  dplyr::mutate(`Content Area` = dplyr::coalesce(`Please select the content area for today’s Co-Lab session.`,
                                                 `Please select the content area for today’s Co-Lab Skyline session.`)) |>
  dplyr::select(-`Please select the content area for today’s Co-Lab session.`,
                -`Please select the content area for today’s Co-Lab Skyline session.`) |>
  dplyr::group_by(`Content Area`) |>
  dplyr::summarise(across(everything(), ~ pos_responses(.x))) |>
  tidyr::pivot_longer(!`Content Area`, names_to = "Question", values_to = "% Positive Responses") |>
  dplyr::mutate(Question = stringr::str_remove_all(Question, "\\.\\.\\.[0-9]")) |>
  dplyr::group_by(`Content Area`) |>
  dplyr::summarise(`% Positive Responses` = mean(`% Positive Responses`, na.rm = T)) |>
  range_write(ss = "https://docs.google.com/spreadsheets/d/1QqsTVWRSr5PUnA4WKfZoqhQZkCLUwVk2mi3-9JtkbHU/edit#gid=1659857917",
              sheet = "Universal Co-Labs",
              range = glue::glue("F20:G{content_area_count}"),
              reformat = F)

weekly_add_length <- 20 + length(unique(cut.Date(as.Date(cps_data$Timestamp), "week")))

#### Weekly ####
#### ISSUE: Needs to include correct number of rows for each week to update ####
cps_data |>
  dplyr::filter(`What type of Co-Lab did you participate in today?` == "Universal") |>
  dplyr::select(contains("How much do you agree with the following statements about this session?"),
                Timestamp) |>
  janitor::remove_empty("cols") |>
  # dplyr::select(-`How much do you agree with the following statements about this session? [This Co-Lab session helped me identify next steps to strengthen my use of Creative Curriculum to meet my students' needs.]`,
  #               -`How much do you agree with the following statements about this session? [This Co-Lab session provided me with the opportunity to actively explore the Creative Curriculum.]`) |>
  dplyr::mutate(Week = cut.Date(as.Date(Timestamp), "week")) |>
  dplyr::select(-Timestamp) |>
  dplyr::group_by(`Week`) |>
  dplyr::summarise(across(everything(), ~ pos_responses(.x))) |>
  tidyr::pivot_longer(!`Week`, names_to = "Question", values_to = "% Positive Responses") |>
  dplyr::mutate(Question = stringr::str_remove_all(Question, "\\.\\.\\.[0-9]")) |>
  dplyr::group_by(`Week`) |>
  dplyr::summarise(`% Positive Responses` = mean(`% Positive Responses`, na.rm = T)) |>
  range_write(ss = "https://docs.google.com/spreadsheets/d/1QqsTVWRSr5PUnA4WKfZoqhQZkCLUwVk2mi3-9JtkbHU/edit#gid=1659857917",
              sheet = "Universal Co-Labs",
              range = glue::glue("L20:M{weekly_add_length}"),
              reformat = F)

#### By Co-Lab ####
cps_data |>
  dplyr::filter(`What type of Co-Lab did you participate in today?` == "Universal") |>
  dplyr::select(contains("How much do you agree with the following statements about this session?"),
                "Which Co-Lab did you participate in today?") |>
  janitor::remove_empty("cols") |>
  # dplyr::select(-`How much do you agree with the following statements about this session? [This Co-Lab session helped me identify next steps to strengthen my use of Creative Curriculum to meet my students' needs.]`,
  #               -`How much do you agree with the following statements about this session? [This Co-Lab session provided me with the opportunity to actively explore the Creative Curriculum.]`) |>
  dplyr::group_by(`Which Co-Lab did you participate in today?`) |>
  dplyr::summarise(across(everything(), ~ pos_responses(.x))) |>
  tidyr::pivot_longer(!`Which Co-Lab did you participate in today?`, names_to = "Question", values_to = "% Positive Responses") |>
  dplyr::mutate(Question = stringr::str_remove_all(Question, "\\.\\.\\.[0-9]")) |>
  dplyr::group_by(`Which Co-Lab did you participate in today?`) |>
  dplyr::summarise(`% Positive Responses` = mean(`% Positive Responses`, na.rm = T)) |>
  dplyr::mutate(`Which Co-Lab did you participate in today?` = replace_na(`Which Co-Lab did you participate in today?`, "NA")) |>
  range_write(ss = "https://docs.google.com/spreadsheets/d/1QqsTVWRSr5PUnA4WKfZoqhQZkCLUwVk2mi3-9JtkbHU/edit#gid=1659857917",
              sheet = "Universal Co-Labs",
              range = "R20:S24",
              reformat = F)

#### Open Ended Feedback (Idk what to do with this) ####
# open_ended_feedback <- cps_data |>
#   dplyr::select(`What went well in today’s session?`,
#          `What could have been better about today’s session?`,
#          `What is the learning from this session that you are most excited about trying out?`,
#          `Feel free to leave us any additional comments, concerns, or questions.`)
####