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
  dplyr::filter(`What type of Co-Lab did you participate in today?` == "Skyline") |>
  dplyr::select(contains("How much do you agree with the following statements about this session?")) |>
  janitor::remove_empty("cols") |>
  # dplyr::select(-`How much do you agree with the following statements about this session? [This Co-Lab session helped me identify next steps to strengthen my use of Creative Curriculum to meet my students' needs.]`,
  #               -`How much do you agree with the following statements about this session? [This Co-Lab session provided me with the opportunity to actively explore the Creative Curriculum.]`) |>
  dplyr::summarise(across(everything(), ~ pos_responses(.x))) |>
  tidyr::pivot_longer(everything(), names_to = "Question", values_to = "% Positive Responses") |>
  dplyr::mutate(Question = stringr::str_remove_all(Question, "\\.\\.\\.[0-9]"),
                Question = stringr::str_remove_all(Question, "How much do you agree with the following statements about this session\\? \\[|[0-9]|\\]")) |>
  range_write(ss = "https://docs.google.com/spreadsheets/d/1QqsTVWRSr5PUnA4WKfZoqhQZkCLUwVk2mi3-9JtkbHU/edit#gid=1659857917",
              sheet = "Skyline Co-Labs",
              range = "D20:E38",
              reformat = F)

#### By Content Area ###
cps_data |>
  dplyr::filter(`What type of Co-Lab did you participate in today?` == "Skyline") |>
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
              sheet = "Skyline Co-Labs",
              range = "K20:L30",
              reformat = F)

weekly_add_length <- 20 + length(unique(cut.Date(as.Date(cps_data$Timestamp), "week")))

#### Weekly ####
#### ISSUE: Needs to include correct number of rows for each week to update ####
cps_data |>
  dplyr::filter(`What type of Co-Lab did you participate in today?` == "Skyline") |>
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
              sheet = "Skyline Co-Labs",
              range = glue::glue("P20:Q{weekly_add_length}"),
              reformat = F)

#### By Co-Lab ####
colabs_count <- length(unique(cps_data$`Which Co-Lab did you participate in today?`[cps_data$`What type of Co-Lab did you participate in today?` == "Skyline"])) + 20

cps_data |>
  dplyr::filter(`What type of Co-Lab did you participate in today?` == "Skyline") |>
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
              sheet = "Skyline Co-Labs",
              range = glue::glue("V20:W{colabs_count}"),
              reformat = F)

session_count <- cps_data$`What Co-Lab session did you participate in today?` |> 
  unique() |> 
  keep( ~ !is.na(.x)) |> 
  length() + 2

#### ELA Questions only ####
cps_data |>
  dplyr::select("What Co-Lab session did you participate in today?",
                "How much do you agree with the following statement about this session? [I feel very comfortable navigating the TFG and using it to plan.]",
                "How much do you agree with the following statement about this session? [I understand all of the essential components of the Skyline ELA classroom and how they support students’ literacy learning.]",
                "How much do you agree with the following statement about this session? [Scripting a Think-Aloud was beneficial in moving my personal teaching practice forward.]",
                "How much do you agree with the following statement about this session? [Intentionally centering students’ identities during the Think-Aloud had a positive impact on student learning and/or engagement.]") |>
  janitor::remove_empty("rows") |>
  group_by(`What Co-Lab session did you participate in today?`) |>
  dplyr::summarise(across(everything(), ~ pos_responses(.x))) |>
  pivot_longer(!`What Co-Lab session did you participate in today?`, names_to = "Session", values_to = "Percent") |>
  group_by(`What Co-Lab session did you participate in today?`) |>
  summarise(Percent = mean(Percent, na.rm = T)) |>
  range_write(ss = "https://docs.google.com/spreadsheets/d/1QqsTVWRSr5PUnA4WKfZoqhQZkCLUwVk2mi3-9JtkbHU/edit#gid=278547399",
              sheet = "Skyline Co-Labs",
              range = glue::glue("A2:B{session_count}"),
              reformat = F,
              col_names = T)

#### Open Ended Feedback (Idk what to do with this) ####
# open_ended_feedback <- cps_data |>
#   dplyr::select(`What went well in today’s session?`,
#          `What could have been better about today’s session?`,
#          `What is the learning from this session that you are most excited about trying out?`,
#          `Feel free to leave us any additional comments, concerns, or questions.`)
####