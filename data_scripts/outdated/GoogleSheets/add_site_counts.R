library(googlesheets4)
library(qualtRics)
library(TeachingLab)
library(tidyverse)

# surveys <- qualtRics::all_surveys()

# math_bootcamp <- fetch_survey(surveyID = "SV_37uHoiF60EUKATQ", 
#                                                        verbose = TRUE)

### HAVE TO GET RESPONSES HERE TO FILTER FOR END OF COURSE/END OF SESSION ###
participant_feedback <- fetch_survey(surveyID = "SV_djt8w6zgigaNq0C", 
                                     verbose = TRUE)
### End of Session Survey Count ###
session_survey_count <- participant_feedback |>
  # filter(last_session_or_not == "No - today was not the final session.") |>
  filter(!is.na(Facilitator1)) |>
  group_by(Site) |>
  count() |>
  rename(`End of Session` = n)

### End of Course Survey Count ###
course_survey_count <- participant_feedback |>
  filter(last_session_or_not == "No - this was the final session for this PL course (e.g., last day of Bootcamp, close of Inquiry cycle) or coaching.") |>
  group_by(Site) |>
  count() |>
  rename(`End of Course` = n)

diagnostic_survey <- fetch_survey(surveyID = "SV_8vrKtPDtqQFbiBM",
                                  force_request = TRUE)

diagnostic_survey_count <- diagnostic_survey |>
  mutate(Site = replace_na(as.character(Site), "Other")) |>
  group_by(Site) |>
  count(sort = T) |>
  rename(`Diagnostic (pre)` = n)

knowledge_assessment_ids <- tibble::tibble(id = c("SV_37uHoiF60EUKATQ",
                                                  "SV_9YsBPlM5jZ30Dbg",
                                                  "SV_d5nw8tm0NF56kU6",
                                                  "SV_esouu9cYMOBSsGG",
                                                  "SV_0cxz1wVSJm3YOvc",
                                                  "SV_0GwEWwJqBGVOCPQ",
                                                  "SV_0vqNPC8wOinlWGa",
                                                  "SV_1CeZeXCWeyARdWe",
                                                  "SV_1HBrIAy2QDQwhiC",
                                                  "SV_1MJC6vEhbx69d30",
                                                  "SV_2lRbQxavLPyyRyC",
                                                  "SV_4HgPBvUQG6gxtsO",
                                                  "SV_55e1kSGK8f2TB4i",
                                                  "SV_5mCJ6o027GHTGcu",
                                                  "SV_5mMRhEvhx7YCLZQ",
                                                  "SV_6ineKFETiGhyDEq",
                                                  "SV_6umnpT1GKXJeWnI",
                                                  "SV_78OgnxKdYrBrem2",
                                                  "SV_7OOmjLlJxpVgME6",
                                                  "SV_8CFKiPQxAxwOZ9Q",
                                                  "SV_9HsrInMIskVsqTY",
                                                  "SV_bg5hii3sOQikIce",
                                                  "SV_bqg3mIevbXmAjfo",
                                                  "SV_cAMzWUjKWLZYC8e",
                                                  "SV_cwsF6v3SUG5zhc2",
                                                  "SV_d1pWuGz0wIOlO5M",
                                                  "SV_daT4Yvd8svibO1U",
                                                  "SV_efmWSbQwB6pclWm",
                                                  "SV_eONDuDJ9dfq5ZZk",
                                                  "SV_eVSKqZnfbI6k0rs",
                                                  "SV_ezb2kb3hqO6meHk"),
                                           name = c("Math: Bootcamp",
                                                    "Math: RAISE",
                                                    "ELA: Bootcamp - General",
                                                    "ELA: Bootcamp - Foundational Skills",
                                                    "ELA Guidebooks: Cycle of Inquiry 2 - Writing & Language Skills",
                                                    "ELA Guidebooks Diverse Learners: Bootcamp - Writing",
                                                    "Math: Bootcamp  - Curriculum Flexible",
                                                    "Math: Cycle of Inquiry I - Eliciting Student Thinking – Curriculum Flexible",
                                                    "ELA Guidebooks Diverse Learners: Cycle of Inquiry - Fluency",
                                                    "Math: Supporting Math Intervention",
                                                    "ELA Foundational Skills: Cycle of Inquiry 1: Classroom Management",
                                                    "ELA Guidebooks Diverse Learners: Bootcamp - Teacher",
                                                    "Math: Accelerating Learning",
                                                    "ELA EL: HQIM & Enrichment",
                                                    "Math: Cycle of Inquiry VI- Summarizing the Mathematics",
                                                    "ELA Curriculum Adaptive Foundational Skills: Cycle of Inquiry",
                                                    "Math: Cycle of Inquiry II - Making Math Visible",
                                                    "ELA Guidebooks Diverse Learners: Cycle of Inquiry - Vocabulary",
                                                    "ELA Guidebooks Diverse Learners: Bootcamp - Leader",
                                                    "ELA EL: Bootcamp - ALL Block (3-5)",
                                                    "Math: Cycle of Inquiry V- Sequencing and Connecting Representations",
                                                    "Math: Cycle of Inquiry I - Eliciting Student Thinking",
                                                    "ELA General: Cycle of Inquiry - Speaking & Listening",
                                                    "ELA General: Cycle of Inquiry - Complex Text",
                                                    "Math: Cycle of Inquiry III - Facilitating Mathematical Discourse",
                                                    "ELA CRSE: PLC",
                                                    "ELA Guidebooks: Cycle of Inquiry 1 - Close Reading/ Speaking & Listening",
                                                    "School Leaders: ELA CRSE PLC",
                                                    "Math: Cycle of Inquiry IV - Checking for Understanding",
                                                    "ELA Foundational Skills: Cycle of Inquiry 1",
                                                    "ELA Foundational Skills: Cycle of Inquiry 2: Using Data to Inform Foundational Skills Instruction"))

### Function to get a data frame of Site, n, knowledge assessment name ###
knowledge_assessment_n <- function(survey_id, survey_name) {
  print(paste0("Getting... ", survey_name))
  ### Get Survey ###
  know_assess <- qualtRics::fetch_survey(surveyID = survey_id, verbose = TRUE)
  
  ### All necessary knowledge_assessment_columns ###
  knowledge_assessment_columns <- c("Pre Assessment 1", "Post Assessment 1", "Pre Assessment 2", "Post Assessment 2", "Pre Assessment 3", "Post Assessment 3", "Pre Assessment 4", "Post Assessment 4", "Pre Assessment 5", "Post Assessment 5")
  if (nrow(know_assess |> filter(Finished == TRUE)) >= 1) {
    ### Ensure there is a site column ###
    if ("Site" %in% colnames(know_assess)) {
      ### Get Count of each knowledge assessment by Site ###
      know_assess_count <- know_assess |>
        dplyr::mutate(id = paste0(tolower(Initials), DOB),
                      Date = EndDate) |>
        dplyr::filter(Finished == TRUE & id != "tst1000") |>
        dplyr::group_by(id) |>
        dplyr::arrange(Date) |>
        dplyr::mutate(assessment_number = paste0("Assessment ", 1:dplyr::n())) |>
        dplyr::ungroup() |>
        dplyr::group_by(Site, assessment_number) |>
        dplyr::count(sort = T) |>
        dplyr::ungroup() |>
        dplyr::mutate(assessment_number = ifelse(assessment_number == "Assessment 1",
                                                 "Pre Assessment 1",
                                                 paste0("Post ", assessment_number))) |>
        tidyr::pivot_wider(names_from = "assessment_number", values_from = "n") |>
        dplyr::mutate(know_assess = survey_name,
                      Site = as.character(Site))
      
      columns_to_add <- setdiff(knowledge_assessment_columns, colnames(know_assess))
      
      column_df <- as.data.frame(columns_to_add) |>
        dplyr::mutate(fake = NA) |>
        tidyr::pivot_wider(names_from = "columns_to_add", values_from = "fake") |>
        dplyr::mutate(know_assess = survey_name)
      
      if (length(columns_to_add) >= 1) {
        know_assess_count <- know_assess_count |>
          dplyr::left_join(column_df)
      }
      
      return(know_assess_count)
    }
    
  } else {
    print("No responses yet!")
    NULL
  }
  
}

### Testing function ###
# test <- knowledge_assessment_n(survey_id = knowledge_assessment_ids$id[2],
#                                survey_name = knowledge_assessment_ids$name[2]) |>
#   print()

knowledge_assessment_count <- purrr::map2_dfr(knowledge_assessment_ids$id, knowledge_assessment_ids$name, 
                                              ~ knowledge_assessment_n(.x, .y))

data_collection_sy22_23 <- session_survey_count |>
  dplyr::left_join(course_survey_count) |>
  dplyr::left_join(diagnostic_survey_count) |>
  dplyr::left_join(knowledge_assessment_count |> select(-know_assess)) |>
  tibble::add_column(`Ongoing Coaching` = NA, .after = "End of Course") |>
  tibble::add_column(`End of Coaching` = NA, .after = "Ongoing Coaching") |>
  tibble::add_column(`Follow up (post)` = NA, .after = "Diagnostic (pre)") |>
  tibble::add_column(`Round 1` = NA, .after = "Post Assessment 5") |>
  tibble::add_column(`Round 2` = NA, .after = "Round 1") |>
  tibble::add_column(`Round 3` = NA, .after = "Round 2") |>
  tibble::add_column(`Round 4` = NA, .after = "Round 3") |>
  tibble::add_column(`Round 5` = NA, .after = "Round 4") |>
  tibble::add_column(`Student Survey pre` = NA, .after = "Round 5") |>
  tibble::add_column(`Student Survey post` = NA, .after = "Student Survey pre") |>
  tibble::add_column(`Student work samples round 1` = NA, .after = "Student Survey post") |>
  tibble::add_column(`Student work samples round 2` = NA, .after = "Student work samples round 1")


### Add two to sheet length to get actual range for google sheet to be written ###
sheet_length <- ncol(data_collection_sy22_23) + 2

googlesheets4::range_write(ss = "https://docs.google.com/spreadsheets/d/1fP1I1lugnfi-a5LpVgvtp1PtdRObColtvioTAFMtYqo/edit#gid=0",
                           data = data_collection_sy22_23,
                           sheet = "Tracker (n sizes) SY22-23",
                           range = glue::glue("A3:Z{sheet_length}"),
                           reformat = F,
                           col_names = F)

