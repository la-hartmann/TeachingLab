library(dplyr)
library(janitor)
library(purrr)
library(qualtRics)
library(readr)
library(TeachingLab)
library(tibble)
library(tidyr)

### Knowledge Assessments Scoring and Storage Script ###
### List of ids and knowledge assessments ###
knowledge_assessment_ids <- tibble::tibble(
  id = c(
    "SV_37uHoiF60EUKATQ",
    "SV_eLksATH1PWbrJEq",
    "SV_01k3VTm9Ptd0NFA",
    "SV_bg5hii3sOQikIce",
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
    "SV_bqg3mIevbXmAjfo",
    "SV_cAMzWUjKWLZYC8e",
    "SV_cwsF6v3SUG5zhc2",
    "SV_d1pWuGz0wIOlO5M",
    "SV_daT4Yvd8svibO1U",
    "SV_efmWSbQwB6pclWm",
    "SV_eONDuDJ9dfq5ZZk",
    "SV_eVSKqZnfbI6k0rs",
    "SV_ezb2kb3hqO6meHk",
    "SV_eL4PMDURWjWyrn8",
    "SV_1MJC6vEhbx69d30",
    "SV_d3TcHB2JN5xnHJI",
    "SV_2mCAWxvQuy2RnBY"
  ),
  name = c(
    "Math: Bootcamp",
    "Math: Cycle of Inquiry - Instructional Routines",
    "ELA General Cycle of Inquiry - Understanding the Key Actions of the IPG",
    "Math: Cycle of Inquiry I - Eliciting Student Thinking",
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
    "ELA General: Cycle of Inquiry - Speaking & Listening",
    "ELA General: Cycle of Inquiry - Complex Text",
    "Math: Cycle of Inquiry III - Facilitating Mathematical Discourse",
    "ELA CRSE: PLC",
    "ELA Guidebooks: Cycle of Inquiry 1 - Close Reading/ Speaking & Listening",
    "School Leaders: ELA CRSE PLC",
    "Math: Cycle of Inquiry IV - Checking for Understanding",
    "ELA Foundational Skills: Cycle of Inquiry 1",
    "ELA Foundational Skills: Cycle of Inquiry 2: Using Data to Inform Foundational Skills Instruction",
    "School Leaders: Curriculum Adaptive Math/ELA (ILN)",
    "Math: Learning Across the Domains",
    "Language Standards/Conventions Knowledge_NYBlendedLit",
    "Math: Cycle of Inquiry Self-Reported Practices"
  )
)
write_rds(knowledge_assessment_ids , here::here("data/sy23_24/knowledge_assessment_ids.rds"))

### Filter list for just those with responses ###

knowledge_assessments_for_scoring <- knowledge_assessment_ids |>
  dplyr::filter(name %in% c(
    "Math: Bootcamp",
    "Math: Cycle of Inquiry I - Eliciting Student Thinking",
    "ELA General Cycle of Inquiry - Understanding the Key Actions of the IPG",
    "Math: Cycle of Inquiry - Instructional Routines",
    "ELA ANA",
    "ELA K-2 ANA",
    "Math ANA"
  )) |>
  ### This line is for the knowledge assessments dashboard, it can't handle the / ###
  dplyr::mutate(name = stringr::str_replace_all(name, "\\/", "|"))

### Get max score of survey ###
# if (survey_name == "Math: Bootcamp") {
#   max_overall_score <- 9
# } else if (survey_name == "Math: RAISE") {
#   max_overall_score <- 11
# } else if (survey_name == "ELA: Bootcamp - General") {
#   max_overall_score <- 8
# } else if (survey_name == "ELA: Bootcamp - Foundational Skills") {
#   max_overall_score <- 7
# } else if (survey_name == "ELA General: Cycle of Inquiry - Complex Text") {
#   max_overall_score <- 6
# } else if (survey_name == "Math: Accelerating Learning") {
#   max_overall_score <- 6
# } else if (survey_name == "Math: Cycle of Inquiry I - Eliciting Student Thinking") {
#   max_overall_score <- 6
# } else if (survey_name == "School Leaders: Curriculum Adaptive Math|ELA (ILN)") {
#   max_overall_score <- 9
# } else if (survey_name == "Math: Learning Across the Domains") {
#   max_overall_score <- 5
# } else if (survey_name == "Language Standards|Conventions Knowledge_NYBlendedLit") {
#   max_overall_score <- 10
# } else if (survey_name == "ELA Guidebooks: Cycle of Inquiry 2 - Writing & Language Skills") {
#   max_overall_score <- 6
# } else if (survey_name == "Math: Cycle of Inquiry III - Facilitating Mathematical Discourse") {
#   max_overall_score <- 5
# } else if (survey_name == "Math ANA") {
#   max_overall_score <- 15
# } else if (survey_name == "ELA ANA") {
#   max_overall_score <- 15 # For normal ANA
#   max_overall_score <- 21 # For ELA K-2 ANA
# } else if (survey_name == "ELA K-2 ANA") {
#   max_overall_score <- 15 # Not sure here tbh
# }


### NOTE THAT ONLY ANSWER SHOULD EVER BE NA, QUESTIONS MAY FAIL TO MATCH SCORE NAMES BECAUSE OF PARENTHESES ###
### NOTE THAT SOME QUESTIONS WITH MORE THAN 2 CORRECT ANSWERS MAY NEED TO BE HARD CODED AS 3 OR 4 BECAUSE OF STR_DETECT BELOW
### SEE MATH: CYCLE OF INQUIRY VI - SUMMARIZING THE MATHEMATICS ###
knowledge_assessments_detailed_scored <- purrr::map2_dfr(
  knowledge_assessments_for_scoring$id, knowledge_assessments_for_scoring$name,
  ~ TeachingLab::knowledge_assess_detailed_score(.x, .y, start_date = as.Date("2023-07-01"))
) |>
  dplyr::mutate(max_score = dplyr::case_when(stringr::str_detect(question2, "key functions") ~ 4, # Max score for question on Understanding Key Actions of the IPG
                                             stringr::str_detect(question2, "Select all that apply") ~ 2,
                                             is.na(question2) & question1 != "Score" ~ 1, # This means that it will b a single answer question, hence 1 point
                                             know_assess == "Math: Bootcamp" & question1 == "Score" ~ 9,
                                             know_assess == "ELA: Bootcamp - General" & question1 == "Score" ~ 8,
                                             know_assess == "ELA: Bootcamp - Foundational Skills" & question1 == "Score" ~ 7,
                                             know_assess == "Math: Accelerating Learning" & question1 == "Score" ~ 6,
                                             know_assess == "Math ANA" & question1 == "Score" ~ 15,
                                             know_assess == "ELA ANA" & question1 == "Score" ~ 21,
                                             know_assess == "ELA K-2 ANA" & question1 == "Score" ~ 15,
                                             know_assess == "Math: Cycle of Inquiry I - Eliciting Student Thinking" & question1 == "Score" ~ 6,
                                             know_assess == "ELA General Cycle of Inquiry - Understanding the Key Actions of the IPG" & question1 == "Score" ~ 7,
                                             know_assess == "Math: Cycle of Inquiry - Instructional Routines" & question1 == "Score" ~ 4))

### Format here is id, prepost, site, know_assess, date, question, answer, score ###
readr::write_rds(knowledge_assessments_detailed_scored, here::here("dashboards/course_assessments/data/knowledge_assessments.rds"))
readr::write_rds(knowledge_assessments_detailed_scored, here::here("dashboards/OngoingReport_23_24/data/knowledge_assessments.rds"))

know_assess_answers <- read_sheet("1rbzcBgQ3hHG7v-HC-jQMknjSgMkvG74wDKACiOfza2M") |>
  dplyr::filter(Correct == TRUE) |>
  dplyr::pull(Answer) |>
  as.character()
write_rds(know_assess_answers, here::here("dashboards/course_assessments/data/know_assess_answers.rds"))

# #### Add in educator and follow up survey ###
# educator_survey <- TeachingLab::get_diagnostic_survey(update = TRUE, year = "23_24")
# # Doesn't need update because it is the same as educator survey just post results #
# followup_educator <- TeachingLab::get_followup_educator(update = FALSE, year = "23_24")
# 
# ### percent, prepost, site, know_assess ###
# knowledge_assessments_educator <- knowledge_assessments_detailed_scored |>
# dplyr::bind_rows(
#   ### Pre Math ANA ###
#   educator_survey |>
#     tidyr::drop_na(hqim1_1) |>
#     dplyr::mutate(
#       id = tolower(email),
#       know_assess = "Math ANA",
#       prepost = "pre",
#       prepost = factor(prepost, levels = c("pre", "post")),
#       score = SC4, ### TOTAL MAX SCORE HERE IS 15
#       across(where(is.logical), as.character),
#       date = RecordedDate) |>
#     dplyr::select(id, prepost, site, know_assess, date,
#                   dplyr::contains("hqim"), equity, dplyr::contains("equity_instruction"), 
#                   instruction, dplyr::contains("eliciting_ss_1_6_12"), dplyr::contains("eliciting_ss_1_k5"),
#                   eliciting_ss_2, eliciting_ss_3, dplyr::contains("eliciting_ss_4"),
#                   score) |>
#     dplyr::mutate(
#       across(where(is.character), ~ na_if(.x, "NA")),
#       id = as.character(id),
#       site = as.character(site),
#       date = as.POSIXct(as.character(date)),
#       score = as.numeric(score)
#     ) |>
#     TeachingLab::relabel_qualtrics_df() |>
#     tidyr::pivot_longer(!c(id, prepost, site, know_assess, date, score), names_to = "question", values_to = "answer") |>
#     tidyr::drop_na(score) |>
#     dplyr::rename(question1 = question) |>
#     dplyr::mutate(question2 = stringr::str_extract(question1, ".*?(?=\\. - )"), # Get first part for question 1
#                   question1 = stringr::str_remove_all(question1, ".*?\\. - ")) |> # Get second part for question 2
#     dplyr::relocate(question2, .after = question1) |>
#     dplyr::relocate(score, .after = answer), ### STILL NEEDS SCORE TO BE BY QUESTION AND MAX_SCORE VARIABLE
#   ### Post Math ANA ###
#   followup_educator |>
#     tidyr::drop_na(hqim1_1) |>
#     dplyr::mutate(
#       id = tolower(email),
#       know_assess = "Math ANA", ### TOTAL SCORE HERE IS 15
#       prepost = "post",
#       prepost = factor(prepost, levels = c("pre", "post")),
#       score = SC0,
#       across(where(is.logical), as.character)) |>
#     dplyr::select(id, prepost, site, know_assess, date = RecordedDate, 
#                   dplyr::contains("hqim"), equity, dplyr::contains("equity_instruction"), 
#                   instruction, dplyr::contains("eliciting_ss_1_6_12"), dplyr::contains("eliciting_ss_1_k5"),
#                   eliciting_ss_2, eliciting_ss_3, dplyr::contains("eliciting_ss_4"),
#                   score) |>
#     tidyr::pivot_longer(!c(id, prepost, site, know_assess, date, score), names_to = "question", values_to = "answer") |>
#     tidyr::drop_na(score),
#   ### Pre ELA K-2 ANA ###
#   educator_survey |>
#     tidyr::drop_na(ela_bc_1_1) |>
#     dplyr::mutate(
#       id = tolower(email),
#       know_assess = "ELA K-2 ANA",
#       prepost = "pre",
#       prepost = factor(prepost, levels = c("pre", "post")),
#       score = SC0, ### TOTAL MAX SCORE HERE IS EITHER 15 OR 21
#       across(where(is.logical), as.character)) |>
#     dplyr::select(id, prepost, site, know_assess, date = RecordedDate, 
#                   dplyr::contains("k2_ela1"), k2_ela_2, dplyr::contains("k2_ela_3"), 
#                   score) |>
#     tidyr::pivot_longer(!c(id, prepost, site, know_assess, date, score), names_to = "question", values_to = "answer") |>
#     tidyr::drop_na(score),
#   ### Post ELA K-2 ANA ###
#   followup_educator |>
#     tidyr::drop_na(ela_bc_1_1) |>
#     dplyr::mutate(
#       id = tolower(email),
#       know_assess = "ELA K-2 ANA",
#       prepost = "post",
#       prepost = factor(prepost, levels = c("pre", "post")),
#       score = SC0,
#       across(where(is.logical), as.character)) |>
#     dplyr::select(id, prepost, site, know_assess, date = RecordedDate, 
#                   dplyr::contains("k2_ela1"), k2_ela_2, dplyr::contains("k2_ela_3"), 
#                   score) |>
#     tidyr::pivot_longer(!c(id, prepost, site, know_assess, date, score), names_to = "question", values_to = "answer") |>
#     tidyr::drop_na(score),
#   ### JUST Pre ELA ANA ###
#   educator_survey |>
#     tidyr::drop_na(k2_ela1_1) |>
#     dplyr::mutate(
#       id = tolower(email),
#       know_assess = "ELA ANA",
#       prepost = "pre",
#       prepost = factor(prepost, levels = c("pre", "post")),
#       score = SC0, ### TOTAL MAX SCORE HERE IS 21
#       across(where(is.logical), as.character)) |>
#     dplyr::select(id, prepost, site, know_assess, date = RecordedDate, 
#                   dplyr::contains("ela_bc_1"), ela_bc_2, ela_bc_3, dplyr::contains("ela_bc_4"), dplyr::contains("ela_bc_5"),
#                   dplyr::contains("cycle_complex_txt1"), cycle_complex_txt2, 
#                   dplyr::contains("cycle_s&l1"), dplyr::contains("cycle_s&l_2"),
#                   score) |>
#     tidyr::pivot_longer(!c(id, prepost, site, know_assess, date, score), names_to = "question", values_to = "answer") |>
#     tidyr::drop_na(score),
#   ### JUST Post ELA ANA ###
#   followup_educator |>
#     tidyr::drop_na(k2_ela1_1) |>
#     dplyr::mutate(
#       id = tolower(email),
#       know_assess = "ELA ANA",
#       prepost = "post",
#       prepost = factor(prepost, levels = c("pre", "post")),
#       score = SC0, ### TOTAL MAX SCORE HERE IS 21
#       across(where(is.logical), as.character)) |>
#     dplyr::select(id, prepost, site, know_assess, date = RecordedDate, 
#                   dplyr::contains("ela_bc_1"), ela_bc_2, ela_bc_3, dplyr::contains("ela_bc_4"), dplyr::contains("ela_bc_5"),
#                   dplyr::contains("cycle_complex_txt1"), cycle_complex_txt2, 
#                   dplyr::contains("cycle_s&l1"), dplyr::contains("cycle_s&l_2"),
#                   score) |>
#     tidyr::pivot_longer(!c(id, prepost, site, know_assess, date, score), names_to = "question", values_to = "answer") |>
#     tidyr::drop_na(score)
# )

