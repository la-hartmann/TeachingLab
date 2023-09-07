library(googlesheets4)
library(tidyverse)
library(TeachingLab)

end_session_count <- function(data, grouping) {
  
  data |>
    dplyr::filter(last_session_or_not != "No - this was the final session for this PL course (e.g., last day of Bootcamp, close of Inquiry cycle) or last coaching session of the cycle." &
                    course != "Coaching") |>
    dplyr::group_by({{ grouping }}) |>
    dplyr::count() |>
    dplyr::rename(`End of Session` = n)
}

end_course_count <- function(data, grouping) {
  
  data |>
    dplyr::filter(last_session_or_not == "No - this was the final session for this PL course (e.g., last day of Bootcamp, close of Inquiry cycle) or last coaching session of the cycle." &
                    course != "Coaching") |>
    dplyr::group_by({{ grouping }}) |>
    dplyr::count() |>
    dplyr::rename(`End of Course` = n)
}

ongoing_coach_count <- function(data, grouping) {
  
  data |>
    dplyr::filter(last_session_or_not != "No - this was the final session for this PL course (e.g., last day of Bootcamp, close of Inquiry cycle) or last coaching session of the cycle." &
                    course == "Coaching") |>
    dplyr::group_by({{ grouping }}) |>
    dplyr::count() |>
    dplyr::rename(`Ongoing Coaching` = n)
}

end_coaching_count <- function(data, grouping) {
  
  data |>
    dplyr::filter(last_session_or_not == "No - this was the final session for this PL course (e.g., last day of Bootcamp, close of Inquiry cycle) or last coaching session of the cycle." &
                    course == "Coaching") |>
    dplyr::group_by({{ grouping }}) |>
    dplyr::count() |>
    dplyr::rename(`End of Coaching` = n)
}

diagnostic_count <- function(data, grouping) {
  
  data |>
    dplyr::group_by({{ grouping }}) |>
    dplyr::count()
}

ipg_count <- function(data, grouping) {
  
  data |>
    dplyr::group_by({{ grouping }}) |>
    dplyr::count()
}

### Function to get a data frame of site, n, knowledge assessment name ###
knowledge_assessment_n <- function(survey_id, survey_name, site = "site") {
  
  site <- rlang::sym(site)
  
  print(paste0("Getting... ", survey_name))
  ### Get Survey ###
  know_assess <- qualtRics::fetch_survey(surveyID = survey_id, 
                                         verbose = TRUE, 
                                         include_display_order = FALSE) |>
    dplyr::filter(RecordedDate >= as.Date("2023-07-01")) |>
    suppressWarnings()
  
  ### All necessary knowledge_assessment_columns ###
  if (nrow(know_assess |> dplyr::filter(Finished == TRUE)) >= 1) {
    ### Ensure there is a site column ###
    if ("site" %in% colnames(know_assess)) {
      ### Get Count of each knowledge assessment by site ###
      know_assess_count <- know_assess |>
        dplyr::mutate(
          Date = RecordedDate
        ) |>
        dplyr::filter(Finished == TRUE & !email %in% c("test@teachinglab.org")) |>
        dplyr::group_by(email) |>
        dplyr::mutate(
          n_response = dplyr::n(), # Get number of responses by person, sometimes there are more than 2 :/
          maxdate = max(RecordedDate), # Get max date of creation for most recent response
          prepost = dplyr::if_else(RecordedDate >= maxdate & n_response > 1, "post", "pre"),
          prepost = factor(prepost, levels = c("pre", "post"))
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(email, prepost, !!site) |>
        dplyr::count(sort = T) |>
        dplyr::ungroup() |>
        tidyr::pivot_wider(names_from = "prepost", values_from = "n") |>
        dplyr::mutate(
          site = as.character(!!site)
        ) |>
        dplyr::select(-email)
      
      if (!"post" %in% colnames(know_assess_count)) {
        know_assess_count$post <- NA
      }
      
      know_assess_count <- know_assess_count |>
        dplyr::group_by(site) |>
        dplyr::summarise(
          pre = sum(pre, na.rm = T),
          post = sum(post, na.rm = T)
        ) |>
        dplyr::rename(
          !!paste0(survey_name, " Pre") := pre,
          !!paste0(survey_name, " Post") := post
        )
      
      ### Older, worse setup code ###
      # columns_to_add <- setdiff(knowledge_assessment_columns, colnames(know_assess))
      #
      # column_df <- as.data.frame(columns_to_add) |>
      #   dplyr::mutate(fake = NA) |>
      #   tidyr::pivot_wider(names_from = "columns_to_add", values_from = "fake") |>
      #   dplyr::mutate(know_assess = survey_name)
      #
      # if (length(columns_to_add) >= 1) {
      #   know_assess_count <- know_assess_count |>
      #     dplyr::left_join(column_df)
      # }
      
      return(know_assess_count)
    }
  } else {
    NULL
  }
}

participant_feedback <- qualtRics::fetch_survey(
  surveyID = "SV_djt8w6zgigaNq0C",
  verbose = TRUE,
  include_display_order = FALSE,
  force_request = FALSE
) |>
  dplyr::filter(Finished == TRUE & RecordedDate >= as.Date("2023-07-01"))
### End of Session Survey Count ###
session_survey_count <- participant_feedback |>
  end_session_count(site)

### End of course Survey Count ###
course_survey_count <- participant_feedback |>
  end_course_count(site)

### End of ongoing Survey Count ###
ongoing_coaching_survey_count <- participant_feedback |>
  ongoing_coach_count(site)

### End of end_coach Survey Count ###
end_coach_survey_count <- participant_feedback |>
  end_coaching_count(site)

### Educator Survey Count ###

followup_count <- qualtRics::fetch_survey(
  surveyID = "SV_8vrKtPDtqQFbiBM",
  verbose = FALSE,
  include_display_order = FALSE,
  force_request = FALSE
) |>
  dplyr::filter(RecordedDate >= as.Date("2023-07-01") & Finished == TRUE) |>
  dplyr::mutate(email = tolower(email)) |>
  dplyr::group_by(email) |>
  dplyr::mutate(n = dplyr::n(),
                max_date = ifelse(RecordedDate == max(RecordedDate), TRUE, FALSE)) |>
  dplyr::ungroup() |>
  dplyr::filter(n == 2 & max_date == TRUE) |>
  diagnostic_count(site) |>
  dplyr::rename(`Follow up (post)` = n)

educator_count <- qualtRics::fetch_survey(
  surveyID = "SV_8vrKtPDtqQFbiBM",
  verbose = FALSE,
  include_display_order = FALSE,
  force_request = FALSE
) |>
  dplyr::filter(RecordedDate >= as.Date("2023-07-01") & Finished == TRUE) |>
  dplyr::group_by(email) |>
  dplyr::mutate(n = dplyr::n(),
                max_date = ifelse(RecordedDate == max(RecordedDate), TRUE, FALSE)) |>
  dplyr::ungroup() |>
  dplyr::filter(n == 1 | (n == 2 & max_date == FALSE)) |>
  diagnostic_count(site) |>
  dplyr::rename(`Diagnostic (pre)` = n) |>
  dplyr::left_join(followup_count)

### IPG Forms Count ###

classroom_obs_count <- qualtRics::fetch_survey(
  surveyID = "SV_0BSnkV9TVXK1hjw",
  verbose = FALSE,
  include_display_order = FALSE,
  force_request = FALSE
) |>
  dplyr::filter(RecordedDate >= as.Date("2023-07-01") & Finished == TRUE) |>
  dplyr::group_by(site, service_tl_only) |>
  dplyr::count(sort = T) |>
  pivot_wider(names_from = service_tl_only, values_from = n)

### Student Survey Counts ###

student_survey <- qualtRics::fetch_survey(
  surveyID = "SV_9uze2faHuIf3vP8",
  verbose = FALSE,
  convert = FALSE,
  include_display_order = FALSE,
  force_request = FALSE
) |>
  dplyr::filter(Finished == TRUE & RecordedDate >= as.Date("2023-07-01")) |>
  dplyr::group_by(site) |>
  dplyr::count(sort = T) |>
  dplyr::ungroup()

eic_student_survey <- qualtRics::fetch_survey(
  surveyID = "SV_8f9l21n6ML58WFM",
  convert = FALSE,
  verbose = FALSE,
  include_display_order = FALSE,
  force_request = FALSE
) |>
  dplyr::filter(Finished == TRUE & RecordedDate >= as.Date("2023-07-01")) |>
  dplyr::group_by(site) |>
  dplyr::count(sort = T) |>
  dplyr::ungroup()

final_student_survey_count <- student_survey |>
  dplyr::left_join(eic_student_survey) |>
  dplyr::group_by(site) |>
  dplyr::count(sort = T) |>
  dplyr::ungroup() |>
  dplyr::rename(`Student Survey (pre)` = n) |>
  dplyr::mutate(`Student Survey (post)` = 0)

### Student Work Counts ###

student_work_count <- qualtRics::fetch_survey("SV_6nwa9Yb4OyXLji6",
                                        include_display_order = FALSE,
                                        verbose = FALSE,
                                        force_request = FALSE
) |>
  dplyr::filter(RecordedDate >= as.Date("2023-07-01") & Finished == TRUE) |>
  suppressWarnings() |>
  dplyr::group_by(site) |>
  dplyr::count(sort = T) |>
  dplyr::ungroup() |>
  dplyr::rename(`Student Work (pre)` = n) |>
  dplyr::mutate(`Student Work (post)` = 0)

### Knowledge Assessment Counts ###

knowledge_assessment_ids <- tibble::tibble(
  id = c(
    "SV_37uHoiF60EUKATQ",
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
    "SV_ezb2kb3hqO6meHk",
    "SV_eL4PMDURWjWyrn8",
    "SV_1MJC6vEhbx69d30",
    "SV_d3TcHB2JN5xnHJI"
  ),
  name = c(
    "Math: Bootcamp",
    "Math: RAISE",
    "ELA: Bootcamp - General",
    "ELA: Bootcamp - Foundational Skills",
    "ELA Guidebooks: Cycle of Inquiry 2 - Writing & Language Skills",
    "ELA Guidebooks Diverse Learners: Bootcamp - Writing",
    "Math: Bootcamp  - Curriculum Flexible",
    "Math: Cycle of Inquiry I - Eliciting Student Thinking - Curriculum Flexible",
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
    "ELA Foundational Skills: Cycle of Inquiry 2: Using Data to Inform Foundational Skills Instruction",
    "School Leaders: Curriculum Adaptive Math/ELA (ILN)",
    "Math: Learning Across the Domains",
    "Language Standards/Conventions Knowledge_NYBlendedLit"
  )
)

knowledge_assessments_count <- purrr::map2_dfr(
  knowledge_assessment_ids$id, knowledge_assessment_ids$name,
  ~ knowledge_assessment_n(.x, .y)
) |>
  dplyr::group_by(site) |>
  dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(.x, na.rm = TRUE))) |>
  dplyr::mutate(dplyr::across(!site, ~ na_if(.x, 0))) |>
  janitor::remove_empty("cols")

main_sheet <- read_sheet("1hIAg3aNVXwSJGi14QaOblVFbPwwTgpAvj88TeGKD8zA",
                         sheet = "Overall Sites",
                         range = "A:A") |>
  rename(site = Site)

sites_count_df <- main_sheet |>
  dplyr::left_join(session_survey_count, by = "site") |>
  dplyr::left_join(course_survey_count, by = "site") |>
  dplyr::left_join(ongoing_coaching_survey_count, by = "site") |>
  dplyr::left_join(end_coach_survey_count, by = "site") |>
  dplyr::left_join(educator_count, by = "site") |>
  dplyr::left_join(classroom_obs_count, by = "site") |>
  dplyr::left_join(final_student_survey_count, by = "site") |>
  dplyr::left_join(student_work_count, by = "site") |>
  dplyr::left_join(knowledge_assessments_count, by = "site") |>
  dplyr::rename(Site = site)

col_range <- paste0("A1:", LETTERS[ncol(sites_count_df)], nrow(sites_count_df) + 1)

range_write(sites_count_df,
            ss = "1hIAg3aNVXwSJGi14QaOblVFbPwwTgpAvj88TeGKD8zA",
            sheet = "Overall Sites",
            col_names = TRUE,
            reformat = FALSE,
            range = col_range)

ny_manual_entry_adjust <- function(data, dropdown) {
  if (dropdown %in% c("NY_D9", "NY_D11", "NY_D12", "NY_D16", "NY_D75")) {
    
    ny_add <- dplyr::case_when(dropdown == "NY_D9" ~ "NY_D9_09x",
                               dropdown == "NY_D11" ~ "NY_D11_11x",
                               dropdown == "NY_D12" ~ "NY_D12_12x",
                               dropdown == "NY_D16" ~ "NY_D16_16x",
                               dropdown == "NY_D75" ~ "NY_75_75x")
    
    data <- data |>
      dplyr::mutate(site = paste0(ny_add, site),
                    site = ifelse(str_detect(site, "(?i)other|000"), "Other", site)) # 000 was a recode for other for a lot of participants it seemed like
  }
  
  data <- data |>
    dplyr::mutate(site = as.character(site))
  
}

dropdown_count <- function(dropdown, dropdown2) {
  
  dropdown2 <- rlang::sym(dropdown2)
  
  participant_feedback <- qualtRics::fetch_survey(
    surveyID = "SV_djt8w6zgigaNq0C",
    verbose = FALSE,
    include_display_order = FALSE,
    force_request = FALSE
  ) |>
    dplyr::filter(Finished == TRUE & RecordedDate >= as.Date("2023-07-01")) |>
    dplyr::mutate(!!dropdown2 := as.character(!!dropdown2))
  ### End of Session Survey Count ###
  session_survey_count <- participant_feedback |>
    dplyr::filter(last_session_or_not != "No - this was the final session for this PL course (e.g., last day of Bootcamp, close of Inquiry cycle) or last coaching session of the cycle." &
                    course != "Coaching") |>
    dplyr::group_by(!!dropdown2) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::rename(`End of Session` = n, site = dropdown2) |>
    dplyr::filter(!is.na(site)) |>
    ny_manual_entry_adjust(dropdown = dropdown)
  
  ### End of course Survey Count ###
  course_survey_count <- participant_feedback |>
    dplyr::filter(last_session_or_not == "No - this was the final session for this PL course (e.g., last day of Bootcamp, close of Inquiry cycle) or last coaching session of the cycle." &
                    course != "Coaching") |>
    dplyr::group_by(!!dropdown2) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::rename(`End of Course` = n, site = dropdown2) |>
    dplyr::filter(!is.na(site)) |>
    ny_manual_entry_adjust(dropdown = dropdown)
  
  ### End of ongoing Survey Count ###
  ongoing_coaching_survey_count <- participant_feedback |>
    dplyr::filter(last_session_or_not != "No - this was the final session for this PL course (e.g., last day of Bootcamp, close of Inquiry cycle) or last coaching session of the cycle." &
                    course == "Coaching") |>
    dplyr::group_by(!!dropdown2) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::rename(`Ongoing Coaching` = n, site = dropdown2) |>
    dplyr::filter(!is.na(site)) |>
    ny_manual_entry_adjust(dropdown = dropdown)

  ### End of end_coach Survey Count ###
  end_coach_survey_count <- participant_feedback |>
    dplyr::filter(last_session_or_not == "No - this was the final session for this PL course (e.g., last day of Bootcamp, close of Inquiry cycle) or last coaching session of the cycle." &
                    course == "Coaching") |>
    dplyr::group_by(!!dropdown2) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::rename(`End of Coaching` = n, site = dropdown2) |>
    dplyr::filter(!is.na(site)) |>
    ny_manual_entry_adjust(dropdown = dropdown)
  
  ### Educator Survey Count ###
  followup_count <- qualtRics::fetch_survey(
    surveyID = "SV_8vrKtPDtqQFbiBM",
    verbose = FALSE,
    include_display_order = FALSE,
    force_request = FALSE
  ) |>
    dplyr::filter(RecordedDate >= as.Date("2023-07-01") & Finished == TRUE) |>
    dplyr::mutate(email = tolower(email)) |>
    dplyr::group_by(email) |>
    dplyr::mutate(n = dplyr::n(),
                  max_date = ifelse(RecordedDate == max(RecordedDate), TRUE, FALSE)) |>
    dplyr::ungroup() |>
    dplyr::filter(n == 2 & max_date == TRUE) |>
    dplyr::group_by(!!dropdown2) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::rename(`Follow up (post)` = n, site = dropdown2) |>
    ny_manual_entry_adjust(dropdown = dropdown)
  
  educator_count <- qualtRics::fetch_survey(
    surveyID = "SV_8vrKtPDtqQFbiBM",
    verbose = FALSE,
    include_display_order = FALSE,
    force_request = FALSE
  ) |>
    dplyr::mutate(!!dropdown2 := as.character(!!dropdown2)) |>
    dplyr::filter(RecordedDate >= as.Date("2023-07-01") & Finished == TRUE) |>
    dplyr::group_by(!!dropdown2) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::rename(`Diagnostic (pre)` = n, site = dropdown2) |>
    dplyr::left_join(followup_count) |>
    dplyr::filter(!is.na(site)) |>
    ny_manual_entry_adjust(dropdown = dropdown)
  
  ### IPG Forms Count ###
  
  classroom_obs_count <- qualtRics::fetch_survey(
    surveyID = "SV_0BSnkV9TVXK1hjw",
    verbose = FALSE,
    include_display_order = FALSE,
    force_request = FALSE
  ) |>
    dplyr::mutate(!!dropdown2 := as.character(!!dropdown2)) |>
    dplyr::filter(RecordedDate >= as.Date("2023-07-01") & Finished == TRUE) |>
    dplyr::group_by(!!dropdown2, service_tl_only) |>
    dplyr::count(sort = T) |>
    tidyr::pivot_wider(names_from = service_tl_only, values_from = n) |>
    dplyr::rename(site = dropdown2) |>
    dplyr::filter(!is.na(site)) |>
    ny_manual_entry_adjust(dropdown = dropdown)
  
  ### Student Work Counts ###
  
  # student_work_count <- qualtRics::fetch_survey("SV_6nwa9Yb4OyXLji6",
  #                                               include_display_order = FALSE,
  #                                               verbose = FALSE,
  #                                               force_request = FALSE
  # ) |>
  #   dplyr::filter(RecordedDate >= as.Date("2023-07-01")) |>
  #   suppressWarnings() |>
  #   dplyr::group_by(!!dropdown2) |>
  #   dplyr::count(sort = T) |>
  #   dplyr::ungroup() |>
  #   dplyr::rename(`Student Work (pre)` = n) |>
  #   dplyr::mutate(`Student Work (post)` = 0) |>
  #   dplyr::rename(site = dropdown2)
  
  knowledge_assessments_count <- purrr::map2_dfr(
    knowledge_assessment_ids$id, knowledge_assessment_ids$name,
    ~ knowledge_assessment_n(.x, .y, site = dropdown2)
  ) |>
    dplyr::filter(!is.na(site)) |>
    dplyr::group_by(site) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(.x, na.rm = TRUE))) |>
    dplyr::ungroup() |>
    dplyr::mutate(dplyr::across(!site, ~ na_if(.x, 0))) |>
    ny_manual_entry_adjust(dropdown = dropdown)
  
  main_sheet <- read_sheet("1hIAg3aNVXwSJGi14QaOblVFbPwwTgpAvj88TeGKD8zA",
                           sheet = dropdown,
                           range = "A:A") |>
    dplyr::rename(site = Site)
  
  ### Conditional for if there is no data ###
  if ((nrow(session_survey_count) + nrow(course_survey_count) + nrow(ongoing_coaching_survey_count) + nrow(end_coach_survey_count) +
      nrow(educator_count) + nrow(classroom_obs_count) + nrow(knowledge_assessments_count)) == 0) {
    sites_count_df <- main_sheet |>
      dplyr::mutate(`No Data` = "No data has been collected here yet - reach out to the L&R team if you want to know more.") |>
      dplyr::rename(Site = site)
  } else {
    sites_count_df <- main_sheet |>
      dplyr::full_join(session_survey_count, by = "site") |>
      dplyr::full_join(course_survey_count, by = "site") |>
      dplyr::full_join(ongoing_coaching_survey_count, by = "site") |>
      dplyr::full_join(end_coach_survey_count, by = "site") |>
      dplyr::full_join(educator_count, by = "site") |>
      dplyr::full_join(classroom_obs_count, by = "site") |>
      # dplyr::left_join(final_student_survey_count, by = "site") |>
      # dplyr::left_join(student_work_count, by = "site") |>
      dplyr::full_join(knowledge_assessments_count, by = "site") |>
      janitor::remove_empty("cols") |>
      dplyr::rename(Site = site) |>
      dplyr::mutate(other = ifelse(Site == "Other", TRUE, FALSE)) |>
      dplyr::group_by(other) |>
      dplyr::arrange(Site, .by_group = TRUE) |>
      dplyr::ungroup() |>
      dplyr::select(-other)
  }
  
  range_clear(ss = "1hIAg3aNVXwSJGi14QaOblVFbPwwTgpAvj88TeGKD8zA",
              sheet = dropdown,
              range = "A1:L100",
              reformat = FALSE)
  
  col_range <- paste0("A1:", LETTERS[ncol(sites_count_df)], nrow(sites_count_df) + 1)
  
  range_write(sites_count_df,
              ss = "1hIAg3aNVXwSJGi14QaOblVFbPwwTgpAvj88TeGKD8zA",
              sheet = dropdown,
              col_names = TRUE,
              reformat = FALSE,
              range = col_range)
  
}

dropdown_count(dropdown = "NY_D6", dropdown2 = "district6")
dropdown_count(dropdown = "NY_D9", dropdown2 = "district9")
dropdown_count(dropdown = "NY_D11", dropdown2 = "district11")
dropdown_count(dropdown = "NY_D12", dropdown2 = "district12")
dropdown_count(dropdown = "NY_D16", dropdown2 = "district16")
dropdown_count(dropdown = "NY_D75", dropdown2 = "district75")
dropdown_count(dropdown = "NY_Rochester City Schools", dropdown2 = "rochester")
dropdown_count(dropdown = "NY_Ascend Charter Schools", dropdown2 = "ny_ascend")
dropdown_count(dropdown = "IL_CPS Network 4", dropdown2 = "network4")
dropdown_count(dropdown = "IL_CPS Network 7", dropdown2 = "network7")
dropdown_count(dropdown = "IL_CPS Network 12", dropdown2 = "network12")
dropdown_count(dropdown = "MA_DESE", dropdown2 = "ma_dese")
dropdown_count(dropdown = "AR_Blytheville School District", dropdown2 = "ar_blytheville")
dropdown_count(dropdown = "AR_Friendship Aspire Academy", dropdown2 = "ar_friendship")
dropdown_count(dropdown = "AR_Hope Public Schools", dropdown2 = "ar_hope")
dropdown_count(dropdown = "AR_Osceola School District", dropdown2 = "ar_osceola")
dropdown_count(dropdown = "LA_Pointe Coupee", dropdown2 = "la_pointe_coupee")

