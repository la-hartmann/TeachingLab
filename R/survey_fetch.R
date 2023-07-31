#' @title End of Session Dashboard Data
#' @description Gets dashboard data by reading it in from data folder
#' @param update FALSE, whether or not to update
#' @param year "21_22" or "22_23"
#' @return Returns a tibble
#' @export
get_session_survey <- function(update = FALSE, year = "22_23") {
  if (year == "22_23") {
    session_survey <- qualtRics::fetch_survey(
      surveyID = "SV_djt8w6zgigaNq0C",
      verbose = FALSE,
      include_display_order = FALSE,
      force_request = update
    ) |>
      dplyr::filter(last_session_or_not == "Yes - there will be more sessions for this PL course or coaching support." & course != "Coaching" & Finished == TRUE)
  } else if (update == FALSE & year == "21_22") {
    session_survey <- readr::read_rds("data/sy21_22/session_survey_21_22data.rds")
  } else if (update == TRUE & year == "21_22") {
    print("This code can unfortunately no longer be run due to the cancellation of the SurveyMonkey contract.")
  }

  write.csv(session_survey, glue::glue("data/sy{year}/session_survey.csv"))

  return(session_survey)
}

#' @title End of Course Dashboard Data
#' @description Gets dashboard data by reading it in from data folder
#' @param update FALSE, optional to update end of course data or not
#' @param year "21_22" or "22_23"
#' @return Returns a tibble
#' @export
get_course_survey <- function(update = FALSE, year = "22_23") {
  if (year == "22_23") {
    course_survey <- qualtRics::fetch_survey(
      surveyID = "SV_djt8w6zgigaNq0C",
      verbose = FALSE,
      include_display_order = FALSE,
      force_request = update
    ) |>
      dplyr::filter(last_session_or_not != "Yes - there will be more sessions for this PL course or coaching support." & course != "Coaching" & Finished == TRUE)
  } else if (update == FALSE & year == "21_22") {
    course_survey <- readr::read_rds(file = "data/merged/course_surveymonkey.rds")
  } else if (update == TRUE & year == "21_22") {
    print("This code can unfortunately no longer be run due to the cancellation of the SurveyMonkey contract.")
  }

  write.csv(course_survey, glue::glue("data/sy{year}/course_survey.csv"))

  return(course_survey)
}

#' @title Student Survey Data
#' @description Gets data from Student Survey
#' @param update FALSE whether or not to update the data
#' @param year "21_22" or "22_23"
#' @return Returns a tibble
#' @export
get_student_survey <- function(update = FALSE, year = "22_23") {
  if (year == "22_23") {
    student_survey <- qualtRics::fetch_survey(
      surveyID = "SV_9uze2faHuIf3vP8",
      verbose = FALSE,
      convert = FALSE,
      include_display_order = FALSE,
      force_request = update
    ) |>
      dplyr::filter(Finished == TRUE) |>
      dplyr::mutate(
        eic = FALSE,
        site = as.character(site),
        site = dplyr::case_when(
          site == "New Mexico" ~ "NM_NM Public Education Department",
          !is.na(network4) ~ "IL_Chicago Public Schools_Network 4",
          !is.na(network7) ~ "IL_Chicago Public Schools_Network 7",
          !is.na(network12) ~ "IL_Chicago Public Schools_Network 12"
        ),
        prepost = dplyr::case_when(
          RecordedDate < as.Date("2023-02-12") ~ "Pre",
          RecordedDate > as.Date("2023-02-12") ~ "Post"
        ),
        prepost = factor(prepost, levels = c("Pre", "Post"))
      )

    eic_student_survey <- qualtRics::fetch_survey(
      surveyID = "SV_8f9l21n6ML58WFM",
      convert = FALSE,
      verbose = FALSE,
      include_display_order = FALSE,
      force_request = update
    ) |>
      dplyr::filter(Finished == TRUE) |>
      dplyr::mutate(
        eic = TRUE,
        site = as.character(site),
        site = stringr::str_replace_all(site, c(
          "Rochester City School District" = "NY_Rochester City Schools",
          "NYC District 11" = "NY_D11"
        )),
        grade_level = readr::parse_number(as.character(grade_level)),
        prepost = dplyr::case_when(
          RecordedDate < as.Date("2023-04-01") ~ "Pre",
          RecordedDate >= as.Date("2023-04-01") ~ "Post"
        ),
        prepost = factor(prepost, levels = c("Pre", "Post"))
      )

    student_survey_coalesced <- student_survey |>
      dplyr::full_join(eic_student_survey)
  } else if (year == "21_22" & update == FALSE) {
    student_survey_coalesced <- readr::read_rds("data/sy21_22/student_survey.rds")
  } else if (update == TRUE & year == "21_22") {
    print("This code can unfortunately no longer be run due to the cancellation of the SurveyMonkey contract.")
  }

  return(student_survey_coalesced)
}


#' @title Diagnostic Survey Update
#' @description Get the diagnostic survey
#' @param update FALSE, optional updating
#' @param year "21_22" or "22_23"
#' @return A tibble
#' @export
get_diagnostic_survey <- function(update = FALSE, year = "22_23") {
  if (year == "22_23") {
    nm_diagnostic <- qualtRics::fetch_survey(
      surveyID = "SV_3a2OM4f9j85EuyO",
      verbose = FALSE,
      include_display_order = FALSE,
      force_request = update
    ) |>
      dplyr::filter(Finished == TRUE) |>
      dplyr::mutate(dplyr::across(dplyr::where(is.factor), ~ dplyr::na_if(as.character(.x), "NA")),
        email = tolower(email),
        prepost = "Pre",
        prepost = factor(prepost, levels = c("Pre", "Post")),
        site = "NM_NM Public Education Department",
        teaching_experience = dplyr::case_when(
          teaching_experience <= 10 ~ "1-10",
          teaching_experience > 10 & teaching_experience <= 19 ~ "11-20",
          teaching_experience > 19 & teaching_experience <= 29 ~ "21-30",
          teaching_experience > 29 & teaching_experience <= 39 ~ "31-40",
          teaching_experience > 39 & teaching_experience <= 49 ~ "41-50"
        ),
        teaching_experience = as.character(teaching_experience)
      ) |>
      dplyr::group_by(email) |>
      dplyr::filter(row_number() == 1) |>
      dplyr::ungroup()


    diagnostic_final <- qualtRics::fetch_survey(
      surveyID = "SV_8vrKtPDtqQFbiBM",
      verbose = FALSE,
      include_display_order = FALSE,
      force_request = update
    ) |>
      suppressWarnings() |>
      dplyr::mutate(dplyr::across(dplyr::where(is.factor), ~ dplyr::na_if(as.character(.x), "NA")),
        prepost = "Pre",
        prepost = factor(prepost, levels = c("Pre", "Post"))
      ) |>
      dplyr::filter(Finished == TRUE & is.na(future_location) &
        !(RecordedDate >= as.Date("2022-11-01") & site == "TX_RAISE Rice University") &
        !(RecordedDate >= as.Date("2023-04-15") & site == "AR_Arkansas DOE")) |> # last part here gets rid of TX_RAISE follow up from initial and Ar_Arkansas DOE
      dplyr::bind_rows(nm_diagnostic)
  } else if (update == FALSE & year == "21_22") {
    diagnostic_final <- readr::read_rds("data/sy21_22/diagnostic.rds")
  } else if (update == TRUE & year == "21_22") {
    print("This code can unfortunately no longer be run due to the cancellation of the SurveyMonkey contract.")
  }

  write.csv(diagnostic_final, glue::glue("data/sy{year}/educator_survey.csv"))
  return(diagnostic_final)
}

#' @title Coaching Participant Feedback Data
#' @description Gets data from Coaching Participant Feedback
#' @param update FALSE, whether or not to pull the updated version
#' @param year "21_22" or "22_23"
#' @return Returns a tibble
#' @export
get_ongoing_coaching <- function(update = FALSE, year = "22_23") {
  if (year == "22_23") {
    coaching_feedback_clean <- qualtRics::fetch_survey(
      surveyID = "SV_djt8w6zgigaNq0C",
      verbose = FALSE,
      include_display_order = FALSE,
      force_request = update
    ) |>
      dplyr::filter(last_session_or_not == "Yes - there will be more sessions for this PL course or coaching support." & course == "Coaching" & Finished == TRUE)
  } else if (update == FALSE & year == "21_22") {
    coaching_feedback_clean <- readr::read_rds("data/sy21_22/coaching_participant_feedback.rds")
  } else if (update == TRUE & year == "21_22") {
    print("This code can unfortunately no longer be run due to the cancellation of the SurveyMonkey contract.")
  }

  write.csv(coaching_feedback_clean, glue::glue("data/sy{year}/ongoing_coaching.csv"))

  return(coaching_feedback_clean)
}

#' @title Follow Up Educator Survey Data
#' @description Gets data from the Follow Up Educator Survey
#' @param update FALSE, whether or not to pull the updated version
#' @param year "21_22" or "22_23"
#' @return Returns a tibble
#' @export
get_followup_educator <- function(update = FALSE, year = "22_23") {
  if (year == "22_23") {
    nm_diagnostic <- qualtRics::fetch_survey(
      surveyID = "SV_3a2OM4f9j85EuyO",
      verbose = FALSE,
      include_display_order = FALSE,
      force_request = update
    ) |>
      dplyr::filter(Finished == TRUE) |>
      dplyr::mutate(dplyr::across(dplyr::where(is.factor), ~ dplyr::na_if(as.character(.x), "NA")),
        email = tolower(email),
        prepost = "Post",
        prepost = factor(prepost, levels = c("Pre", "Post")),
        site = "NM_NM Public Education Department",
        teaching_experience = as.character(teaching_experience)
      ) |>
      dplyr::group_by(email) |>
      dplyr::filter(row_number() == 2) |>
      dplyr::ungroup()

    followup_educator_general <- qualtRics::fetch_survey(
      surveyID = "SV_8vrKtPDtqQFbiBM",
      verbose = FALSE,
      include_display_order = FALSE,
      force_request = update
    ) |>
      dplyr::mutate(dplyr::across(tidyselect::where(is.factor), ~ dplyr::na_if(as.character(.x), "NA")),
        prepost = "Post",
        prepost = factor(prepost, levels = c("Pre", "Post"))
      ) |>
      dplyr::filter(Finished == TRUE & !is.na(future_location))

    tx_raise_additional_data <- qualtRics::fetch_survey(
      surveyID = "SV_8vrKtPDtqQFbiBM",
      verbose = FALSE,
      include_display_order = FALSE,
      force_request = update
    ) |>
      dplyr::filter(Finished == TRUE & RecordedDate >= as.Date("2022-11-01") & site == "TX_RAISE Rice University") |>
      dplyr::mutate(dplyr::across(dplyr::where(is.factor), ~ dplyr::na_if(as.character(.x), "NA")),
        prepost = "Post",
        prepost = factor(prepost, levels = c("Pre", "Post"))
      )

    arkansas_doe_additional_data <- qualtRics::fetch_survey(
      surveyID = "SV_8vrKtPDtqQFbiBM",
      verbose = FALSE,
      include_display_order = FALSE,
      force_request = update
    ) |>
      dplyr::filter(Finished == TRUE & RecordedDate >= as.Date("2023-04-15") & site == "AR_Arkansas DOE") |>
      dplyr::mutate(dplyr::across(dplyr::where(is.factor), ~ dplyr::na_if(as.character(.x), "NA")),
        prepost = "Post",
        prepost = factor(prepost, levels = c("Pre", "Post"))
      )

    followup_educator_clean <- followup_educator_general |>
      dplyr::bind_rows(tx_raise_additional_data, nm_diagnostic, arkansas_doe_additional_data)
  } else if (update == FALSE & year == "21_22") {
    followup_educator_clean <- readr::read_rds("data/sy21_22/followup_educator_survey.rds")
  } else if (update == TRUE & year == "21_22") {
    print("This code can unfortunately no longer be run due to the cancellation of the SurveyMonkey contract.")
  }

  return(followup_educator_clean)
}

#' @title Ongoing Coaching Feedback Survey Data
#' @description Gets data from the Ongoing Coaching Feedback Survey
#' @param update FALSE, whether or not to pull the updated version
#' @param year "21_22" or "22_23"
#' @return Returns a tibble
#' @export
get_end_coaching <- function(update = FALSE, year = "22_23") {
  if (year == "22_23") {
    end_coaching_survey_clean <- qualtRics::fetch_survey(
      surveyID = "SV_djt8w6zgigaNq0C",
      verbose = FALSE,
      include_display_order = FALSE,
      force_request = update
    ) |>
      dplyr::filter(last_session_or_not != "Yes - there will be more sessions for this PL course or coaching support." & course == "Coaching" & Finished == TRUE)
  } else if (update == FALSE & year == "21_22") {
    end_coaching_survey_clean <- readr::read_rds("data/sy21_22/ongoing_coaching_feedback.rds")
  } else if (update == TRUE & year == "21_22") {
    print("This code can unfortunately no longer be run due to the cancellation of the SurveyMonkey contract.")
  }

  write.csv(end_coaching_survey_clean, glue::glue("data/sy{year}/end_coaching.csv"))

  return(end_coaching_survey_clean)
}


#' @title Student Work Data
#' @description Gets metadata about student work files
#' @param update FALSE, whether or not to pull the updated version
#' @param year "21_22" or "22_23"
#' @return Returns a tibble
#' @export
get_student_work <- function(update = FALSE, year = "22_23") {
  if (year == "22_23") {
    student_work <- qualtRics::fetch_survey("SV_6nwa9Yb4OyXLji6",
      include_display_order = FALSE,
      verbose = FALSE,
      force_request = update
    ) |>
      suppressWarnings()
  }
}

#' @title Knowledge Assessments Update
#' @description Get the knowledge assessments survey
#' @param update FALSE, optional updating
#' @param year "21_22" or "22_23"
#' @return A tibble
#' @export
get_knowledge_assessments <- function(update = FALSE, year = "22_23") {
  if (year == "22_23" & update == TRUE) {
    ### List of ids and knowledge assessments ###
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
    
    ### Filter list for just those with responses ###
    
    knowledge_assessments_for_scoring <- knowledge_assessment_ids |>
      dplyr::filter(name %in% c(
        "ELA: Bootcamp - General",
        "Math: Bootcamp",
        "Math: Cycle of Inquiry I - Eliciting Student Thinking",
        "ELA: Bootcamp - Foundational Skills",
        "ELA General: Cycle of Inquiry - Complex Text",
        "Math: Learning Across the Domains",
        "Math: RAISE",
        "Math: Accelerating Learning",
        "School Leaders: Curriculum Adaptive Math/ELA (ILN)",
        "Language Standards/Conventions Knowledge_NYBlendedLit",
        "ELA Guidebooks: Cycle of Inquiry 2 - Writing & Language Skills",
        "Math: Cycle of Inquiry III - Facilitating Mathematical Discourse"
      )) |>
      ### This line is for the knowledge assessments dashboard, it can't handle the / ###
      dplyr::mutate(name = stringr::str_replace_all(name, "\\/", "|"))
    
    ### Getting Digital Nest for Joining ###
    educator_survey <- qualtRics::fetch_survey("SV_8vrKtPDtqQFbiBM",
                                               verbose = FALSE,
                                               force_request = update,
                                               include_display_order = FALSE
    )
    
    ### percent, prepost, site, know_assess ###
    all_knowledge_assessments <- purrr::map2_dfr(
      knowledge_assessments_for_scoring$id, knowledge_assessments_for_scoring$name,
      ~ TeachingLab::knowledge_assess_select_score(.x, .y)
    ) |>
      dplyr::bind_rows(
        ### Digital Nest School Leader Qs
        educator_survey |>
          dplyr::filter(Finished == TRUE & site == "US_Digital Nest") |>
          dplyr::mutate(
            id = paste0(tolower(initials), dob),
            know_assess = "US_Digital Nest School Leaders"
          ) |>
          dplyr::group_by(id) |>
          dplyr::mutate(
            n_response = dplyr::n(), # Get number of responses by person, sometimes there are more than 2 :/
            maxdate = max(RecordedDate), # Get max date of creation for most recent response
            matched = dplyr::if_else(n_response > 1 & maxdate == RecordedDate, "post", "pre")
          ) |> # Define as post for matched if more than 1 response and date is max of date_created
          dplyr::mutate(matched = factor(matched, levels = c("pre", "post"))) |> # Make matched a factor
          dplyr::mutate(prepost = dplyr::if_else(RecordedDate >= maxdate & n_response > 1, "post", "pre")) |> # Make pre and post defined by pre-October and post-October
          dplyr::mutate(prepost = factor(prepost, levels = c("pre", "post"))) |> # Make prepost a factor
          dplyr::ungroup() |>
          dplyr::mutate(score = SC0 / max(SC0, na.rm = T)) |>
          dplyr::select(percent = score, prepost, site, know_assess) |>
          tidyr::drop_na(percent),
        ### Math ANA ###
        educator_survey |>
          dplyr::filter(Finished == TRUE & RecordedDate <= as.Date("2023-01-01")) |>
          tidyr::drop_na(hqim1_1) |>
          dplyr::mutate(
            id = paste0(tolower(initials), dob),
            know_assess = "Math ANA"
          ) |>
          dplyr::group_by(id) |>
          dplyr::mutate(
            n_response = dplyr::n(), # Get number of responses by person, sometimes there are more than 2 :/
            maxdate = max(RecordedDate), # Get max date of creation for most recent response
            matched = dplyr::if_else(n_response > 1 & maxdate == RecordedDate, "post", "pre")
          ) |> # Define as post for matched if more than 1 response and date is max of date_created
          dplyr::mutate(matched = factor(matched, levels = c("pre", "post"))) |> # Make matched a factor
          dplyr::mutate(prepost = dplyr::if_else(RecordedDate >= maxdate & n_response > 1, "post", "pre")) |> # Make pre and post defined by pre-October and post-October
          dplyr::mutate(prepost = factor(prepost, levels = c("pre", "post"))) |> # Make prepost a factor
          dplyr::ungroup() |>
          dplyr::mutate(score = SC0 / max(SC0, na.rm = T)) |>
          dplyr::select(percent = score, prepost, site, know_assess) |>
          tidyr::drop_na(percent),
        ### ELA ANA ###
        educator_survey |>
          dplyr::filter(Finished == TRUE & RecordedDate <= as.Date("2023-01-01")) |>
          tidyr::drop_na(ela_bc_1_1) |>
          dplyr::mutate(
            id = paste0(tolower(initials), dob),
            know_assess = "ELA ANA"
          ) |>
          dplyr::group_by(id) |>
          dplyr::mutate(
            n_response = dplyr::n(), # Get number of responses by person, sometimes there are more than 2 :/
            maxdate = max(RecordedDate), # Get max date of creation for most recent response
            matched = dplyr::if_else(n_response > 1 & maxdate == RecordedDate, "post", "pre")
          ) |> # Define as post for matched if more than 1 response and date is max of date_created
          dplyr::mutate(matched = factor(matched, levels = c("pre", "post"))) |> # Make matched a factor
          dplyr::mutate(prepost = dplyr::if_else(RecordedDate >= maxdate & n_response > 1, "post", "pre")) |> # Make pre and post defined by pre-October and post-October
          dplyr::mutate(prepost = factor(prepost, levels = c("pre", "post"))) |> # Make prepost a factor
          dplyr::ungroup() |>
          dplyr::mutate(score = SC0 / max(SC0, na.rm = T)) |>
          dplyr::select(percent = score, prepost, site, know_assess) |>
          tidyr::drop_na(percent),
        ### ELA K-2 ANA ###
        educator_survey |>
          dplyr::filter(Finished == TRUE & RecordedDate <= as.Date("2023-01-01")) |>
          tidyr::drop_na(k2_ela1_1) |>
          dplyr::mutate(
            id = paste0(tolower(initials), dob),
            know_assess = "ELA K-2 ANA"
          ) |>
          dplyr::group_by(id) |>
          dplyr::mutate(
            n_response = dplyr::n(), # Get number of responses by person, sometimes there are more than 2 :/
            maxdate = max(RecordedDate), # Get max date of creation for most recent response
            matched = dplyr::if_else(n_response > 1 & maxdate == RecordedDate, "post", "pre")
          ) |> # Define as post for matched if more than 1 response and date is max of date_created
          dplyr::mutate(matched = factor(matched, levels = c("pre", "post"))) |> # Make matched a factor
          dplyr::mutate(prepost = dplyr::if_else(RecordedDate >= maxdate & n_response > 1, "post", "pre")) |> # Make pre and post defined by pre-October and post-October
          dplyr::mutate(prepost = factor(prepost, levels = c("pre", "post"))) |> # Make prepost a factor
          dplyr::ungroup() |>
          dplyr::mutate(score = SC0 / max(SC0, na.rm = TRUE)) |>
          dplyr::select(percent = score, prepost, site, know_assess) |>
          tidyr::drop_na(percent)
      )
  } else if (year == "22_23" & update == FALSE) {
    all_knowledge_assessments <- readr::read_rds("data/sy22_23/knowledge_assessments_22_23.rds")
  } else if (update == FALSE & year == "21_22") {
    all_knowledge_assessments <- readr::read_rds("data/sy21_22/knowledge_assessments.rds")
  } else if (update == TRUE & year == "21_22") {
    print("Can no longer update due to loss of SurveyMonkey license!")
  }
  
  return(all_knowledge_assessments)
}