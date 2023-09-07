library(dplyr)
library(googlesheets4)
library(qualtRics)
library(tibble)
library(tidyr)

### If all surveys needs to be checked ###
# surveys <- qualtRics::all_surveys()

### HAVE TO GET RESPONSES HERE TO FILTER FOR END OF COURSE/END OF SESSION ###
participant_feedback <- qualtRics::fetch_survey(
  surveyID = "SV_djt8w6zgigaNq0C",
  verbose = TRUE,
  include_display_order = FALSE
) |>
  dplyr::filter(Finished == TRUE)
### End of Session Survey Count ###
session_survey_count <- participant_feedback |>
  # filter(last_session_or_not == "No - today was not the final session.") |>
  dplyr::filter(!is.na(facilitator1)) |>
  dplyr::group_by(site) |>
  dplyr::count() |>
  dplyr::rename(`End of Session` = n)
### Just district 9 end of session count ###
district_9_session_count <- participant_feedback |>
  dplyr::filter(!is.na(facilitator1)) |>
  dplyr::group_by(district9) |>
  dplyr::count() |>
  tidyr::drop_na() |>
  dplyr::rename(`End of Session` = n)
### Just district 11 end of session count ###
district_11_session_count <- participant_feedback |>
  dplyr::filter(!is.na(facilitator1)) |>
  dplyr::group_by(district11) |>
  dplyr::count() |>
  dplyr::mutate(district11 = as.character(district11)) |>
  tidyr::drop_na() |>
  dplyr::rename(`End of Session` = n)

##### End of End of session count ######

### End of Course Survey Count ###
course_survey_count <- participant_feedback |>
  dplyr::filter(last_session_or_not == "No - this was the final session for this PL course (e.g., last day of Bootcamp, close of Inquiry cycle) or coaching.") |>
  dplyr::group_by(site) |>
  dplyr::count() |>
  dplyr::rename(`End of Course` = n)
### Just district 9 end of course count ###
district_9_course_count <- participant_feedback |>
  dplyr::filter(last_session_or_not == "No - this was the final session for this PL course (e.g., last day of Bootcamp, close of Inquiry cycle) or coaching.") |>
  dplyr::group_by(district9) |>
  dplyr::count() |>
  tidyr::drop_na() |>
  dplyr::rename(`End of Course` = n)
### Just district 11 end of course count ###
district_11_course_count <- participant_feedback |>
  dplyr::filter(last_session_or_not == "No - this was the final session for this PL course (e.g., last day of Bootcamp, close of Inquiry cycle) or coaching.") |>
  dplyr::group_by(district11) |>
  dplyr::count() |>
  dplyr::mutate(district11 = as.character(district11)) |>
  tidyr::drop_na() |>
  dplyr::rename(`End of Course` = n)
##### End of End of course count ######

### Ongoing coaching count ###
ongoing_coaching_count <- participant_feedback |>
  dplyr::filter(last_session_or_not == "Yes - there will be more sessions for this PL course or coaching cycle." &
    course == "Coaching") |>
  dplyr::mutate(site = replace_na(as.character(site), "Other")) |>
  dplyr::group_by(site) |>
  dplyr::count(sort = T) |>
  dplyr::rename(`Ongoing Coaching` = n)

district9_ongoing_coaching_count <- participant_feedback |>
  dplyr::filter(last_session_or_not == "Yes - there will be more sessions for this PL course or coaching cycle." & course == "Coaching") |>
  dplyr::group_by(district9) |>
  dplyr::count(sort = T) |>
  tidyr::drop_na() |>
  dplyr::rename(`Ongoing Coaching` = n)

district11_ongoing_coaching_count <- participant_feedback |>
  dplyr::filter(last_session_or_not == "Yes - there will be more sessions for this PL course or coaching cycle." & course == "Coaching") |>
  dplyr::group_by(district11) |>
  dplyr::count(sort = T) |>
  dplyr::mutate(district11 = as.character(district11)) |>
  tidyr::drop_na() |>
  dplyr::rename(`Ongoing Coaching` = n)
##### End of Ongoing Coaching count ######

### End of coaching count ###
end_coaching_count <- participant_feedback |>
  dplyr::filter(last_session_or_not != "Yes - there will be more sessions for this PL course or coaching cycle." & course == "Coaching") |>
  dplyr::mutate(site = replace_na(as.character(site), "Other")) |>
  dplyr::group_by(site) |>
  dplyr::count(sort = T) |>
  dplyr::rename(`End of Coaching` = n)

district9_end_coaching_count <- participant_feedback |>
  dplyr::filter(last_session_or_not != "Yes - there will be more sessions for this PL course or coaching cycle." & course == "Coaching") |>
  dplyr::group_by(district9) |>
  dplyr::count(sort = T) |>
  tidyr::drop_na() |>
  dplyr::rename(`End of Coaching` = n)

district11_end_coaching_count <- participant_feedback |>
  dplyr::filter(last_session_or_not != "Yes - there will be more sessions for this PL course or coaching cycle." & course == "Coaching") |>
  dplyr::group_by(district11) |>
  dplyr::count(sort = T) |>
  dplyr::mutate(district11 = as.character(district11)) |>
  tidyr::drop_na() |>
  dplyr::rename(`End of Coaching` = n)
##### End of end of Coaching count ######

### Educator Survey & Follow Up Survey Counting ###
diagnostic_survey <- TeachingLab::get_diagnostic_survey(update = FALSE, year = "22_23")
followup_survey <- TeachingLab::get_followup_educator(update = FALSE, year = "22_23")

diagnostic_survey_count <- diagnostic_survey |>
  dplyr::mutate(site = replace_na(as.character(site), "Other")) |>
  dplyr::group_by(site) |>
  dplyr::count(sort = T) |>
  dplyr::rename(`Diagnostic (pre)` = n)
followup_survey_count <- followup_survey |>
  dplyr::mutate(site = replace_na(as.character(site), "Other")) |>
  dplyr::group_by(site) |>
  dplyr::count(sort = T) |>
  dplyr::rename(`Follow up (post)` = n)

district9_diagnostic_count <- diagnostic_survey |>
  dplyr::group_by(district9) |>
  dplyr::count(sort = T) |>
  tidyr::drop_na() |>
  dplyr::rename(`Diagnostic (pre)` = n)
district9_followup_count <- followup_educator |>
  dplyr::group_by(district9) |>
  dplyr::count(sort = T) |>
  tidyr::drop_na() |>
  dplyr::rename(`Follow up (post)` = n)

district11_diagnostic_count <- diagnostic_survey |>
  dplyr::group_by(district11) |>
  dplyr::count(sort = T) |>
  tidyr::drop_na() |>
  dplyr::rename(`Diagnostic (pre)` = n)
district11_followup_count <- followup_educator |>
  dplyr::group_by(district11) |>
  dplyr::count(sort = T) |>
  tidyr::drop_na() |>
  dplyr::rename(`Follow up (post)` = n)

##### End of Educator Survey count ######

### Classroom Observation/IPG count ###
ipg_forms <- TeachingLab::get_ipg_forms(update = TRUE, year = "22_23")

### ISSUE: How to relabel baseline/mid-year to round 1, 2, 3, 4, 5??? ###
classroom_obs_count <- ipg_forms |>
  dplyr::mutate(
    site = tidyr::replace_na(as.character(site), "Other"),
    dplyr::across(c(`direct_to_ts_obs`, not_direct_to_ts), ~ as.character(.x)),
    round = dplyr::coalesce(`direct_to_ts_obs`, not_direct_to_ts)
  ) |>
  dplyr::mutate(round = stringr::str_replace_all(round, c(
    "Baseline \\(first observation of the year\\)" = "First site visit",
    "Mid-year \\(middle of service, if applicable\\)" = "Third site visit",
    "Ongoing" = "Fourth site visit",
    "End of year \\(last observation of the year\\)" = "Fifth site visit"
  ))) |>
  dplyr::group_by(site, round) |>
  dplyr::count(sort = T) |>
  dplyr::mutate(
    round = tidyr::replace_na(round, "Fifth site visit"),
    round = factor(round, levels = c(
      "First site visit",
      "Second site visit",
      "Third site visit",
      "Fourth site visit",
      "Fifth site visit"
    ))
  ) |>
  dplyr::group_by(site, round) |>
  dplyr::summarise(n = sum(n, na.rm = T)) |>
  dplyr::ungroup() |>
  tidyr::pivot_wider(names_from = round, values_from = n, names_sort = TRUE)

district9_classroom_obs_count <- ipg_forms |>
  dplyr::mutate(
    site = tidyr::replace_na(as.character(site), "Other"),
    dplyr::across(c(`direct_to_ts_obs`, not_direct_to_ts), ~ as.character(.x)),
    round = dplyr::coalesce(`direct_to_ts_obs`, not_direct_to_ts)
  ) |>
  dplyr::mutate(round = stringr::str_replace_all(round, c(
    "Baseline \\(first observation of the year\\)" = "First site visit",
    "Mid-year \\(middle of service, if applicable\\)" = "Third site visit",
    "Ongoing" = "Fourth site visit",
    "End of year \\(last observation of the year\\)" = "Fifth site visit"
  ))) |>
  dplyr::group_by(district9, round) |>
  dplyr::count(sort = T) |>
  tidyr::drop_na(district9) |>
  dplyr::mutate(
    round = tidyr::replace_na(round, "Fifth site visit"),
    round = factor(round, levels = c(
      "First site visit",
      "Second site visit",
      "Third site visit",
      "Fourth site visit",
      "Fifth site visit"
    ))
  ) |>
  tidyr::pivot_wider(names_from = round, values_from = n, names_sort = TRUE)

district11_classroom_obs_count <- ipg_forms |>
  dplyr::mutate(
    site = tidyr::replace_na(as.character(site), "Other"),
    dplyr::across(c(`direct_to_ts_obs`, not_direct_to_ts), ~ as.character(.x)),
    round = dplyr::coalesce(`direct_to_ts_obs`, not_direct_to_ts)
  ) |>
  dplyr::mutate(round = stringr::str_replace_all(round, c(
    "Baseline \\(first observation of the year\\)" = "First site visit",
    "Mid-year \\(middle of service, if applicable\\)" = "Third site visit",
    "Ongoing" = "Fourth site visit",
    "End of year \\(last observation of the year\\)" = "Fifth site visit"
  ))) |>
  dplyr::group_by(district11, round) |>
  dplyr::count(sort = T) |>
  tidyr::drop_na(district11) |>
  dplyr::mutate(
    round = tidyr::replace_na(round, "Fourth site visit"),
    round = factor(round, levels = c(
      "First site visit",
      "Second site visit",
      "Third site visit",
      "Fourth site visit",
      "Fifth site visit"
    ))
  ) |>
  tidyr::pivot_wider(names_from = round, values_from = n, names_sort = TRUE)

if (!"Fourth site visit" %in% colnames(district11_classroom_obs_count) & nrow(district11_classroom_obs_count) > 0) {
  district11_classroom_obs_count <- dplyr::mutate(district11_classroom_obs_count,
    `Fourth site visit` = NA
  ) |>
    dplyr::relocate(`Fourth site visit`, .before = `Fifth site visit`)
}

##### End of Classroom Obs count ######

### Student Survey Count ###
eic_student_survey <- TeachingLab::get_student_survey(update = TRUE, year = "22_23") |>
    dplyr::filter(eic == TRUE) |>
    janitor::remove_empty("cols")

eic_student_survey_count <- eic_student_survey |>
  dplyr::mutate(site = stringr::str_replace_all(site, c(
    "Rochester City School District" = "NY_Rochester City School District",
    "NYC District 11" = "NY_D11"
  ))) |>
  tidyr::drop_na(site) |>
  dplyr::group_by(site, prepost) |>
  dplyr::count(sort = T) |>
  tidyr::pivot_wider(names_from = prepost, values_from = n) |>
  dplyr::rename(`Student Survey pre` = Pre,
                `Student Survey post` = Post)

d11_eic_student_survey_count <- eic_student_survey |>
  dplyr::filter(!is.na(district11)) |>
  dplyr::group_by(district11, prepost) |>
  dplyr::count(sort = T) |>
  tidyr::pivot_wider(names_from = prepost, values_from = n) |>
  dplyr::rename(`Student Survey pre` = Pre,
                `Student Survey post` = Post)

student_survey <- TeachingLab::get_student_survey(update = FALSE, year = "22_23") |>
    dplyr::filter(eic == FALSE) |>
    janitor::remove_empty("cols")

student_survey_count <- student_survey |>
  tidyr::drop_na(site) |>
  dplyr::group_by(site, prepost) |>
  dplyr::count(sort = T) |>
  tidyr::pivot_wider(names_from = prepost, values_from = n) |>
  dplyr::rename(`Student Survey pre` = Pre,
                `Student Survey post` = Post) |>
  dplyr::bind_rows(eic_student_survey_count)

student_work <- googlesheets4::read_sheet(ss = "15ixca0QKloZtYLcmj_9Uc20zdQ5FE6pSVj3EBamLoiI",
                                          sheet = "Student Work Scores")

student_work_count <- student_work |>
  dplyr::filter(!Prepost %in% c("NA", "DUPLICATE") & !is.na(Prepost)) |>
  dplyr::mutate(Site = replace_na(as.character(Site), "Other")) |>
  dplyr::group_by(Site, Prepost) |>
  dplyr::count(sort = T) |>
  tidyr::pivot_wider(names_from = Prepost, values_from = n) |>
  dplyr::rename(`Student work samples round 1` = Pre,
                `Student work samples round 2` = Post)

student_work_count_district_9 <- student_work |>
  dplyr::filter(!Prepost %in% c("NA", "DUPLICATE") & !is.na(Prepost) & Site == "NY_D9" & !is.na(`District 9`)) |>
  dplyr::group_by(district9 = `District 9`, Prepost) |>
  dplyr::count(sort = T) |>
  tidyr::pivot_wider(names_from = Prepost, values_from = n) |>
  dplyr::rename(`Student work samples round 1` = Pre,
                `Student work samples round 2` = Post)

student_work_count_district_11 <- student_work |>
  dplyr::filter(!Prepost %in% c("NA", "DUPLICATE") & !is.na(Prepost) & Site == "NY_D11" & !is.na(`District 11`)) |>
  dplyr::group_by(district11 = `District 11`, Prepost) |>
  dplyr::count(sort = T) |>
  tidyr::pivot_wider(names_from = Prepost, values_from = n) |>
  dplyr::rename(`Student work samples round 1` = Pre,
                `Student work samples round 2` = Post)
###### End of Student Survey/Student Work Survey Count ######

### Knowledge Assessment Counting ###
knowledge_assessment_ids <- readr::read_rds(here::here("data/sy22_23/knowledge_assessment_ids.rds"))

### Function to get a data frame of site, n, knowledge assessment name ###
knowledge_assessment_n <- function(survey_id, survey_name) {
  print(paste0("Getting... ", survey_name))
  ### Get Survey ###
  know_assess <- qualtRics::fetch_survey(surveyID = survey_id, 
                                         verbose = TRUE, 
                                         include_display_order = FALSE) |>
    suppressWarnings()

  ### All necessary knowledge_assessment_columns ###
  if (nrow(know_assess |> dplyr::filter(Finished == TRUE & RecordedDate >= as.Date("2023-07-01"))) >= 1) {
    ### Ensure there is a site column ###
    if ("site" %in% colnames(know_assess)) {
      ### Get Count of each knowledge assessment by site ###
      know_assess_count <- know_assess |>
        dplyr::mutate(
          id = paste0(tolower(initials), dob),
          Date = RecordedDate
        ) |>
        dplyr::filter(Finished == TRUE & !id %in% c("tst1000", "tst0001", "tst0000", "tst0010")) |>
        dplyr::group_by(id) |>
        dplyr::mutate(
          n_response = dplyr::n(), # Get number of responses by person, sometimes there are more than 2 :/
          maxdate = max(RecordedDate), # Get max date of creation for most recent response
          prepost = dplyr::if_else(RecordedDate >= maxdate & n_response > 1, "post", "pre"),
          prepost = factor(prepost, levels = c("pre", "post"))
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(id, prepost, site) |>
        dplyr::count(sort = T) |>
        dplyr::ungroup() |>
        tidyr::pivot_wider(names_from = "prepost", values_from = "n") |>
        dplyr::mutate(
          site = as.character(site)
        ) |>
        dplyr::select(-id)

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
    print("No responses yet!")
    NULL
  }
}

### Testing function ###
# test <- knowledge_assessment_n(survey_id = knowledge_assessment_ids$id[1],
#                                survey_name = knowledge_assessment_ids$name[1]) |>
#   view()

knowledge_assessment_count <- purrr::map2_dfr(
  knowledge_assessment_ids$id, knowledge_assessment_ids$name,
  ~ knowledge_assessment_n(.x, .y)
) |>
  dplyr::group_by(site) |>
  dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(.x, na.rm = TRUE))) |>
  dplyr::mutate(dplyr::across(!site, ~ na_if(.x, 0))) |>
  janitor::remove_empty("cols")

d11_d9_knowledge_assessment_n <- function(survey_id, survey_name, district_filter) {
  print(paste0("Getting... ", survey_name))
  ### Get Survey ###
  know_assess <- qualtRics::fetch_survey(surveyID = survey_id, 
                                         include_display_order = FALSE,
                                         verbose = FALSE) |>
    suppressWarnings()

  if (nrow(know_assess |> dplyr::filter(Finished == TRUE)) >= 1) {
    ### First rename
    know_assess <- know_assess |>
      dplyr::select(-site)
    if (district_filter == "district11") {
      print(know_assess$district11)
      names(know_assess)[names(know_assess) == "district11"] <- "site"
      print(know_assess$site)
    } else if (district_filter == "district9") {
      names(know_assess)[names(know_assess) == "district9"] <- "site"
    }

    # print(sum(!is.na(know_assess$site)))
    if (sum(!is.na(know_assess$site)) >= 1) {
      ### Get Count of each knowledge assessment by site ###
      know_assess_count <- know_assess |>
        dplyr::mutate(
          id = paste0(tolower(initials), dob),
          Date = RecordedDate,
          site = tidyr::replace_na(as.character(site), "Other")
        ) |>
        dplyr::filter(Finished == TRUE & !id %in% c("tst1000", "TST1000", "tst0000", "TST0000")) |>
        dplyr::group_by(id) |>
        dplyr::mutate(
          n_response = dplyr::n(), # Get number of responses by person, sometimes there are more than 2 :/
          maxdate = max(RecordedDate), # Get max date of creation for most recent response
          prepost = dplyr::if_else(RecordedDate >= maxdate & n_response > 1, "post", "pre"),
          prepost = factor(prepost, levels = c("pre", "post"))
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(id, prepost, site) |>
        dplyr::count(sort = T) |>
        dplyr::ungroup() |>
        tidyr::pivot_wider(names_from = "prepost", values_from = "n") |>
        dplyr::select(-id)

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

      return(know_assess_count)
    }
  } else {
    print("No responses yet!")
    NULL
  }
}

d9_knowledge_assessment_count <- purrr::map2_dfr(
  knowledge_assessment_ids$id, knowledge_assessment_ids$name,
  ~ d11_d9_knowledge_assessment_n(.x, .y, district_filter = "district9")
) |>
  dplyr::group_by(site) |>
  dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(.x, na.rm = TRUE))) |>
  dplyr::mutate(dplyr::across(!site, ~ na_if(.x, 0))) |>
  janitor::remove_empty("cols")

test_func <- function(x) {
  sum(!is.na(fetch_survey(x)$district11))
}

if (any(map_dbl(knowledge_assessment_ids$id, test_func)) > 0) {
  d11_knowledge_assessment_count <- purrr::map2_dfr(
    knowledge_assessment_ids$id, knowledge_assessment_ids$name,
    ~ d11_d9_knowledge_assessment_n(.x, .y, district_filter = "district11")
  ) |>
    dplyr::group_by(site) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(.x, na.rm = TRUE))) |>
    dplyr::mutate(dplyr::across(!site, ~ dplyr::na_if(.x, 0))) |>
    janitor::remove_empty("cols")
}


### End of Knowledge Assessment Counting ###


### Adding sites List ###

current_sites <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/11jlo9UeWxZGwunhDb24hZBwAKc5b8ZKM9AYNWZaUyZY/edit#gid=1874068580",
  sheet = "NEW_FY23 Automation (Sites + Courses)",
  range = "C:C"
) |>
  dplyr::rename(site = `Site in Survey`) |>
  dplyr::mutate(site = stringr::str_replace_all(site, c("NM_NM PED" = "NM_NM Public Education Department")))

### All Data Write Section ###
data_collection_sy22_23 <- current_sites |>
  dplyr::left_join(session_survey_count) |>
  dplyr::left_join(course_survey_count) |>
  dplyr::left_join(ongoing_coaching_count, by = "site") |>
  dplyr::left_join(end_coaching_count, by = "site") |>
  dplyr::left_join(diagnostic_survey_count) |>
  dplyr::left_join(followup_survey_count) |>
  dplyr::left_join(classroom_obs_count, by = "site") |>
  dplyr::left_join(student_survey_count, by = "site") |>
  dplyr::left_join(student_work_count, by = c("site" = "Site")) |>
  dplyr::left_join(knowledge_assessment_count, by = c("site"))

### Add two to sheet length to get actual range for google sheet to be written ###
sheet_length <- nrow(data_collection_sy22_23) + 2
sheet_col_length <- paste0("A", LETTERS[(ncol(data_collection_sy22_23) - 26)])

googlesheets4::range_write(
  ss = "https://docs.google.com/spreadsheets/d/1fP1I1lugnfi-a5LpVgvtp1PtdRObColtvioTAFMtYqo/edit#gid=0",
  data = data_collection_sy22_23,
  sheet = "Tracker (n sizes) SY22-23",
  range = glue::glue("A2:{sheet_col_length}{sheet_length}"),
  reformat = F,
  col_names = T
)

###### End All Data Write Section #####

### D9 Write Section ###

district9_sites <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/11jlo9UeWxZGwunhDb24hZBwAKc5b8ZKM9AYNWZaUyZY/edit#gid=1874068580",
  sheet = "FY24 School selection for sites",
  range = "B:B"
) |>
  dplyr::rename(district9 = NY_D9) |>
  dplyr::bind_rows(tibble::tibble(district9 = "Other"))

d9_data_collection_sy22_23 <- district9_sites |>
  dplyr::left_join(district_9_session_count, by = "district9") |>
  dplyr::left_join(district_9_course_count) |>
  dplyr::left_join(district9_ongoing_coaching_count) |>
  dplyr::left_join(district9_end_coaching_count) |>
  dplyr::left_join(district9_diagnostic_count) |>
  dplyr::left_join(district9_followup_count) |>
  dplyr::left_join(district9_classroom_obs_count) |>
  tibble::add_column(`Student Survey pre` = NA, .after = "Fifth site visit") |>
  tibble::add_column(`Student Survey post` = NA, .after = "Student Survey pre") |>
  dplyr::left_join(student_work_count_district_9) |>
  dplyr::left_join(d9_knowledge_assessment_count, by = c("district9" = "site"))

### Add two to sheet length to get actual range for google sheet to be written ###
sheet_length <- nrow(d9_data_collection_sy22_23) + 2
d9_sheet_col_length <- LETTERS[ncol(d9_data_collection_sy22_23)]

googlesheets4::range_write(
  ss = "https://docs.google.com/spreadsheets/d/1fP1I1lugnfi-a5LpVgvtp1PtdRObColtvioTAFMtYqo/edit#gid=0",
  data = d9_data_collection_sy22_23,
  sheet = "District 9 (n sizes) SY22-23",
  range = glue::glue("A2:{d9_sheet_col_length}{sheet_length}"),
  reformat = F,
  col_names = T
)
###### End D9 Data Write Section #####

### D11 Write Section ###

district11_sites <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/11jlo9UeWxZGwunhDb24hZBwAKc5b8ZKM9AYNWZaUyZY/edit#gid=1874068580",
  sheet = "FY24 School selection for sites",
  range = "A:A"
) |>
  rename(district11 = NY_D11) |>
  bind_rows(tibble(district11 = "Other"))

d11_data_collection_sy22_23 <- district11_sites |>
  dplyr::left_join(district_11_session_count, by = "district11") |>
  dplyr::left_join(district_11_course_count) |>
  dplyr::left_join(district11_ongoing_coaching_count) |>
  dplyr::left_join(district11_end_coaching_count) |>
  dplyr::left_join(district11_diagnostic_count) |>
  tibble::add_column(`Follow up (post)` = NA, .after = "Diagnostic (pre)") |>
  dplyr::left_join(district11_classroom_obs_count) |>
  dplyr::left_join(d11_eic_student_survey_count) |>
  dplyr::left_join(student_work_count_district_11) |>
  (\(.) if (exists("d11_knowledge_assessment_count")) dplyr::left_join(., d11_knowledge_assessment_count, by = c("district11" = "site")) else .)()


### Add two to sheet length to get actual range for google sheet to be written ###
sheet_length <- nrow(d11_data_collection_sy22_23) + 2
d11_sheet_col_length <- LETTERS[ncol(d11_data_collection_sy22_23)]

googlesheets4::range_write(
  ss = "https://docs.google.com/spreadsheets/d/1fP1I1lugnfi-a5LpVgvtp1PtdRObColtvioTAFMtYqo/edit#gid=0",
  data = d11_data_collection_sy22_23,
  sheet = "District 11 (n sizes) SY22-23",
  range = glue::glue("A2:{d11_sheet_col_length}{sheet_length}"),
  reformat = F,
  col_names = T
)
###### End D11 Data Write Section #####
######################################## End Script ##################################################

