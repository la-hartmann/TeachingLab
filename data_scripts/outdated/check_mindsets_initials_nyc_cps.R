### Possible Improvements 
### - Greater functionalization of initials checking
### Make column selection automated by name

library(fedmatch)
library(googlesheets4)
library(qualtRics)
library(tidyverse)


### Read in Google Sheet ###
nyc_cps <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1WSQ2AvMZ4AwrC7SO78Yq2vF-UOcUnXQ8D82K1jbuOtE/edit#gid=1590040528",
  sheet = "Master list",
  skip = 1
)

### Read in educator survey survey ###
educator_survey <- TeachingLab::get_diagnostic_survey(update = FALSE, year = "22_23")

ipg_forms <- TeachingLab::get_ipg_forms(update = FALSE, year = "22_23")

student_work_survey <- TeachingLab::get_student_work(year = "22_23")

participant_feedback <- qualtRics::fetch_survey(
  surveyID = "SV_djt8w6zgigaNq0C",
  force_request = FALSE,
  verbose = FALSE,
  include_display_order = FALSE
)


### RPPL Qualtrics Surveys ###
# tl_treatment_nyc <- qualtRics::fetch_survey(
#   surveyID = "SV_eanFyxsWh8w14sC",
#   verbose = FALSE,
#   force_request = TRUE
# )
#
# tl_treatment_nyc_cps <- qualtRics::fetch_survey(
#   surveyID = "SV_5oKi7EimqAzKy7Y",
#   verbose = FALSE,
#   force_request = TRUE
# )
#
# tl_control <- qualtRics::fetch_survey(
#   surveyID = "SV_6lihuymOYtUfKS2",
#   verbose = FALSE,
#   force_request = TRUE
# )

### Get initials function in just First Initial, Second Initial (DG) format ####
get_initials <- function(teacher_name) {
  gsub("(?<=[A-Z])[^A-Z]+", "", teacher_name, perl = TRUE)
}

### Remove duplicate words function (except for special characters) ###
rem_dup_word <- function(x) {
  x <- paste0(unique(stringr::str_split(x, "/")[[1]]), collapse = "/")

  x
}

########################### Script starts here ######################################

######### Educator Survey Matching ##########
### Get just Chicago data from educator survey and reformat the initials ###
teacher_codes <- educator_survey |>
  dplyr::filter(Finished == TRUE & str_detect(site, "Chicago|NY_D")) |>
  dplyr::mutate(two_letter_initials = paste0(
    substr(toupper(initials), 1, 1),
    substr(toupper(initials), 3, 3)
  )) |>
  dplyr::mutate(
    dplyr::across(c(
      network4, network7, network12,
      district9, district11, district27
    ), ~ as.character(.x)),
    school = dplyr::coalesce(
      network4, network7, network12,
      district9, district11, district27
    )
  ) |>
  dplyr::select(site, content_area, two_letter_initials, initials, school)

###### Join in both NY and Chicago Initial sets #####
is_in_ed_survey <- nyc_cps |>
  dplyr::mutate(
    two_letter_initials_from_sheet = substr(get_initials(Teacher), 1, 2),
    School = ifelse(Site == "NY_D9", paste0(Site, "_0", stringr::str_remove(Site, "NY_D"), "X", School), School),
    School = ifelse(Site == "NY_D11", paste0(Site, "_", stringr::str_remove(Site, "NY_D"), "x", School), School),
    School = as.character(School)
  ) |>
  dplyr::select(Site, two_letter_initials_from_sheet, School) |>
  dplyr::left_join(teacher_codes |> dplyr::select(-content_area),
    by = c(
      "two_letter_initials_from_sheet" = "two_letter_initials",
      "Site" = "site",
      "School" = "school"
    ),
    multiple = "all",
    relationship = "many-to-many"
  ) |>
  # dplyr::mutate(Initials = dplyr::coalesce(initials.x, initials.y)) |>
  # dplyr::select(-initials.x, -initials.y) |>
  dplyr::group_by(two_letter_initials_from_sheet, Site, School) |>
  dplyr::mutate(initials_count = dplyr::n()) |>
  dplyr::mutate(name = ifelse(initials_count >= 1, "initials_dup", NA)) |>
  tidyr::pivot_wider(values_from = initials, names_from = name, values_fn = list) |>
  dplyr::mutate(
    initials_dup = toupper(stringr::str_remove_all(stringr::str_replace_all(stringr::str_remove_all(as.character(initials_dup), "c\\(|\\)"), '\\", \\"', "/"), '\\"')),
    initials_dup = purrr::map(initials_dup, rem_dup_word),
    initials_dup = ifelse(stringr::str_detect(initials_dup, "NA"), NA, as.character(initials_dup))
  ) |>
  tidyr::drop_na(initials_dup) |>
  dplyr::filter(initials_dup != "NA, NA" & initials_dup != "NA, NA, NA") |>
  dplyr::rename(
    `Teacher code` = initials_dup,
    Initials = two_letter_initials_from_sheet
  ) |>
  dplyr::select(-initials_count)

is_in_ed_survey_final <- nyc_cps |>
  dplyr::mutate(
    Initials = get_initials(Teacher),
    School = ifelse(Site == "NY_D9", paste0(Site, "_0", str_remove(Site, "NY_D"), "X", School), School),
    School = ifelse(Site == "NY_D11", paste0(Site, "_", str_remove(Site, "NY_D"), "x", School), School),
    School = as.character(School)
  ) |>
  dplyr::select(Site, Initials, School) |>
  dplyr::left_join(is_in_ed_survey) |>
  dplyr::mutate(correct_initials = ifelse(is.na(`Teacher code`), FALSE, TRUE))

### Get number of rows to write to sheet ###
rows_to_sheet <- nrow(is_in_ed_survey_final) + 2

### Make it so rows that are already marked as correct are not overwritten via combining old data TRUE/FALSE
### with new TRUE/FALSE ###
is_in_ed_survey_final_with_corrections <- is_in_ed_survey_final |>
  dplyr::select(`Teacher code`, correct_initials) |>
  dplyr::bind_cols(nyc_cps |> dplyr::select(corrected_code = `Teacher code`, `Diagnostic Educator Survey (Y/N)`)) |>
  dplyr::mutate(
    `Teacher code` = ifelse(correct_initials != `Diagnostic Educator Survey (Y/N)` & correct_initials == FALSE, corrected_code, `Teacher code`),
    correct_initials = ifelse(!is.na(corrected_code), `Diagnostic Educator Survey (Y/N)`, correct_initials)
  ) |>
  dplyr::select(`Teacher code`, correct_initials)

### Write Educator Survey Initials match to sheet ###
is_in_ed_survey_final_with_corrections |>
  googlesheets4::range_write(
    ss = "1WSQ2AvMZ4AwrC7SO78Yq2vF-UOcUnXQ8D82K1jbuOtE",
    sheet = "Master list",
    range = glue::glue("H3:I{rows_to_sheet}"),
    col_names = F,
    reformat = F
  )

##### End of matching for Educator Survey Initials ######

###### IPG Forms matching start #########
### IPG Forms reformat last name get school and site ###
ipg_forms_match <- ipg_forms |>
  dplyr::filter(Finished == TRUE & stringr::str_detect(site, "NY_D|Chi")) |>
  dplyr::mutate(
    last_name = stringr::str_to_title(sub(".* ", "", teacher)),
    dplyr::across(c(
      network4, network7, network12,
      district9, district11
    ), ~ as.character(.x)),
    school = dplyr::coalesce(
      network4, network7, network12,
      district9, district11
    )
  ) |>
  dplyr::select(site, school, last_name) |>
  dplyr::distinct(last_name, .keep_all = TRUE) |>
  dplyr::mutate(Match = TRUE)

### Google sheet reformat last name get school and site ###
sheet_match_ipg <- nyc_cps |>
  dplyr::mutate(
    last_name = sub(".* ", "", Teacher),
    School = ifelse(Site == "NY_D9", paste0(Site, "_0", stringr::str_remove(Site, "NY_D"), "X", School), School),
    School = ifelse(Site == "NY_D11", paste0(Site, "_", stringr::str_remove(Site, "NY_D"), "x", School), School),
    School = as.character(School)
  ) |>
  select(Site, School, last_name)

ipg_matched_final <- sheet_match_ipg |>
  dplyr::left_join(ipg_forms_match, by = c("Site" = "site", "School" = "school", "last_name")) |>
  dplyr::select(Match) |>
  dplyr::mutate(Match = replace_na(Match, FALSE))

ipg_matched_final_with_corrections <- ipg_matched_final |>
  dplyr::select(Match) |>
  dplyr::bind_cols(nyc_cps |> select(`Baseline IPG Observation`)) |>
  dplyr::mutate(
    Match = ifelse(`Baseline IPG Observation` == TRUE & Match == FALSE, `Baseline IPG Observation`, Match)
  ) |>
  dplyr::select(Match)

rows_to_sheet <- nrow(ipg_matched_final_with_corrections) + 2

ipg_matched_final_with_corrections |>
  googlesheets4::range_write(
    ss = "1WSQ2AvMZ4AwrC7SO78Yq2vF-UOcUnXQ8D82K1jbuOtE",
    sheet = "Master list",
    range = glue::glue("J3:J{rows_to_sheet}"),
    col_names = F,
    reformat = F
  )

### Finding last names that are matched ###
ipg_matched_names <- sheet_match_ipg |>
  dplyr::left_join(ipg_forms_match) |>
  dplyr::filter(Match == TRUE) |>
  dplyr::select(last_name)

### All last names in IPG forms ###
all_ipg_last_names <- ipg_forms |>
  dplyr::filter(Finished == TRUE & stringr::str_detect(site, "NY_D|Chi")) |>
  dplyr::mutate(
    last_name = stringr::str_to_title(sub(".* ", "", teacher)),
    dplyr::across(c(
      network4, network7, network12,
      district9, district11
    ), ~ as.character(.x)),
    school = dplyr::coalesce(
      network4, network7, network12,
      district9, district11
    )
  ) |>
  dplyr::select(site, school, last_name) |>
  dplyr::distinct(last_name, .keep_all = TRUE)

### Filters only those not matched ###
unmatched_last_names <- all_ipg_last_names |>
  dplyr::anti_join(ipg_matched_names, by = "last_name") |>
  dplyr::arrange(site)

### Clear Sheet by column so daily update doesn't include old data in lower rows ###
googlesheets4::range_clear(
  ss = "1WSQ2AvMZ4AwrC7SO78Yq2vF-UOcUnXQ8D82K1jbuOtE",
  sheet = "IPG Not Matched",
  range = "A:C",
  reformat = FALSE
)

### Write new set of unmatched last names ###
unmatched_last_names |>
  googlesheets4::range_write(
    ss = "1WSQ2AvMZ4AwrC7SO78Yq2vF-UOcUnXQ8D82K1jbuOtE",
    sheet = "IPG Not Matched",
    col_names = TRUE,
    reformat = FALSE,
    range = glue::glue("A1:C{nrow(unmatched_last_names) + 1}")
  )

################################ End of IPG Matching ################################################

################################ Educator Survey ID Non-Matching ################################################
### Finding IDs that are matched ###
matched_ids <- is_in_ed_survey_final_with_corrections |>
  dplyr::filter(!is.na(`Teacher code`)) |>
  dplyr::select(`Teacher code`)

### Filters educator survey for Chicago and NY that are not Rochester or Channelview and then selects just
### site school initials and finished ###
all_ed_survey_ids <- educator_survey |>
  dplyr::filter(stringr::str_detect(site, "Chicago|NY") &
    !stringr::str_detect(site, "Rochester|Channel")) |>
  dplyr::mutate(
    dplyr::across(c(
      network4, network7, network12,
      district9, district11, district27
    ), ~ as.character(.x)),
    school = dplyr::coalesce(
      network4, network7, network12,
      district9, district11, district27
    ),
    initials = toupper(initials)
  ) |>
  dplyr::select(site, school, content_area, initials, Finished)


### Joins all selected educator survey ids to already matched ids in the sheet to remove all
### already matched ids ###
unmatched_ids <- all_ed_survey_ids |>
  dplyr::anti_join(matched_ids, by = c("initials" = "Teacher code")) |>
  dplyr::arrange(Finished, site) |>
  dplyr::filter(!initials %in% c("SGS", "SHS")) ### FILTER THESE IDS SINCE THEY DO EXIST IN SHEET THEY JUST ARE UNIDENTIFIABLE DUE TO SAME FIRST LAST INITIALS AND DON'T ANTIJOIN ###

googlesheets4::range_clear(
  ss = "1WSQ2AvMZ4AwrC7SO78Yq2vF-UOcUnXQ8D82K1jbuOtE",
  sheet = "Not Matched (Updating)",
  range = "A:E",
  reformat = FALSE
)

unmatched_ids |>
  googlesheets4::range_write(
    ss = "1WSQ2AvMZ4AwrC7SO78Yq2vF-UOcUnXQ8D82K1jbuOtE",
    sheet = "Not Matched (Updating)",
    col_names = TRUE,
    reformat = FALSE,
    range = glue::glue("A1:E{nrow(unmatched_ids) + 1}")
  )

######## End of Non-Matching ##########


######## Start of Student Work Matching #############
### This works by merging on either teacher code to the sheet or on their name directly ###
student_work_survey_matched <- student_work_survey |>
  dplyr::filter(Finished == TRUE) |>
  dplyr::mutate(dplyr::across(c(district9, district11, district27), ~ as.character(.x)),
    School = dplyr::coalesce(district9, district11, district27),
    initials = toupper(initials)
  ) |>
  dplyr::select(initials, `teacher_name`, School) ## A lot of NAs for school because not all submissions are D9/D11

student_work_survey_matched_final <- student_work_survey_matched |>
  dplyr::left_join(nyc_cps |>
    dplyr::mutate(
      School = ifelse(Site == "NY_D9", paste0("NY_D9_09x", School), School),
      School = ifelse(Site == "NY_D11", paste0("NY_D11_11x", School), School)
    ) |>
    dplyr::select(Teacher, `Teacher code`, School), by = c(
    "initials" = "Teacher code",
    "teacher_name" = "Teacher",
    "School"
  )) |>
  dplyr::mutate(uploaded = "TRUE")

nyc_cps |>
  dplyr::select(`Teacher code`, Teacher) |>
  dplyr::mutate(
    uploaded = ifelse(`Teacher code` %in% student_work_survey_matched_final$initials, TRUE, FALSE),
    uploaded = ifelse(`Teacher` %in% student_work_survey_matched_final$`teacher_name`, TRUE, FALSE),
    uploaded = tidyr::replace_na(uploaded, FALSE)
  ) |>
  dplyr::select(uploaded) |>
  googlesheets4::range_write(
    ss = "1WSQ2AvMZ4AwrC7SO78Yq2vF-UOcUnXQ8D82K1jbuOtE",
    sheet = "Master list",
    range = glue::glue("K3:K{rows_to_sheet}"),
    col_names = F,
    reformat = F
  )


#### Write those not matching to extra tab in google sheet ####

googlesheets4::range_clear(
  ss = "1WSQ2AvMZ4AwrC7SO78Yq2vF-UOcUnXQ8D82K1jbuOtE",
  sheet = "Student Work Not Matched",
  range = "A:C",
  reformat = FALSE
)

student_work_survey_matched |>
  dplyr::left_join(nyc_cps |>
    dplyr::mutate(
      School = ifelse(Site == "NY_D9", paste0("NY_D9_09x", School), School),
      School = ifelse(Site == "NY_D11", paste0("NY_D11_11x", School), School)
    ) |>
    dplyr::select(Teacher, `Teacher code`, School), by = c(
    "initials" = "Teacher code",
    "teacher_name" = "Teacher",
    "School"
  )) |>
  dplyr::mutate(
    check = ifelse(`teacher_name` %in% nyc_cps$Teacher, TRUE, FALSE),
    check = ifelse(initials %in% nyc_cps$`Teacher code`, TRUE, check)
  ) |>
  dplyr::filter(check == FALSE) |>
  dplyr::select(-check) |>
  googlesheets4::range_write(
    ss = "1WSQ2AvMZ4AwrC7SO78Yq2vF-UOcUnXQ8D82K1jbuOtE",
    sheet = "Student Work Not Matched",
    col_names = T,
    reformat = F
  )

##################### End of Student Work Matching ###################################


####### Checking consent #########
# tl_control_consent <- tl_control |>
#   dplyr::select(Q32_Id, Q28) |>
#   tidyr::drop_na(Q32_Id)
#
# tl_treatment_nyc_consent <- tl_treatment_nyc |>
#   dplyr::select(Q32_Id, Q28) |>
#   tidyr::drop_na(Q32_Id)
#
# tl_treatment_nyc_cps_consent <- tl_treatment_nyc_cps |>
#   dplyr::select(Q32_Id, Q28) |>
#   tidyr::drop_na(Q32_Id)
#
# all_consent <- tl_control_consent |>
#   dplyr::bind_rows(tl_treatment_nyc_consent, tl_treatment_nyc_cps_consent) |>
#   dplyr::select(email = Q28) |>
#   tidyr::drop_na(email) |>
#   dplyr::mutate(signature = TRUE)

# nyc_cps |>
#   dplyr::select(`Teacher email`) |>
#   dplyr::left_join(all_consent, by = c("Teacher email" = "email")) |>
#   dplyr::mutate(signature = ifelse(is.na(signature), FALSE, TRUE)) |>
#   dplyr::select(signature) |>
#   googlesheets4::range_write(
#     ss = "1WSQ2AvMZ4AwrC7SO78Yq2vF-UOcUnXQ8D82K1jbuOtE",
#     sheet = "Master list",
#     range = glue::glue("L3:L{rows_to_sheet}"),
#     col_names = F,
#     reformat = F
#   )
###################### End Consent Matching ##########################################

rows_to_sheet <- nrow(nyc_cps) + 2

######################## IPG Mid Year Matching ##############################

mid_year_match <- ipg_forms |>
  dplyr::filter(stringr::str_detect(site, "Chic|NY") &
    (direct_to_ts_obs == "Mid-year (middle of service, if applicable)" |
      not_direct_to_ts %in% c(
        "Second site visit",
        "Third site visit",
        "Fourth site visit"
      ))) |>
  dplyr::select(teacher) |>
  dplyr::mutate(match = TRUE)

fuzzy_match_midyear <- fedmatch::merge_plus(
  data1 = nyc_cps |> dplyr::select(Teacher) |> dplyr::mutate(unique_key_1 = dplyr::row_number()),
  data2 = mid_year_match |> dplyr::mutate(unique_key_2 = dplyr::row_number()),
  by.x = "Teacher",
  by.y = "teacher",
  match_type = "fuzzy",
  unique_key_1 = "unique_key_1",
  unique_key_2 = "unique_key_2",
  fuzzy_settings = build_fuzzy_settings(maxDist = 0.108)
)

nyc_cps |>
  dplyr::select(Teacher) |>
  dplyr::left_join(fuzzy_match_midyear$matches |> dplyr::select(Teacher, match),
    by = "Teacher",
    multiple = "first"
  ) |>
  dplyr::select(match) |>
  dplyr::mutate(match = ifelse(is.na(match), FALSE, match)) |>
  googlesheets4::range_write(
    ss = "1WSQ2AvMZ4AwrC7SO78Yq2vF-UOcUnXQ8D82K1jbuOtE",
    sheet = "Master list",
    range = glue::glue("M3:M{rows_to_sheet}"),
    col_names = F,
    reformat = F
  )

######################## End of IPG Mid Year Matching ##############################

######################## IPG Ongoing Matching ##############################

ongoing_match <- ipg_forms |>
  dplyr::filter(stringr::str_detect(site, "Chic|NY") &
    (direct_to_ts_obs == "Ongoing")) |>
  dplyr::select(teacher) |>
  dplyr::mutate(match = TRUE)

fuzzy_match_ongoing <- fedmatch::merge_plus(
  data1 = nyc_cps |> dplyr::select(Teacher) |> dplyr::mutate(unique_key_1 = dplyr::row_number()),
  data2 = ongoing_match |> dplyr::mutate(unique_key_2 = dplyr::row_number()),
  by.x = "Teacher",
  by.y = "teacher",
  match_type = "fuzzy",
  unique_key_1 = "unique_key_1",
  unique_key_2 = "unique_key_2",
  fuzzy_settings = build_fuzzy_settings(maxDist = 0.13)
)

nyc_cps |>
  dplyr::select(Teacher) |>
  dplyr::left_join(fuzzy_match_ongoing$matches |> dplyr::select(Teacher, match),
    by = "Teacher",
    multiple = "first"
  ) |>
  dplyr::select(match) |>
  dplyr::mutate(match = ifelse(is.na(match), FALSE, match)) |>
  googlesheets4::range_write(
    ss = "1WSQ2AvMZ4AwrC7SO78Yq2vF-UOcUnXQ8D82K1jbuOtE",
    sheet = "Master list",
    range = glue::glue("N3:N{rows_to_sheet}"),
    col_names = F,
    reformat = F
  )

######################## IPG End of Year Matching ##############################

end_year_match <- ipg_forms |>
  dplyr::filter(stringr::str_detect(site, "Chic|NY") &
    (direct_to_ts_obs == "End of year (last observation of the year)")) |>
  dplyr::select(teacher) |>
  dplyr::mutate(match = TRUE)

fuzzy_match_endyear <- fedmatch::merge_plus(
  data1 = nyc_cps |> dplyr::select(Teacher) |> dplyr::mutate(unique_key_1 = dplyr::row_number()),
  data2 = end_year_match |> dplyr::mutate(unique_key_2 = dplyr::row_number()),
  by.x = "Teacher",
  by.y = "teacher",
  match_type = "fuzzy",
  unique_key_1 = "unique_key_1",
  unique_key_2 = "unique_key_2",
  fuzzy_settings = build_fuzzy_settings(maxDist = 0.13)
)

nyc_cps |>
  dplyr::select(Teacher) |>
  dplyr::left_join(fuzzy_match_endyear$matches |> dplyr::select(Teacher, match),
    by = "Teacher",
    multiple = "first"
  ) |>
  dplyr::select(match) |>
  dplyr::mutate(match = ifelse(is.na(match), FALSE, match)) |>
  googlesheets4::range_write(
    ss = "1WSQ2AvMZ4AwrC7SO78Yq2vF-UOcUnXQ8D82K1jbuOtE",
    sheet = "Master list",
    range = glue::glue("O3:O{rows_to_sheet}"),
    col_names = F,
    reformat = F
  )

######################## IPG IPRT 1 Matching ##############################

iprt_match <- ipg_forms |>
  dplyr::mutate(IPRT_1_2 = na_if(as.character(IPRT_1_2), "NA")) |>
  dplyr::filter(stringr::str_detect(site, "Chic|NY") &
    !is.na(IPRT_1_2)) |>
  dplyr::select(teacher, IPRT_1_2) |>
  dplyr::mutate(match = TRUE)

fuzzy_match_iprt1 <- fedmatch::merge_plus(
  data1 = nyc_cps |> dplyr::select(Teacher) |> dplyr::mutate(unique_key_1 = dplyr::row_number()),
  data2 = iprt_match |> dplyr::mutate(unique_key_2 = dplyr::row_number()),
  by.x = "Teacher",
  by.y = "teacher",
  match_type = "fuzzy",
  unique_key_1 = "unique_key_1",
  unique_key_2 = "unique_key_2",
  fuzzy_settings = build_fuzzy_settings(maxDist = 0.09)
)

if (!is.null(fuzzy_match_iprt1$matches)) {
  nyc_cps |>
    dplyr::select(Teacher) |>
    dplyr::left_join(fuzzy_match_iprt1$matches |> dplyr::select(Teacher, match),
      by = "Teacher",
      multiple = "first"
    ) |>
    dplyr::select(match) |>
    dplyr::mutate(match = ifelse(is.na(match), FALSE, match)) |>
    googlesheets4::range_write(
      ss = "1WSQ2AvMZ4AwrC7SO78Yq2vF-UOcUnXQ8D82K1jbuOtE",
      sheet = "Master list",
      range = glue::glue("W3:W{rows_to_sheet}"),
      col_names = F,
      reformat = F
    )
} else {
  tibble::tibble(
    match = rep("FALSE", rows_to_sheet - 2)
  ) |>
    googlesheets4::range_write(
      ss = "1WSQ2AvMZ4AwrC7SO78Yq2vF-UOcUnXQ8D82K1jbuOtE",
      sheet = "Master list",
      range = glue::glue("W3:W{rows_to_sheet}"),
      col_names = F,
      reformat = F
    )
}

######################## IPG IPRT 2 Matching ##############################

iprt_match_2 <- ipg_forms |>
  dplyr::mutate(IPRT_2_2 = na_if(as.character(IPRT_2_2), "NA")) |>
  dplyr::filter(stringr::str_detect(site, "Chic|NY") &
    !is.na(IPRT_2_2)) |>
  dplyr::select(teacher, IPRT_2_2) |>
  dplyr::mutate(match = TRUE)

fuzzy_match_iprt_2 <- fedmatch::merge_plus(
  data1 = nyc_cps |> dplyr::select(Teacher) |> dplyr::mutate(unique_key_1 = dplyr::row_number()),
  data2 = iprt_match_2 |> dplyr::mutate(unique_key_2 = dplyr::row_number()),
  by.x = "Teacher",
  by.y = "teacher",
  match_type = "fuzzy",
  unique_key_1 = "unique_key_1",
  unique_key_2 = "unique_key_2",
  fuzzy_settings = build_fuzzy_settings(maxDist = 0.09)
)

if (!is.null(fuzzy_match_iprt_2$matches)) {
  nyc_cps |>
    dplyr::select(Teacher) |>
    dplyr::left_join(fuzzy_match_iprt_2$matches |> dplyr::select(Teacher, match),
      by = "Teacher",
      multiple = "first"
    ) |>
    dplyr::select(match) |>
    dplyr::mutate(match = ifelse(is.na(match), FALSE, match)) |>
    googlesheets4::range_write(
      ss = "1WSQ2AvMZ4AwrC7SO78Yq2vF-UOcUnXQ8D82K1jbuOtE",
      sheet = "Master list",
      range = glue::glue("W3:W{rows_to_sheet}"),
      col_names = F,
      reformat = F
    )
} else {
  tibble::tibble(
    match = rep("FALSE", rows_to_sheet - 2)
  ) |>
    googlesheets4::range_write(
      ss = "1WSQ2AvMZ4AwrC7SO78Yq2vF-UOcUnXQ8D82K1jbuOtE",
      sheet = "Master list",
      range = glue::glue("X3:X{rows_to_sheet}"),
      col_names = F,
      reformat = F
    )
}

######################## IPG IPRT 2 Matching ##############################

iprt_match_3 <- ipg_forms |>
  dplyr::mutate(IPRT_3_2 = na_if(as.character(IPRT_3_2), "NA")) |>
  dplyr::filter(stringr::str_detect(site, "Chic|NY") &
    !is.na(IPRT_3_2)) |>
  dplyr::select(teacher, IPRT_3_2) |>
  dplyr::mutate(match = TRUE)

fuzzy_match_iprt_3 <- fedmatch::merge_plus(
  data1 = nyc_cps |> dplyr::select(Teacher) |> dplyr::mutate(unique_key_1 = dplyr::row_number()),
  data2 = iprt_match_3 |> dplyr::mutate(unique_key_2 = dplyr::row_number()),
  by.x = "Teacher",
  by.y = "teacher",
  match_type = "fuzzy",
  unique_key_1 = "unique_key_1",
  unique_key_2 = "unique_key_2",
  fuzzy_settings = build_fuzzy_settings(maxDist = 0.09)
)

if (!is.null(fuzzy_match_iprt_3$matches)) {
  nyc_cps |>
    dplyr::select(Teacher) |>
    dplyr::left_join(fuzzy_match_iprt_3$matches |> dplyr::select(Teacher, match),
      by = "Teacher",
      multiple = "first"
    ) |>
    dplyr::select(match) |>
    dplyr::mutate(match = ifelse(is.na(match), FALSE, match)) |>
    googlesheets4::range_write(
      ss = "1WSQ2AvMZ4AwrC7SO78Yq2vF-UOcUnXQ8D82K1jbuOtE",
      sheet = "Master list",
      range = glue::glue("W3:W{rows_to_sheet}"),
      col_names = F,
      reformat = F
    )
} else {
  tibble::tibble(
    match = rep("FALSE", rows_to_sheet - 2)
  ) |>
    googlesheets4::range_write(
      ss = "1WSQ2AvMZ4AwrC7SO78Yq2vF-UOcUnXQ8D82K1jbuOtE",
      sheet = "Master list",
      range = glue::glue("Y3:Y{rows_to_sheet}"),
      col_names = F,
      reformat = F
    )
}

######## Function for checking initials match #############
######## Works by taking an input tibble of just the initials to match ##############
check_initials_match <- function(codes_match, column) {
  colnames(codes_match)[1] <- "initials"
  colnames(codes_match)[2] <- "site"

  rows_to_sheet <- nrow(nyc_cps) + 2

  col_select <- LETTERS[which(colnames(nyc_cps) == column)]

  nyc_cps |>
    dplyr::select(initials = `Teacher code`, site = Site) |>
    dplyr::left_join(codes_match,
      multiple = "first"
    ) |>
    dplyr::mutate(
      match = ifelse(is.na(match), FALSE, match),
      initials = toupper(initials)
    ) |>
    dplyr::select(match) |>
    googlesheets4::range_write(
      ss = "1WSQ2AvMZ4AwrC7SO78Yq2vF-UOcUnXQ8D82K1jbuOtE",
      sheet = "Master list",
      range = glue::glue("{col_select}3:{col_select}{rows_to_sheet}"),
      col_names = F,
      reformat = F
    )
}

############################## Ongoing coaching matching ##############################

participant_feedback |>
  dplyr::filter(stringr::str_detect(site, "Chic|NY") &
    course == "Coaching" &
    last_session_or_not == "Yes - there will be more sessions for this PL course or coaching cycle.") |>
  dplyr::select(initials, site) |>
  dplyr::mutate(
    initials = toupper(initials),
    match = TRUE
  ) |>
  check_initials_match("Participant feedback - ongoing coaching")

############################## End of Year coaching matching ##############################

participant_feedback |>
  dplyr::filter(stringr::str_detect(site, "Chic|NY") &
    course == "Coaching" &
    last_session_or_not == "No - this was the final session for this PL course (e.g., last day of Bootcamp, close of Inquiry cycle) or coaching.") |>
  dplyr::select(initials, site) |>
  dplyr::mutate(
    initials = toupper(initials),
    match = TRUE
  ) |>
  check_initials_match("Participant feedback - End of year coaching")


############################## Follow up educator survey matching ##############################

educator_survey |>
  dplyr::filter(stringr::str_detect(site, "Chic|NY") &
    RecordedDate >= as.Date("2023-03-01")) |>
  dplyr::select(initials, site) |>
  dplyr::mutate(
    initials = toupper(initials),
    match = TRUE
  ) |>
  check_initials_match("Follow up Educator Survey")


######################## Followup Educator Survey ##############################

########################### End Script #############################################
