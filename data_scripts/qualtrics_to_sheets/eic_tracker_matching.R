library(dplyr)
library(googlesheets4)
library(purrr)
library(qualtRics)
library(stringr)
library(tidyr)

rem_dup_word <- function(x) {
  x <- paste0(unique(str_split(x, "/")[[1]]), collapse = "/")
  
  x
}

### Get original tracker sheet ###
eic_tracker <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1t9iZGwjzLBuijQcJ38kdOwTHHSdwdMMNdagKyJml3FU/edit#gid=2032615435",
  sheet = 1
)
### Qualtrics Surveys ###
### Teaching Lab ###
educator_survey <- qualtRics::fetch_survey(
  surveyID = "SV_8vrKtPDtqQFbiBM",
  verbose = FALSE,
  force_request = TRUE
)

eic_student_survey <- qualtRics::fetch_survey(
  surveyID = "SV_8f9l21n6ML58WFM",
  verbose = FALSE,
  force_request = TRUE
)

eic_emails_student_work <- eic_student_survey |>
  dplyr::filter(Finished == TRUE) |>
  dplyr::group_by(RecipientEmail) |>
  dplyr::count(sort = T) |>
  dplyr::mutate(`Student Survey` = TRUE) |>
  dplyr::relocate(n, .after = `Student Survey`)

student_work_survey <- qualtRics::fetch_survey(
  surveyID = "SV_6nwa9Yb4OyXLji6",
  verbose = FALSE,
  force_request = TRUE
)

################################################ End of Surveys GET ##################################################

#### Student Work Survey Matching ####

student_work_initials <- student_work_survey |>
  dplyr::filter(str_detect(site, "NY_D11|NY_Roch")) |>
  dplyr::mutate(initials = toupper(initials)) |>
  tidyr::drop_na(initials) |>
  dplyr::group_by(initials) |>
  dplyr::count(sort = T) |>
  dplyr::mutate(`Student work` = TRUE) |>
  dplyr::relocate(`Student work`, .before = n)

student_work_names <- student_work_survey |>
  dplyr::filter(str_detect(site, "NY_D11|NY_Roch")) |>
  dplyr::group_by(teacher_name) |>
  dplyr::count() |>
  tidyr::drop_na() |>
  dplyr::mutate(`Student work 2` = TRUE)

### EIC Tracker transform Initials, School, District and select ###
eic_tracker_initials <- eic_tracker |>
  dplyr::mutate(
    Initials = paste0(substr(FirstName, 1, 1), substr(LastName, 1, 1)),
    School = as.character(School),
    School = ifelse(School == "NULL", NA, School),
    District = ifelse(District == "NY_Rochester City School District", "NY_Rochester City Schools", District),
    School = ifelse(District == "NY_D11", paste0(District, "_", stringr::str_remove(District, "NY_D"), "x", School), School)
  ) |>
  dplyr::select(Site = District, School, Initials)


### Get just NY D11 and Rochester data from educator survey and reformat the initials ###
teacher_code_ny <- educator_survey |>
  dplyr::filter(Finished == TRUE & stringr::str_detect(site, "NY_D11|NY_Roch")) |>
  dplyr::mutate(initials2 = paste0(
    substr(toupper(initials), 1, 1),
    substr(toupper(initials), 3, 3)
  )) |>
  dplyr::mutate(dplyr::across(c(
    network4, network7, network12,
    district9, district11, district27,
    rochester
  ), ~ as.character(.x)),
  School = dplyr::coalesce(
    network4, network7, network12,
    district9, district11, district27,
    rochester
  )
  ) |>
  dplyr::select(site, initials2, initials, School)


### Join in initials
ny_is_in_ed_survey <- eic_tracker_initials |>
  dplyr::left_join(teacher_code_ny,
    by = c("Initials" = "initials2", "Site" = "site", "School"),
    multiple = "all"
  ) |>
  dplyr::rename(Initials = Initials,
                initials3 = initials) |>
  dplyr::group_by(initials3, Site, School) |>
  dplyr::mutate(initials_count = n()) |>
  dplyr::ungroup() |>
  dplyr::mutate(name = ifelse(initials_count >= 1, "initials_dup", NA)) |>
  tidyr::pivot_wider(values_from = Initials, names_from = name, values_fn = list) |>
  dplyr::mutate(
    initials_dup = toupper(stringr::str_remove_all(stringr::str_replace_all(stringr::str_remove_all(as.character(initials_dup), "c\\(|\\)"), '\\", \\"', "/"), '\\"')),
    initials_dup = purrr::map(initials_dup, rem_dup_word),
    initials_dup = ifelse(stringr::str_detect(initials_dup, "NA"), NA, as.character(initials_dup))
  ) |>
  tidyr::drop_na(initials_dup) |>
  dplyr::filter(initials_dup != "NA, NA" & initials_dup != "NA, NA, NA") |>
  dplyr::rename(
    `Teacher code` = initials_dup,
    Initials = initials3
  ) |>
  dplyr::select(-initials_count)

is_in_ed_survey_final <- eic_tracker |>
  dplyr::mutate(
    Initials = paste0(substr(FirstName, 1, 1), substr(LastName, 1, 1)),
    School = as.character(School),
    School = ifelse(School == "NULL", NA, School),
    District = ifelse(District == "NY_Rochester City School District", "NY_Rochester City Schools", District),
    School = ifelse(District == "NY_D11", paste0(District, "_", stringr::str_remove(District, "NY_D"), "x", School), School),
    Name = paste(FirstName, LastName)
  ) |>
  dplyr::select(Site = District, School, Initials, Email, Name) |>
  dplyr::left_join(ny_is_in_ed_survey) |>
  dplyr::mutate(correct_initials = ifelse(is.na(`Teacher code`), FALSE, TRUE)) |>
  dplyr::left_join(eic_emails_student_work, by = c("Email" = "RecipientEmail")) |>
  dplyr::left_join(student_work_initials, by = c("Teacher code" = "initials")) |>
  dplyr::left_join(student_work_names, by = c("Name" = "teacher_name")) |>
  dplyr::mutate(`Student work` = dplyr::coalesce(`Student work`, `Student work 2`),
         n.y = dplyr::coalesce(n.y, n),
         dplyr::across(c("Student work", "Student Survey"), ~ replace_na(.x, FALSE))) |>
  dplyr::select(-`Student work 2`, -n)

rows_to_sheet <- nrow(is_in_ed_survey_final) + 1

is_in_ed_survey_final |>
  dplyr::select(`Teacher code`, correct_initials, `Student Survey`, n.x, `Student work`, n.y) |>
  googlesheets4::range_write(
    ss = "1t9iZGwjzLBuijQcJ38kdOwTHHSdwdMMNdagKyJml3FU",
    sheet = 1,
    range = glue::glue("F2:K{rows_to_sheet}"),
    col_names = F,
    reformat = F
  )
  




