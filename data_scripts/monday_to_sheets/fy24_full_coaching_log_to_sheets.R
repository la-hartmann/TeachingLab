library(googlesheets4)
library(reticulate)
library(tidyverse)
library(TeachingLab)

### Run python script to get monday json ###
reticulate::source_python(here::here("data_scripts/monday.com/get_full_coaching_log.py"))

### Read in Monday JSON ###
full_log <- jsonlite::fromJSON(here::here("data/monday/fy24_teachers_coaching_log_full.json"))

### Get as a data.frame ###
full_log_df <- full_log$data$boards$items_page$items[[1]]$column_values |>
  as.data.frame()

educator_survey_emails <- TeachingLab::get_diagnostic_survey(year = "23_24") |>
  dplyr::mutate(email2 = dplyr::coalesce(email, RecipientEmail)) |>
  tidyr::drop_na(email2) |>
  dplyr::pull(email2) |>
  tolower()

ipg_names <- TeachingLab::get_ipg_forms(year = "23_24") |>
  dplyr::filter(ipg_rubric %in% c("K-12: Mathematics IPG", 
                                  "K-12: ELA/Literacy IPG (please use this tool for K-2 observations that are not focused on foundational skills)",
                                  "Foundational Skills Observational Tool - FSOT") &
                  direct_to_ts_obs == "Baseline (first observation of the year)") |>
  dplyr::mutate(teacher_name = teacher_select,
                teacher_name = dplyr::na_if(teacher_name, "Other"),
                teacher_name = dplyr::coalesce(teacher_name, teacher)) |>
  tidyr::drop_na(teacher_name) |>
  dplyr::pull(teacher_name)
  
ipg_d11_names <- TeachingLab::get_ipg_forms(year = "23_24") |>
  dplyr::filter(ipg_rubric == "District 11: Mathematics Instructional Walkthrough Tool" &
                  direct_to_ts_obs == "Baseline (first observation of the year)") |>
  dplyr::mutate(teacher_name = teacher_select,
                teacher_name = dplyr::na_if(teacher_name, "Other"),
                teacher_name = dplyr::coalesce(teacher_name, teacher)) |>
  tidyr::drop_na(teacher_name) |>
  dplyr::pull(teacher_name)

student_work_names <- TeachingLab::get_student_work(year = "23_24") |>
  dplyr::filter(Finished == TRUE) |>
  dplyr::mutate(teacher = teacher_select,
                teacher = dplyr::na_if(teacher, "Other"),
                teacher = dplyr::coalesce(teacher, teacher_name)) |>
  # dplyr::select(teacher, teacher_select, teacher_name) |>
  tidyr::drop_na(teacher) |>
  dplyr::pull(teacher)

student_survey_emails <- TeachingLab::get_student_survey(year = "23_24") |>
  dplyr::select(email = RecipientEmail) |>
  tidyr::drop_na(email) |>
  dplyr::mutate(email = tolower(email)) |>
  dplyr::pull(email)

ongoing_coaching_emails <- TeachingLab::get_ongoing_coaching(year = "23_24") |>
  tidyr::drop_na(email) |>
  dplyr::pull(email) |>
  tolower()

end_coaching_emails <- TeachingLab::get_end_coaching(year = "23_24") |>
  tidyr::drop_na(email) |>
  dplyr::pull(email) |>
  tolower()

chicago_signature <- TeachingLab::get_diagnostic_survey(year = "23_24") |>
  dplyr::filter(!is.na(CPS_signature_Id)) |>
  tidyr::drop_na(RecipientEmail) |>
  dplyr::pull(RecipientEmail) |>
  tolower()

nyc_signature <- TeachingLab::get_diagnostic_survey(year = "23_24") |>
  dplyr::filter(!is.na(NY_signature_Id)) |>
  tidyr::drop_na(RecipientEmail) |>
  dplyr::pull(RecipientEmail) |>
  tolower()

final_log_df <- full_log_df |>
  dplyr::mutate(dplyr::across(dplyr::contains("column"), ~ .x$title)) |>
  dplyr::select(column, dplyr::contains("text")) |>
  tidyr::pivot_longer(!column) |>
  tidyr::pivot_wider(names_from = column, values_from = value, values_fn = list) |>
  dplyr::mutate(dplyr::across(dplyr::everything(), ~ as.character(.x)),
    dplyr::across(dplyr::everything(), ~ dplyr::na_if(.x, "NANA")),
    dplyr::across(dplyr::everything(), ~ dplyr::na_if(.x, "c(NA, NA)")),
    dplyr::across(dplyr::everything(), ~ dplyr::na_if(.x, "")),
    `What is the expected frequency of coaching?` = stringr::str_remove_all(`What is the expected frequency of coaching?`, "\", NA\\)|c\\(\""),
    `Content Area` = ifelse(`Content Area` == "Other", `Other content`, `Content Area`),
    Subsite = dplyr::coalesce(`Blytheville School`, `FAA school`, `Hope School`, `Osceola Schools`, `CPS N4 school`, `CPS N7 school`, `CPS N12 school`, `Pt Coupee school`, `Ascend school`, `D6 Schools`, `D9 school`, `D11 school`, `D12 school`, `D16 school`, `D75 school`, `Rochester school`, `If you selected "other" or if your site did not have a school selection, please write in the school name.`),
    Email = tolower(Email)
  ) |>
  dplyr::select(-c(
    name,
    `Blytheville School`,
    `FAA school`,
    `Hope School`,
    `Osceola Schools`,
    `CPS N4 school`,
    `CPS N7 school`,
    `CPS N12 school`,
    `Pt Coupee school`,
    `Ascend school`, `D6 Schools`,
    `D9 school`,
    `D11 school`,
    `D12 school`,
    `D16 school`,
    `D75 school`,
    `Rochester school`,
    `If you selected "other" or if your site did not have a school selection, please write in the school name.`,
    `Is this coachee replacing someone else?`,
    `Write in name of coachee being replaced on your list.`,
    `Frequency other`,
    `Other content`
  )) |>
  dplyr::relocate(Subsite, .after = `Site/Partner`) |>
  dplyr::mutate(
    `Diagnostic Educator Survey` = ifelse(Email %in% educator_survey_emails, TRUE, FALSE),
    `Baseline IPG observation` = ifelse(Coachee %in% ipg_names, TRUE, FALSE),
    `Baseline Student Work` = ifelse(Coachee %in% student_work_names, TRUE, FALSE),
    `Baseline Student Survey` = ifelse(Email %in% student_survey_emails, TRUE, FALSE),
    `Ongoing coaching participant feedback survey` = ifelse(Email %in% ongoing_coaching_emails, TRUE, FALSE),
    `End of coaching cycle participant feedback survey` = ifelse(Email %in% end_coaching_emails, TRUE, FALSE),
    `Chicago Signature` = ifelse(tolower(Email) %in% chicago_signature, TRUE, FALSE),
    `NY Signature` = ifelse(tolower(Email) %in% nyc_signature, TRUE, FALSE)
  )

get_response_rate <- function(data) {
  data |>
    summarise(`Please select today's date.` = paste0("Last updated: ", Sys.Date()),
            `Coach name` = "",
            Coachee = "",
            Email = "",
            Role = "",
            `When is coaching expected to start with this coachee (approximately if no date has been set yet)?` = "",
            `What is the expected frequency of coaching?` = "",
            `What grade(s) does your coaching focus on with this teacher or coach?` = "",
            `Content Area` = "",
            `Site/Partner` = "",
            Subsite = "",
            across(c(`Diagnostic Educator Survey`,
                     `Baseline IPG observation`,
                     `Baseline Student Work`,
                     `Baseline Student Survey`,
                     `Ongoing coaching participant feedback survey`,
                     `End of coaching cycle participant feedback survey`,
                     `Chicago Signature`,
                     `NY Signature`), ~ paste0(round(100 * sum(.x)/length(.x), 2), "% Response Rate")))
}

summary_row <- final_log_df |>
  get_response_rate()

final_log_df_with_summary <- final_log_df |>
  mutate(across(everything(), as.character)) |>
  add_row(summary_row)

sheet_range <- paste0("A2:", LETTERS[ncol(final_log_df_with_summary)], nrow(final_log_df_with_summary) + 1)

range_write(
  data = final_log_df_with_summary,
  ss = "13d5l1k_oTiEKmnKIAFzoBMFWsr1f__JWkFpLmDesacw",
  sheet = "Tracker",
  col_names = FALSE,
  reformat = FALSE,
  range = sheet_range
)

split_final_log <- split(final_log_df, final_log_df$`Site/Partner`) |>
  purrr::imap( ~ .x |> mutate(across(everything(), as.character)) |> add_row(get_response_rate(.x))) |>
  purrr::imap( ~ if(!str_detect(.y, "D9|D11")) { .x |> dplyr::select(-`NY Signature`) } else { .x } ) |>
  purrr::imap( ~ if(!str_detect(.y, "CPS")) { .x |> dplyr::select(-`Chicago Signature`) } else { .x } )

split_final_log$NY_D11 <- split_final_log$NY_D11 |>
  dplyr::mutate(`D11 Instructional Walkthrough Tool` = case_when(Coachee %in% ipg_d11_names ~ "TRUE",
                                                                 Coachee == "" ~ "",
                                                                 TRUE ~ "FALSE")) |>
  dplyr::relocate(`D11 Instructional Walkthrough Tool`, .after = `Baseline IPG observation`) |>
  dplyr::mutate(`D11 Instructional Walkthrough Tool` = ifelse(`D11 Instructional Walkthrough Tool` == "",
                                                              paste0(round(100 * sum(`D11 Instructional Walkthrough Tool` == "TRUE")/length(`D11 Instructional Walkthrough Tool`) - 1, 2), "% Response Rate"),
                                                              `D11 Instructional Walkthrough Tool`))

get_sheet_range <- function(df) {
  paste0("A2:", LETTERS[ncol(df)], nrow(df) + 1)
}

sheet_ranges <- purrr::map_chr(split_final_log, get_sheet_range)

sheet_names <- names(sheet_ranges)

purrr::pmap(
  list(split_final_log,
  sheet_names,
  sheet_ranges),
  ~ range_write(
    data = ..1,
    ss = "13d5l1k_oTiEKmnKIAFzoBMFWsr1f__JWkFpLmDesacw",
    sheet = ..2,
    col_names = FALSE,
    reformat = FALSE,
    range = ..3
  )
)
