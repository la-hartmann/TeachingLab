library(dplyr)
library(googlesheets4)
library(purrr)
library(qualtRics)
library(rcanvas)
library(stringr)
library(tidyr)

rcanvas::set_canvas_domain("https://nmped.instructure.com/")

### Data Tracker Google Sheets ###
new_mexico_tracker <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=804355574",
  sheet = "Data Tracker SY22-23"
)

### Diagnostic in Qualtrics ###
new_mexico_diagnostic <- qualtRics::fetch_survey(
  surveyID = "SV_3a2OM4f9j85EuyO",
  verbose = TRUE,
  force_request = TRUE
)

### Student Survey in Qualtrics ###
student_survey <- qualtRics::fetch_survey(
  surveyID = "SV_9uze2faHuIf3vP8",
  verbose = TRUE
)

### IPG Forms in Qualtrics ###
ipg_forms_nm <- qualtRics::fetch_survey(
  surveyID = "SV_0BSnkV9TVXK1hjw",
  verbose = FALSE
) |>
  dplyr::filter(site == "NM_NM Public Education Department")

### Get canvas gradebook for the course ###
course_gradebook <- rcanvas::get_course_gradebook(course_id = 2925)

### Trying to figure out what is wrong here ###
# course_gradebook |>
#   filter(!is.null(attachments) &
#            assignment_name == "Cohort A: Self-Recorded Video Submission" &
#            attachments != "NULL") |>
#   view()


############################### Script Matching Starts Here ##################################
nm_emails <- new_mexico_diagnostic |>
  dplyr::select(email) |>
  dplyr::mutate(
    email = tolower(email),
    `Educator Survey Completed (Y)` = T
  )

rows <- nrow(new_mexico_tracker) + 1

correct_column <- LETTERS[which(colnames(new_mexico_tracker) == "Educator Survey Completed (Y)")]

### Write diagnostic email match to Google Sheet ###
new_mexico_tracker |>
  dplyr::mutate(email = tolower(Email)) |>
  dplyr::select(-`Educator Survey Completed (Y)`) |>
  dplyr::left_join(nm_emails |> dplyr::distinct(email, .keep_all = TRUE)) |>
  dplyr::select(`Educator Survey Completed (Y)`) |>
  googlesheets4::range_write(
    ss = "https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=804355574",
    sheet = "Data Tracker SY22-23",
    range = glue::glue("{correct_column}2:{correct_column}{rows}"),
    reformat = F,
    col_names = F
  )

nm_emails_2 <- new_mexico_diagnostic |>
  dplyr::filter(RecordedDate >= as.Date("2022-12-08")) |>
  dplyr::select(email) |>
  dplyr::mutate(
    email = tolower(email),
    `Second Educator Survey completion (12/9-1/18)` = T
  )

correct_column_6 <- LETTERS[which(colnames(new_mexico_tracker) == "Second Educator Survey completion (12/9-1/18)")]

### Write diagnostic email match to Google Sheet ###
new_mexico_tracker |>
  dplyr::mutate(email = tolower(Email)) |>
  dplyr::select(-`Second Educator Survey completion (12/9-1/18)`) |>
  dplyr::left_join(nm_emails_2 |> dplyr::distinct(email, .keep_all = TRUE)) |>
  dplyr::select(`Second Educator Survey completion (12/9-1/18)`) |>
  dplyr::mutate(`Second Educator Survey completion (12/9-1/18)` = ifelse(is.na(`Second Educator Survey completion (12/9-1/18)`), FALSE, `Second Educator Survey completion (12/9-1/18)`)) |>
  googlesheets4::range_write(
    ss = "https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=804355574",
    sheet = "Data Tracker SY22-23",
    range = glue::glue("{correct_column_6}2:{correct_column_6}{rows}"),
    reformat = F,
    col_names = F
  )

#### Canvas Name Matching for Assignments ####
### Name Replacement Vector to match to Google Sheets ###
name_replace_vector <- c(
  "Are-Pee M. Castalone" = "Are-Pee Castalone",
  "Jose Cruz Brito Villela" = "Jose Brito",
  "Julio Enrique Meza Quezada" = "Julio Meza",
  "Marta Martin Alonso" = "Marta Martin-Alonso",
  "Martin Neddo Roaque" = "Martin Roaque",
  "Sylvy Galvan De Lucero" = "Sylvy Galvan de Lucero",
  "Vicki Gallegos" = "Virginia Gallegos",
  "Amelia James" = "Amy James"
)

canvas_sheet_match <- function(canvas_assignment_name, sheet_assignment_name) {
  ### Get only those who completed by user.name, completed ###
  completed_round_1 <- course_gradebook |>
    dplyr::filter(assignment_name == canvas_assignment_name & workflow_state == "submitted") |>
    dplyr::mutate(
      user.name = stringr::str_to_title(user.name),
      user.name = stringr::str_replace_all(user.name, name_replace_vector)
    ) |>
    dplyr::distinct(user.name, .keep_all = TRUE) |>
    dplyr::select(workflow_state, user.name) |>
    dplyr::mutate(completed = ifelse(workflow_state != "unsubmitted", TRUE, FALSE)) |>
    dplyr::select(user.name, completed)

  ### Get correct column letter ###
  correct_column_2 <- LETTERS[which(colnames(new_mexico_tracker) == sheet_assignment_name)]

  print("Adding to sheet...")

  ### Write to sheet ###
  new_mexico_tracker |>
    dplyr::select(`Participant Name`) |>
    dplyr::left_join(completed_round_1, by = c("Participant Name" = "user.name")) |>
    dplyr::select(completed) |>
    dplyr::mutate(completed = ifelse(is.na(completed), FALSE, completed)) |>
    googlesheets4::range_write(
      ss = "17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU",
      sheet = "Data Tracker SY22-23",
      col_names = FALSE,
      reformat = FALSE,
      range = glue::glue("{correct_column_2}2:{correct_column_2}{rows}")
    )

  print("Successfully updated!")
}

### Cohort A: Self-Recorded Video Submission ###
canvas_sheet_match(
  canvas_assignment_name = "Cohort A: Self-Recorded Video Submission",
  sheet_assignment_name = "Cohort A: Self-Recorded Video Submission"
)

### Round 1 Student Work Submission ###
canvas_sheet_match(
  canvas_assignment_name = "ROUND 1: Student Work Submission",
  sheet_assignment_name = "ROUND 1: Student Work Submission"
)

### Round 1 Student Survey ###
correct_column_3_1 <- LETTERS[which(colnames(new_mexico_tracker) == "Round 1 Student Survey (12/9-1/18)")]
correct_column_3_2 <- LETTERS[which(colnames(new_mexico_tracker) == "Round 1 Student Survey N")]

student_surveys_1 <- student_survey |>
  dplyr::filter(RecordedDate <= as.Date("2023-02-01")) |>
  dplyr::mutate(dplyr::across(dplyr::contains("teacher_names"), ~ as.character(.x)),
                dplyr::across(dplyr::contains("teacher_names"), ~ dplyr::na_if(.x, "NA")),
    Teacher = dplyr::coalesce(
      `teacher_names_1`,
      `teacher_names_2`,
      `teacher_names_3`,
      `teacher_names_4`,
      `teacher_names_5`,
      `teacher_names_6`,
      `teacher_names_7`,
      `teacher_names_8`,
      `teacher_names_9`,
      `teacher_names_10`,
      `teacher_names_11`
    )
  ) |>
  # filter(is.na(Teacher)) |>
  # select(1:40, Teacher) |>
  # select(contains("Teacher")) |>
  # view()
  dplyr::group_by(Teacher) |>
  dplyr::count(sort = T) |>
  dplyr::mutate(completed = TRUE)

new_mexico_tracker |>
  dplyr::select(`Participant Name`) |>
  dplyr::left_join(student_surveys_1, by = c("Participant Name" = "Teacher")) |>
  dplyr::select(completed, n) |>
  dplyr::mutate(
    completed = ifelse(is.na(completed), FALSE, completed),
    n = ifelse(is.na(n), 0, n)
  ) |>
  googlesheets4::range_write(
    ss = "17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU",
    sheet = "Data Tracker SY22-23",
    col_names = FALSE,
    reformat = FALSE,
    range = glue::glue("{correct_column_3_1}2:{correct_column_3_2}{rows}")
  )

### IPRT Observation Automation ###
submitted_iprt <- ipg_forms |>
  dplyr::mutate(dplyr::across(dplyr::contains("IPRT"), ~ as.character(.x)),
    iprt_done = dplyr::coalesce(
      IPRT_1_36_Name, IPRT_2_20_Name, IPRT_3_20_Name,
      IPRT_4_20_Name, IPRT_5_20_Name, IPRT_6_19_Name
    )
  ) |>
  dplyr::filter(!is.na(iprt_done)) |>
  dplyr::select(teacher) |>
  tidyr::drop_na() |>
  dplyr::distinct() |>
  dplyr::mutate(submitted = TRUE)

correct_column_7 <- LETTERS[which(colnames(new_mexico_tracker) == "IPRT Observation (1/9-1/18)")]

new_mexico_tracker |>
  dplyr::select(`Participant Name`) |>
  dplyr::left_join(submitted_iprt, by = c("Participant Name" = "teacher")) |>
  dplyr::mutate(submitted = ifelse(is.na(submitted), FALSE, TRUE)) |>
  dplyr::select(submitted) |>
  googlesheets4::range_write(
    ss = "https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=804355574",
    sheet = "Data Tracker SY22-23",
    range = glue::glue("{correct_column_7}2:{correct_column_7}{rows}"),
    reformat = F,
    col_names = F
  )

### ROUND 2: Student work Submission ###
canvas_sheet_match(
  canvas_assignment_name = "ROUND 2: Student Work Submission",
  sheet_assignment_name = "ROUND 2: Cohort B Student Work Submission (3/2 -4/5)"
)

### Cohort B: Self-Recorded Video Submission ###
canvas_sheet_match(
  canvas_assignment_name = "Cohort B: Self-Recorded Video Submission",
  sheet_assignment_name = "Cohort B: Self-Recorded Video Submission (3/2 -4/5)"
)

### Third Educator Survey Completion ###
nm_emails_3 <- new_mexico_diagnostic |>
  dplyr::filter(RecordedDate >= as.Date("2023-04-01")) |>
  dplyr::select(email) |>
  dplyr::mutate(
    email = tolower(email),
    `Educator Survey Completed (Y)` = T
  )

correct_column_4 <- LETTERS[which(colnames(new_mexico_tracker) == "Third Educator Survey completion (4/20-5/18)")]

### Write diagnostic email match to Google Sheet ###
new_mexico_tracker |>
  dplyr::mutate(email = tolower(Email)) |>
  dplyr::select(-`Educator Survey Completed (Y)`) |>
  dplyr::left_join(nm_emails_3 |> dplyr::distinct(email, .keep_all = TRUE)) |>
  dplyr::select(`Educator Survey Completed (Y)`) |>
  dplyr::mutate(`Educator Survey Completed (Y)` = ifelse(is.na(`Educator Survey Completed (Y)`), FALSE, `Educator Survey Completed (Y)`)) |>
  googlesheets4::range_write(
    ss = "https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=804355574",
    sheet = "Data Tracker SY22-23",
    range = glue::glue("{correct_column_4}2:{correct_column_4}{rows}"),
    reformat = F,
    col_names = F
  )

### Round 2 Student Survey ###
correct_column_5_1 <- LETTERS[which(colnames(new_mexico_tracker) == "Round 2 Student Survey (4/20-5/18)")]
correct_column_5_2 <- LETTERS[which(colnames(new_mexico_tracker) == "Round 2 Student Survey N")]

student_surveys_2 <- student_survey |>
  dplyr::filter(RecordedDate >= as.Date("2023-04-19")) |>
  dplyr::mutate(dplyr::across(dplyr::contains("teacher_names"), ~ as.character(.x)),
                dplyr::across(dplyr::contains("teacher_names"), ~ dplyr::na_if(.x, "NA")),
                Teacher = dplyr::coalesce(
                  `teacher_names_1`,
                  `teacher_names_2`,
                  `teacher_names_3`,
                  `teacher_names_4`,
                  `teacher_names_5`,
                  `teacher_names_6`,
                  `teacher_names_7`,
                  `teacher_names_8`,
                  `teacher_names_9`,
                  `teacher_names_10`,
                  `teacher_names_11`
                )
  ) |>
  dplyr::group_by(Teacher) |>
  dplyr::count(sort = T) |>
  dplyr::mutate(completed = TRUE)

new_mexico_tracker |>
  dplyr::select(`Participant Name`) |>
  dplyr::left_join(student_surveys_2, by = c("Participant Name" = "Teacher")) |>
  dplyr::select(completed, n) |>
  dplyr::mutate(
    completed = ifelse(is.na(completed), FALSE, completed),
    n = ifelse(is.na(n), 0, n)
  ) |>
  googlesheets4::range_write(
    ss = "17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU",
    sheet = "Data Tracker SY22-23",
    col_names = FALSE,
    reformat = FALSE,
    range = glue::glue("{correct_column_5_1}2:{correct_column_5_2}{rows}")
  )

### Round 3: Student Work Submission ###
canvas_sheet_match(
  canvas_assignment_name = "ROUND 3: Student Work Submission",
  sheet_assignment_name = "ROUND 3: Student Work Submission"
)


### Get all Canvas Student Work and add to Dashboard ###
# course_gradebook |>
#   filter((assignment_name == "ROUND 1: Student Work Submission" |
#            assignment_name == "ROUND 2: Student Work Submission" |
#            assignment_name == "ROUND 3: Student Work Submission") & workflow_state == "submitted") |>
#   view()

############################### Script ends here ###############################################
