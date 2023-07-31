library(dplyr)
library(googlesheets4)
library(qualtRics)
# library(RCurl)
library(stringr)

### Basically just a wrapper ###
student_work <- TeachingLab::get_student_work(year = "22_23", update = TRUE) |>
  dplyr::filter(Finished == TRUE)

student_work_sheet <- googlesheets4::read_sheet(ss = "15ixca0QKloZtYLcmj_9Uc20zdQ5FE6pSVj3EBamLoiI")

### Update Student Work Sheet if Needed ###

#### Make grade bands, replace file names, check if pre post by teacher name ####
student_work2 <- student_work |>
  dplyr::mutate(
    grade_band = dplyr::case_when(
      !is.na(`grade_level_17`) ~ "Pre-K",
      !is.na(`grade_level_19`) ~ "K-2",
      !is.na(`grade_level_14`) ~ "K-2",
      !is.na(`grade_level_15`) ~ "K-2",
      !is.na(`grade_level_16`) ~ "3-5",
      !is.na(`grade_level_3`) ~ "3-5",
      !is.na(`grade_level_4`) ~ "3-5",
      !is.na(`grade_level_5`) ~ "3-5",
      !is.na(`grade_level_6`) ~ "6-8",
      !is.na(`grade_level_7`) ~ "6-8",
      !is.na(`grade_level_8`) ~ "6-8",
      !is.na(`grade_level_9`) ~ "9-12",
      !is.na(`grade_level_10`) ~ "9-12",
      !is.na(`grade_level_11`) ~ "9-12",
      !is.na(`grade_level_12`) ~ "9-12",
      !is.na(`grade_level_13`) ~ "Other"
    ),
    grade = dplyr::case_when(
      !is.na(`grade_level_17`) ~ "Pre-K",
      !is.na(`grade_level_19`) ~ "K",
      !is.na(`grade_level_14`) ~ "1",
      !is.na(`grade_level_15`) ~ "2",
      !is.na(`grade_level_16`) ~ "3",
      !is.na(`grade_level_3`) ~ "3",
      !is.na(`grade_level_4`) ~ "4",
      !is.na(`grade_level_5`) ~ "5",
      !is.na(`grade_level_6`) ~ "6",
      !is.na(`grade_level_7`) ~ "7",
      !is.na(`grade_level_8`) ~ "8",
      !is.na(`grade_level_9`) ~ "9",
      !is.na(`grade_level_10`) ~ "10",
      !is.na(`grade_level_11`) ~ "11",
      !is.na(`grade_level_12`) ~ "12",
      !is.na(`grade_level_13`) ~ "Other"
    ),
    File_Name = stringr::str_replace_all(File_Name, "zip", "pdf"),
    teacher_name2 = tolower(teacher_name),
    teacher_id = paste0(tolower(initials), dob)
  ) |>
  dplyr::group_by(teacher_name2) |>
  dplyr::mutate(Prepost = dplyr::case_when(
    is.na(teacher_name2) ~ NA,
    dplyr::row_number() == 1 ~ "Pre",
    dplyr::row_number() == 2 ~ "Post",
    TRUE ~ NA
  )) |>
  dplyr::ungroup()

student_work_additional_files <- student_work2 |>
  dplyr::filter(additional_yes_no == "Yes") |>
  dplyr::select(-File_Name, -File_Id) |>
  tidyr::pivot_longer(contains("additional_files_Name"), names_to = "add_file_name", values_to = "File_Name") |>
  tidyr::drop_na(File_Name) |>
  dplyr::mutate(File_Id = dplyr::case_when(
    add_file_name == "1_additional_files_Name" ~ `1_additional_files_Id`,
    add_file_name == "2_additional_files_Name" ~ `2_additional_files_Id`,
    add_file_name == "3_additional_files_Name" ~ `3_additional_files_Id`,
    add_file_name == "4_additional_files_Name" ~ `4_additional_files_Id`,
    add_file_name == "5_additional_files_Name" ~ `5_additional_files_Id`
  )) |>
  dplyr::mutate(Prepost = case_when(
    RecordedDate >= as.Date("2023-06-15") & round == "PL - Second submission of the school year" ~ "Post",
    RecordedDate >= as.Date("2023-06-15") & round == "PL - First submission of the school year" ~ "Pre",
    T ~ NA
  )) |>
  dplyr::select(
    `Date of Submission` = RecordedDate,
    `Student Work File` = File_Name,
    `Student Work File ID` = File_Id,
    `Student Work Survey ID` = ResponseId,
    `Teacher or Coach` = teacher_or_coach,
    `Teacher Name` = teacher_name,
    `Teacher Initials` = initials,
    `Teacher DOB` = dob,
    Site = site,
    `District 9` = district9,
    `District 11` = district11,
    MA_DESE = ma_dese,
    `Grade Band` = grade_band,
    Grade = grade,
    Class = class,
    `Subject Area` = subject,
    `# of Students` = `#_of_students_1`,
    Prepost
  )

student_work_selected <- student_work2 |>
  dplyr::mutate(Prepost = case_when(
    RecordedDate >= as.Date("2023-06-15") & round == "PL - Second submission of the school year" ~ "Post",
    RecordedDate >= as.Date("2023-06-15") & round == "PL - First submission of the school year" ~ "Pre",
    T ~ NA
  )) |>
  dplyr::select(
    `Date of Submission` = RecordedDate,
    `Student Work File` = File_Name,
    `Student Work File ID` = File_Id,
    `Student Work Survey ID` = ResponseId,
    `Teacher or Coach` = teacher_or_coach,
    `Teacher Name` = teacher_name,
    `Teacher Initials` = initials,
    `Teacher DOB` = dob,
    Site = site,
    `District 9` = district9,
    `District 11` = district11,
    MA_DESE = ma_dese,
    `Grade Band` = grade_band,
    Grade = grade,
    Class = class,
    `Subject Area` = subject,
    `# of Students` = `#_of_students_1`,
    Prepost
  ) |>
  dplyr::bind_rows(student_work_additional_files) |>
  dplyr::left_join(student_work_sheet |> dplyr::select(`Student Work File ID`, Prepost), by = c("Student Work File ID")) |>
  dplyr::mutate(Prepost = dplyr::coalesce(Prepost.x, Prepost.y)) |>
  dplyr::select(-Prepost.x, -Prepost.y) |>
  dplyr::arrange(`Date of Submission`)

sheet_length <- nrow(student_work_selected) + 1
sheet_cols <- LETTERS[ncol(student_work_selected)]

### Write to google sheet, overwriting previous data, except for Submitted Grade which is column O ###
student_work_selected |>
  dplyr::filter(`Date of Submission` > max(student_work_sheet$`Date of Submission`)) |>
  (\(.) if (nrow(.) >= 1) {
    googlesheets4::sheet_append(
      data = .,
      ss = "15ixca0QKloZtYLcmj_9Uc20zdQ5FE6pSVj3EBamLoiI",
      sheet = "Student Work Scores"
    )
  })()
# googlesheets4::range_write(
#   ss = "15ixca0QKloZtYLcmj_9Uc20zdQ5FE6pSVj3EBamLoiI",
#   sheet = "Student Work Scores",
#   col_names = FALSE,
#   reformat = FALSE,
#   range = glue::glue("A2:{sheet_cols}{sheet_length}")
# )

student_work_submitted <- student_work2 |>
  dplyr::select(
    `Date of Submission` = RecordedDate,
    `Student Work File` = File_Name,
    `Student Work File ID` = File_Id,
    `Student Work Survey ID` = ResponseId,
    `Teacher or Coach` = teacher_or_coach,
    `Teacher Name` = teacher_name,
    `Teacher Initials` = initials,
    `Teacher DOB` = dob,
    Site = site,
    `District 9` = district9,
    `District 11` = district11,
    MA_DESE = ma_dese,
    `Grade Band` = grade_band,
    Grade = grade,
    Class = class,
    `Subject Area` = subject,
    `# of Students` = `#_of_students_1`
  ) |>
  dplyr::arrange(`Date of Submission`)

sheet_length <- nrow(student_work_submitted) + 1
sheet_cols <- LETTERS[ncol(student_work_submitted)]

### Write to google sheet, overwriting previous data, except for Submitted Grade which is column O ###
student_work_submitted |>
  googlesheets4::range_write(
    ss = "1tTZdee6JmcdImpCXS6kkZCa9m2x4gxXCB1Z1o88ppGs",
    sheet = "Student Work Submissions",
    col_names = TRUE,
    reformat = FALSE,
    range = glue::glue("A1:{sheet_cols}{sheet_length}")
  )

######### File Download for Deploy ###############
response_files <- student_work |>
  dplyr::filter(RecordedDate >= Sys.Date() - 1) |>
  dplyr::select(
    ResponseId, File_Id, `1_additional_files_Id`, `2_additional_files_Id`, `3_additional_files_Id`,
    `4_additional_files_Id`, `5_additional_files_Id`
  )

currently_downloaded_files_short <- list.files("~/Teaching Lab/Coding/student_work_samples/www/pdfs", full.names = FALSE)
currently_downloaded_files_long <- list.files("~/Teaching Lab/Coding/student_work_samples/www/pdfs", full.names = TRUE)

##### REMOVES OLD FILES ########
### Get all files that have graded submissions ###
### Add .pdf to string, as well as full file path ###
student_work_sheet |>
  filter(!is.na(`Submitted By`)) |>
  pull(`Student Work File ID`) %>%
  paste0("/Users/dunk/Teaching Lab/Coding/student_work_samples/www/pdfs/", ., ".pdf") -> remove_files

### Get all files that have been graded that are in the grading dashboard ###
final_remove <- remove_files[which(remove_files %in% currently_downloaded_files_long)]

### Remove the files if there are any to be removed ###
if (length(final_remove) >= 1) {
  file.remove(final_remove)
}
##### END OF REMOVING OLD FILES ########

##### Downloads New Files #####
### Get all files that do not have a submission yet ###
student_work_sheet |>
  filter(is.na(`Submitted By`)) |>
  pull(`Student Work File ID`) %>%
  paste0("/Users/dunk/Teaching Lab/Coding/student_work_samples/www/pdfs/", ., ".pdf") -> needed_files

### Reduce to just character string of student work file ID ###
### Filter for only those not in currently downloaded files ###
files_pull <- needed_files[which(!needed_files %in% currently_downloaded_files_long)] |>
  str_remove_all("/Users/dunk/Teaching Lab/Coding/student_work_samples/www/pdfs/|.pdf")

if (length(files_pull) >= 1) {
  
  ### Make a data table of just response id's and file id's that do not have a graded submission ###
  response_files <- student_work_sheet |>
    filter(is.na(`Submitted By`)) |>
    dplyr::select(ResponseId = `Student Work Survey ID`, File_Id = `Student Work File ID`) |>
    dplyr::filter(File_Id %in% files_pull)
  
  ### List of all API Request URLs (includes additional file submissions since the google sheet it is pulling from already has submissions) ###
  url <- purrr::map2_chr(
    response_files$ResponseId, response_files$File_Id,
    ~ glue::glue("https://iad1.qualtrics.com/API/v3/surveys/SV_6nwa9Yb4OyXLji6/responses/{.x}/uploaded-files/{.y}")
  )
  
  ### Download all submissions with file names as file ids ###
  needed_submissions <- purrr::walk2(
    url,
    response_files$File_Id,
    ~ httr::GET(
      url = .x,
      httr::add_headers(`X-API-TOKEN` = "r1vgrzHjb3AQrBQEKgLXd8khdF5R7FFjP5lp7bzT"),
      httr::write_disk(paste0("~/Teaching Lab/Coding/student_work_samples/www/pdfs/", .y, ".pdf"), overwrite = T),
      httr::progress()
    )
  )
  
} else {
  print("No files need to be downloaded!")
}

########################################################################################
### Old code to ###
### Get count of pdf pages ###
# files <- keep(list.files(here::here("File"), full.names = TRUE), ~ str_detect(.x, "\\.pdf"))
# library(pdftools)
# files_pages <- map_int(files, ~ pdf_info(.x)$pages)
#
# data.frame(file = gsub(pattern = "^(?:[^_]*_){2}([^.]*)\\.",
#                        replacement = "\\1.",
#                        keep(list.files(here::here("File")), ~ str_detect(.x, "\\.pdf"))),
#            pages = files_pages) |>
#   left_join(student_work, by = c("file" = "File_Name")) |>
#   group_by(subject) |>
#   summarise(n = sum(pages, na.rm = TRUE))
######################################################