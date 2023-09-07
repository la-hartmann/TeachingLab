library(googledrive)
library(googlesheets4)
library(openxlsx)
library(readxl)
library(tidyverse)

info_writing_1 <- read_xlsx("~/Downloads/Pre-Test Informational Writing A Gr 7-10.xlsx") |>
  dplyr::slice(-1) |>
  dplyr::rename(`Open Ended Response` = last_col())
info_writing_2 <- read_xlsx("~/Downloads/Pre-Test Informational Writing B Gr 7-10.xlsx") |>
  dplyr::rename(`Open Ended Response` = last_col())
info_writing_3 <- read_xlsx("~/Downloads/Post-Test Informational Writing A Gr 7-10.xlsx") |>
  dplyr::rename(`Open Ended Response` = last_col())
info_writing_4 <- read_xlsx("~/Downloads/Post-Test Informational Writing B Gr 7-10.xlsx") |>
  dplyr::slice(-1) |>
  dplyr::rename(`Open Ended Response` = last_col())
persuasive_writing_1 <- read_xlsx("~/Downloads/Pre-Test Persuasive Writing A Gr 7-10.xlsx") |>
  dplyr::rename(`Open Ended Response` = last_col())
persuasive_writing_2 <- read_xlsx("~/Downloads/Pre-Test Persuasive Writing B Gr 7-10.xlsx") |>
  slice(-1) |>
  dplyr::rename(`Open Ended Response` = last_col())
persuasive_writing_3 <- read_xlsx("~/Downloads/Post-Test Persuasive Writing A Gr 7-10.xlsx") |>
  dplyr::rename(`Open Ended Response` = last_col())
persuasive_writing_4 <- read_xlsx("~/Downloads/Post-Test Persuasive Writing B Gr 7-10.xlsx") |>
  dplyr::rename(`Open Ended Response` = last_col())

all_files <- list_rbind(list(info_writing_1, info_writing_2, 
                             info_writing_3, info_writing_4, 
                             persuasive_writing_1, persuasive_writing_2,
                             persuasive_writing_3, persuasive_writing_4))

### Treatment Classroom ###
treatments <- googledrive::drive_ls("https://drive.google.com/drive/folders/1aGNfokbaRXau6eiVKOw6JI6ZU4xtp5rB")

### Control Classroom ###
controls <- googledrive::drive_ls("https://drive.google.com/drive/folders/1cIn0bXBQnxxlImHDu17uHkGCYu-3LBd3")

extract_sheet <- function(x) {
  
  Sys.sleep(1)
  
  sheet_in_folder_id <- googledrive::drive_ls(as_id(x)) |>
    dplyr::filter(stringr::str_detect(name, "Tracker|tracker") & name != "Skyview Semester Student Tracker") |>
    pull(id)
  
  tracker_sheet_names <- sheet_in_folder_id |>
    googlesheets4::sheet_names()
  
  list_rbind(map(tracker_sheet_names, ~ read_sheet(sheet_in_folder_id, sheet = .x) |>
                   dplyr::mutate(across(everything(), ~ as.character(.x)))))
  
}

controls_trackers <- list_rbind(map(controls$id, ~ extract_sheet(.x)))

treatment_trackers <- list_rbind(map(treatments$id, ~ extract_sheet(.x)))

all_trackers <- list_rbind(list(treatment_trackers, controls_trackers))

trackers_final <- all_trackers |>
  mutate(Name = tolower(paste(`First Name`, `Last Name`))) |>
  select(`Collector ID`, Name)
  
final_file_join <- function(file, file_name) {
  
  if ("Collector ID" %in% colnames(file)) {
    file <- file |>
      select(-`Collector ID`)
  }
  
  file_to_write <- file |>
    mutate(Name = tolower(paste(`Student First Name`, `Student Last Name`))) |>
    left_join(trackers_final, by = "Name", relationship = "many-to-many")
  
  openxlsx::write.xlsx(x = file_to_write, file = paste0("~/Downloads/", file_name))
} 

final_file_join(info_writing_1, "Pre-Test Informational Writing A Gr 7-10 with IDs.xlsx")
final_file_join(info_writing_2, "Pre-Test Informational Writing B Gr 7-10 with IDs.xlsx")
final_file_join(info_writing_3, "Post-Test Informational Writing A Gr 7-10 with IDs.xlsx")
final_file_join(info_writing_4, "Post-Test Informational Writing B Gr 7-10 with IDs.xlsx")
final_file_join(persuasive_writing_1, "Pre-Test Persuasive Writing A Gr 7-10 with IDs.xlsx")
final_file_join(persuasive_writing_2, "Pre-Test Persuasive Writing B Gr 7-10 with IDs.xlsx")
final_file_join(persuasive_writing_3, "Post-Test Persuasive Writing A Gr 7-10 with IDs.xlsx")
final_file_join(persuasive_writing_4, "Post-Test Persuasive Writing B Gr 7-10 with IDs.xlsx")


### Jessica asking for new file to be joined as of (06/16/2023) ###
persuasive_writing_a <- read_xlsx("~/Downloads/Pre-Test Persuasive Writing A.xlsx") |>
  slice(-1)

final_file_join(persuasive_writing_a, "Pre-Test Persuasive Writing A Gr 7-10 with IDs.xlsx")
