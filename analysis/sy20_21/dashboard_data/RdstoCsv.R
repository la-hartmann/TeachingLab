library(readr)
library(here)
library(googledrive)
library(data.table)

setwd(here("Data/"))

files <- list.files(pattern = "*.rds") %>%
  map( ~ read_rds(.))

dashboard_data <- as_tibble(files[[1]])
performance_task <- as_tibble(files[[2]])
google_sheets_sy20_21 <- as_tibble(files[[3]])
moodle_export_reformat <- as_tibble(files[[4]])
moodle_new_questions <- as_tibble(files[[5]])
google_sheets_sy19_20 <- as_tibble(files[[6]])
sheets_data_merge <- as_tibble(files[[7]])

files_nords <- str_remove_all(files, ".rds")

write_csv(dashboard_data, here("Data/dashboard_data.csv"))
write_csv(performance_task, here("Data/performance_task.csv"))
write_csv(google_sheets_sy20_21, here("Data/google_sheets_sy20_21.csv"))
write_csv(moodle_export_reformat, here("Data/moodle_export_reformat.csv"))
write_csv(moodle_new_questions, here("Data/moodle_new_questions.csv"))
write_csv(google_sheets_sy19_20, here("Data/google_sheets_sy19_20.csv"))
write_csv(sheets_data_merge, here("Data/sheets_data_merge.csv"))

folder <- drive_update("Data/Dashboard Data")

list.files(pattern = "*.csv") %>%
  map( ~ drive_upload(.x, path = folder)) %>%
  slowly()

drive_ls(folder)


