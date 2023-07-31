library(tidyverse)
library(rcanvas)
library(googledrive)

### Set up Canvas Data ###
set_canvas_domain("https://nmped.instructure.com/")
course_gradebook <- get_course_gradebook(course_id = 1821)

### New Mexico Student Work Filter and Get Attachments ###
new_mexico_student_work <- course_gradebook %>%
  filter(assignment_name == "Student Work Sample Submission" & missing == F) %>%
  select(user.name, attachments) %>%
  unnest(attachments) %>%
  group_by(user.name) %>%
  mutate(n = row_number(),
         user_name_upload = paste0(str_to_title(user.name), n, ".", str_extract(display_name, "[^.]*$")))

### Get existing folder names from Google Drive ###
extant_folders <- googledrive::drive_ls(path = as_id("1W_tGsoOZmo219z862ha5zIuOFjFHX6ZY")) |>
  pull(name) |>
  sort()

### Get New Mexico Student Work Teacher Grades ###
nm_teacher_data <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=0",
                                             col_types = "c") |>
  select(`Participant Name`, `Grade level of work sample`) |>
  mutate(`Participant Name` = str_to_title(`Participant Name`),
         `Grade level of work sample` = str_replace_all(`Grade level of work sample`, ", ", "_"))

### Get Folder Names from user name and check to only make new ones ###
new_folders_to_create <- new_mexico_student_work |>
  mutate(user.name = str_to_title(user.name),
         user.name = str_replace_all(user.name, "Maria D\\. Cisneros", "Maria Dolores Cisneros")) |>
  left_join(nm_teacher_data, by = c("user.name" = "Participant Name")) |>
  select(user.name, `Grade level of work sample`) |>
  distinct(user.name, .keep_all = T) |>
  transmute(name_grade = paste0(str_to_title(user.name), "_", `Grade level of work sample`)) |>
  pull(name_grade) %>%
  .[. %!in% extant_folders] %>%
  .[. %!in% paste0("(DONE) ", extant_folders)] %>%
  sort()

### If there are  new folders to create do so ###
if (length(new_folders_to_create) >= 1) {
  map(new_folders_to_create,
      ~ googledrive::drive_mkdir(name = .x, path = as_id("1W_tGsoOZmo219z862ha5zIuOFjFHX6ZY")))
}

### New Mexico Student Work Download from url to Downloads folder ###
map2(new_mexico_student_work$url, new_mexico_student_work$user_name_upload,
    ~ download.file(url = .x, 
                    destfile = paste0(here::here("Downloads/student_work_new_mexico/"), .y)))

### Get files names to match folder names with a dataframe ###
all_files <- tibble(
  filename = list.files(here::here("Downloads/student_work_new_mexico"), full.names = T)
) %>%
  mutate(folder_match = str_extract(str_remove_all(filename, "/Users/dunk/Teaching Lab/Coding/TeachingLab/Downloads/student_work_new_mexico/"), "[^\\d]+"))

### All named folders is a dataframe matching folders to files ###
all_named_folders <- googledrive::drive_ls(path = as_id("1W_tGsoOZmo219z862ha5zIuOFjFHX6ZY")) %>%
  filter(name != "NM Math Student Work Sample Scoring") %>%
  left_join(all_files, by = c("name" = "folder_match")) %>%
  view()

### Loop over all named folders to upload the files ###
map2(all_named_folders$filename, all_named_folders$id, 
     ~ drive_upload(media = .x, path = .y, overwrite = T))
  
################################# END SCRIPT #################################