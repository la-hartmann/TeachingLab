library(googlesheets4)
library(tidyverse)
library(TeachingLab)
library(rcanvas)
set_canvas_domain("https://nmped.instructure.com/")

##### Get all teacher names, emails, and roles from tracker google sheet #####
names_and_emails <- read_sheet("https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=0",
  sheet = 1,
  range = "A:C"
)

###### Educator survey completion ######
df_name_select <- read_sheet("https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=0",
  sheet = "Data Tracker"
) %>%
  colnames()

#### Find column letter for educator survey completion in sheet ####
select <- which(df_name_select == "Completed TL New Mexico Educators Survey SY21-22 (pre) (Y/N) - SM")
letters[select] %>%
  str_to_upper() -> selection

##### Retrieve New Mexico Educator Survey #####
nm_survey <- surveymonkey::fetch_survey_obj(315553653) %>%
  surveymonkey::parse_survey()

#### Get emails from survey, make lowercase ####
emails_session_1 <- nm_survey %>%
  # filter(lubridate::date(date_created) == as.Date("2022-01-12")) %>%
  select(`Work email`) %>%
  drop_na() %>%
  mutate(email_address = str_to_lower(`Work email`)) %>%
  select(2)

#### Join emails to educator survey completion ####
joined_emails <- read_sheet("https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=0", sheet = 1) %>%
  select(1, 2) %>%
  mutate(
    Email = str_to_lower(Email),
    completed = ifelse(Email %in% emails_session_1$email_address, "Complete", "Not Complete")
  )

#### Select just completion column and write to tracker google sheet ####
joined_emails %>%
  select(completed) %>%
  range_write(
    ss = "https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=0",
    sheet = 1,
    range = glue::glue("{selection}2:{selection}{length(joined_emails$completed) + 1}"),
    col_names = F,
    reformat = F
  )

##### Section for columns D-G from Educator Survey #####
nm_survey_selected <- nm_survey %>%
  select(
    `Work email`,
    `Which of the following best describes your primary role? <br><br>`,
    `In your role this school year, do you provide instruction to students? This instruction could be as students’ primary teacher, support or co-teacher, a push-in or pull-out specialist, etc.`,
    `Do students have student identification numbers at your school?`,
    `Do students have access to personal devices in the classroom on which they could take a survey (e.g., 1:1 laptop, tablet, smartphone)? `
  ) %>%
  mutate(email_address = str_to_lower(`Work email`)) %>%
  select(-`Work email`) %>%
  janitor::remove_empty("rows") %>%
  drop_na(email_address)

#### Check if there are any mismatched emails from the google sheet tracker to the surveymonkey survey
not_extant <- setdiff(stringr::str_to_lower(names_and_emails$Email), nm_survey_selected$email_address)
check_emails <- not_extant[which(!not_extant %in% stringr::str_to_lower(names_and_emails$Email))]
if (length(check_emails) > 0) {
  print("Email mismatch!") # If there is an email which doesn't transcribe from canvas
  # to the google sheet stop code and manually check the difference
  break
}

#### Join tracker google sheet emails to new mexico survey then select completion columns and write to sheet ####
names_and_emails %>%
  select(Email) %>%
  mutate(lower_email = stringr::str_to_lower(Email)) %>%
  left_join(nm_survey_selected, by = c("lower_email" = "email_address")) %>%
  select(-lower_email) %>%
  mutate(across(everything(), ~ replace_na(as.character(.x), "No response"))) %>%
  select(-Email) %>%
  purrr::set_names(., nm = c("D", "E", "F", "G")) %>%
  range_write(
    ss = "https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=0",
    sheet = 1,
    range = glue::glue("D2:G{length(names_and_emails$Email) + 1}"),
    col_names = F,
    reformat = F
  )

##### Section for if student survey has been administered #####
#### Subsequent one for count of student data submissions ####
nm_student_survey <- surveymonkey::fetch_survey_obj(315708746) %>%
  surveymonkey::parse_survey()
### Teacher last names from google sheet data tracker ###
teacher_last_names <- read_sheet("https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=0", sheet = 1) %>%
  select(1, 2, 3) %>%
  mutate(name2 = str_remove_all(`Participant Name`, ".* "))

####### Running list of corrections to standardize teacher names from student input to survey #######
teacher_name_replace <- c("Thomasbarkdale" = "Barksdale",
                          "Barkesdail" = "Barksdale")
### Teacher last names from surveymonkey student survey ###
teacher_names <- nm_student_survey %>%
  distinct(name = `What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`) %>%
  arrange() %>%
  drop_na(1) %>%
  filter(name %!in% c("1", "Jamielopez")) %>% # Jamie lopez removal?
  # expand(name, name1 = name) %>%
  # mutate(lev = stringdist::stringdist(name, name1)) %>%
  # filter(lev == 2)
  identity() %>%
  mutate(name = str_to_title(str_remove_all(name, ".* |.*\\."))) %>%
  mutate(name = str_replace_all(name, teacher_name_replace)) %>% ##### NAME REPLACE OCCURS HERE
  distinct(name) %>%
  filter(name != "")

### Get teacher names based on last name ###
teacher_names_final <- teacher_names %>%
  left_join(teacher_last_names, by = c("name" = "name2")) %>%
  select(`Participant Name`) %>%
  drop_na() %>%
  arrange(`Participant Name`)

### Print teacher names ###
print(teacher_names_final, n = nrow(teacher_names_final))

### Decide if code can proceed ###
proceed <- readline(prompt = "Do any changes need to be made? (y/n)")

### Conditional to decide proceeding ###
if (proceed == "y") {
  print("Make a change in the student responses to avoid duplicates!")
  break
}

### Create data frame of student data completion based on name from earlier retrieval ###
join_names <- tibble::tibble(name = teacher_last_names$`Participant Name`) %>%
  mutate(completed = ifelse(name %in% teacher_names_final$`Participant Name`, "Yes", "No"))

#### Quick manual name checking for student input ####
unadjusted_teacher_names <- stringr::str_to_lower(sort(unique(nm_student_survey$`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`))) %>%
  purrr::keep( ~ !str_detect(.x, "bark|badd|ong|antos|tiff|cisn|lop|vig|cote|tiz|bandl|aqui|trej|marj|marij|alis|alasaisis"))
print(unadjusted_teacher_names)
if (setequal(unadjusted_teacher_names, c("1", "daniel aguilar", "mrs. agino")) == FALSE) {
  print("Check the above names to see which need to be standardized")
  stop()
}

##### CHANGE THIS SCRIPT SO IT WORKS ALGORITHMICALLY #####
##### Idea: look for a unique string in a vector of all names, then pre-create list for that #####
##### string to be adjusted to #####
###### REMINDER TO IGNORE daniel aguilar ########

### Rename all the nm student survey teachers based on minimal strings to get a grouped count ###
nm_student_survey_n <- nm_student_survey %>%
  mutate(teacher_real_name = case_when(
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "bark") == T ~ "Tommy Barksdale",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "badd") == T ~ "Rajasekhar Badditi",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "ong") == T ~ "Rita Kwong",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "antos") == T ~ "Maria Santos",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "tiff") == T ~ "Tiffany Portiz",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "cisn") == T ~ "Maria Dolores Cisneros",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "lop") == T ~ "Jamie Lopez",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "vig") == T ~ "Thomas Vigil",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "cote") == T ~ "Sue Cote",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "tiz") == T ~ "Tiffany Portiz",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "bandl") == T ~ "Raj Bandla",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "aqui") == T ~ "Gilbert Aquino",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "trej") == T ~ "Elisa Trejo",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "marj|marije") == T ~ "Marjie Lyn Pineda",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "alis|alasaisis") == T ~ "Josiah Alisasis",
    TRUE ~ as.character(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`)
  )) %>%
  dplyr::filter(teacher_real_name != "1") %>%
  group_by(teacher_real_name) %>%
  count(sort = T) %>%
  drop_na(teacher_real_name)

#### Join names to student survey then select completed while replacing NA with "No Responses" to google sheet ####
join_names %>%
  left_join(nm_student_survey_n, by = c("name" = "teacher_real_name")) %>%
  select(completed, n) %>%
  mutate(n = replace_na(as.character(n), "No responses")) %>%
  range_write(
    ss = "https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=0",
    sheet = 1,
    range = glue::glue("W2:X{length(join_names$name) + 1}"),
    col_names = F,
    reformat = F
  )

##### Section for canvas usage ######
### To get canvas specific data use some of below, right now only need gradebook for specific course ###
# courses <- get_course_list()
# course_analytics <- get_course_analytics_data(course_id = 1821, type = "activity")
# course_items <- get_course_items(course_id = 1821, item = "users", include = "email")
course_gradebook <- get_course_gradebook(course_id = 1821)

### Data Collection Agreement Completion ###
## Name Replacement to match canvas to google sheet ##
name_replace_canvas <- c(
  "Hidalgo Christina" = "Christina Hidalgo",
  "Richard Greywolf" = "Rich Greywolf",
  "Amber Mccabe" = "P. Amber Mccabe",
  "Janet Gladu" = "Dr. Janet Gladu",
  "Nagarajan Bandla" = "Raj Bandla",
  "Jami D. Jacobson" = "Jami Jacobson",
  "Jamie Hephner" = "Jamie Lopez", # Might change later?
  "Susan J Cote" = "Sue Cote",
  "Amber M. Swinney" = "Amber Swinney",
  "Adrian A" = "Adrian Apodaca",
  "Tiffany Barrion" = "Tiffany Portiz",
  "Monica J. Martinez-Archuleta" = "Monica Martinez-Archuleta",
  "Michael Yara" = "Mike Yara",
  "Jacob-Michael Kelly" = "Matthew Kelly",
  "Maria D. Cisneros" = "Maria Dolores Cisneros"
)

## Completed with name replacement ##
completed_data_collection <- course_gradebook %>%
  filter(assignment_name == "Data Collection Agreement Form") %>%
  mutate(
    completed = ifelse(!is.na(submitted_at), "Yes", "No"),
    user.name = str_to_title(user.name)
  ) %>%
  select(user.name, completed) %>%
  distinct(user.name, .keep_all = TRUE) %>%
  mutate(user.name = str_replace_all(user.name, name_replace_canvas))

### Decide if code can proceed by checking name difference here, 
### Lorenzo Gonzales and Donna Lucero don't actually exist as far as I can tell ###
proceed2 <- setdiff(names_and_emails$`Participant Name`, completed_data_collection$user.name)

### Conditional to decide proceeding ###
if (length(proceed2) != 2) {
  print("Make a change in the student responses to avoid duplicates!")
  print(proceed2)
  break
}

### Join tracker sheet names and emails to canvas data collection agreement ###
join_completed_data_collection <- names_and_emails %>%
  select(`Participant Name`) %>%
  left_join(completed_data_collection, by = c("Participant Name" = "user.name"))

### Write just canvas data agreement completion column to google sheet ###
join_completed_data_collection %>%
  select(completed) %>%
  range_write(
    ss = "https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=0",
    sheet = 1,
    range = glue::glue("N2:N{length(join_completed_data_collection$completed) + 1}"),
    col_names = F,
    reformat = F
  )

### Uploaded Classroom Observation Video Recording 1 (Y/N) - Canvas ###
completed_classroom_video <- course_gradebook %>%
  filter(assignment_name == "Classroom Observation Video Recording") %>%
  mutate(
    completed = ifelse(!is.na(submitted_at), "Yes", "No"),
    user.name = str_to_title(user.name)
  ) %>%
  select(user.name, completed) %>%
  distinct(user.name, .keep_all = TRUE) %>%
  mutate(user.name = str_replace_all(user.name, name_replace_canvas)) %>%
  left_join(names_and_emails, by = c("user.name" = "Participant Name"))

### Join names to completed and change completed to admin if they are an admin ###
join_classroom_video_completed <- names_and_emails %>%
  select(1, 3) %>%
  left_join(completed_classroom_video %>% select(1, 2), by = c("Participant Name" = "user.name")) %>%
  mutate(completed = ifelse(Role == "Admin", "Admin", completed))

### Write just canvas video completion to google sheet ###
join_classroom_video_completed %>%
  select(completed) %>%
  range_write(
    ss = "https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=0",
    sheet = 1,
    range = glue::glue("O2:O{length(join_classroom_video_completed$completed) + 1}"),
    col_names = F,
    reformat = F
  )




######## Not used section #######

# nm_survey %>%
#   filter(lubridate::date(date_created) == as.Date("2022-01-18")) %>%
#   select(`Work email`) %>%
#   drop_na() %>%
#   mutate(email_address = str_to_lower(`Work email`)) %>%
#   select(2) -> emails_session_2
#
# joined_emails_2 <- read_sheet("https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=0", sheet = 1) %>%
#   select(1, 2) %>%
#   mutate(Email = str_to_lower(Email),
#          completed = ifelse(Email %in% emails_session_2$email_address, "Complete", "Not Complete"))
#
# joined_emails_2 %>%
#   select(completed) %>%
#   range_write(ss = "https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=0",
#               sheet = 1,
#               range = glue::glue("R2:R{length(joined_emails$completed) + 1}"),
#               col_names = F)

# blue_background <- googlesheets4:::CellData(
#   userEnteredFormat = googlesheets4:::new(
#     "CellFormat",
#     backgroundColor = googlesheets4:::new(
#       "Color",
#       red = 159 / 255, green = 183 / 255, blue = 220 / 255
#     )
#   )
# )
#
# range_clear(ss = "https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=0",
#             sheet = 1,
#             range = glue::glue("Q2:Q{length(joined_emails$completed) + 1}"))
########################################################################################################
