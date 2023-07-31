library(tidyverse)
library(googlesheets4)
library(TeachingLab)
library(surveymonkey)

ss <- "https://docs.google.com/spreadsheets/d/1RwTgvE_vp7dqxbRSAXXPiN_t3AVfVtN0fNoGDz49HlY/edit#gid=0"
sheet <- "Mississippi_ALL_Data Tracker"

data <- read_sheet(
  ss = ss,
  sheet = sheet
)

### District replacement list so that we have the same format for comparing ids ###
district_replace <- c(
  "Columbus Municipal" = "Columbus Municipal School District, MS",
  "Greenwood Leflore" = "Greenwood Leflore Consolidated School District, MS",
  "Hazlehurst City Schools" = "Hazlehurst City School District, MS",
  "Holmes County" = "Holmes County Consolidated School District, MS",
  "Jackson Public Schools" = "Jackson Public Schools, MS",
  "Kemper County" = "Kemper County School District, MS",
  "Laurel School District" = "Laurel School District, MS",
  "Marshall County" = "Marshall County School District, MS",
  "Meridian Public School District" = "Meridian Public School District, MS",
  "Mississippi Achievement (Yazoo)" = "Mississippi Achievement School District (Yazoo), MS",
  "Natchez-Adams School District" = "Natchez-Adams School District, MS",
  "Noxubee County Schools" = "Noxubee County School District, MS",
  "Vicksburg Warren" = "Vicksburg-Warren County School District, MS"
)

### List of mississippi districts to filter for ###
mississippi_districts <- c(
  "Columbus Municipal School District, MS",
  "Greenwood Leflore Consolidated School District, MS",
  "Hazlehurst City School District, MS",
  "Holmes County Consolidated School District, MS",
  "Jackson Public Schools, MS",
  "Kemper County School District, MS",
  "Laurel School District, MS",
  "Marshall County School District, MS",
  "Meridian Public School District, MS",
  "Mississippi Achievement School District (Yazoo), MS",
  "Natchez-Adams School District, MS",
  "Noxubee County School District, MS",
  "Vicksburg-Warren County School District, MS"
)

### Function to find unique lower case of all NON-google sheet data, also applies to google ###
### sheets data for formatting purposes ###
unique_lower <- function(x) {
  x %>%
    tolower() %>%
    unique()
}

### Educator Survey Matching ###
pre_educator_survey <- readr::read_rds(here::here("data/sy21_22/diagnostic.rds"))
### Mississippi Educator Survey Follow Up ###
follow_up_educator_survey <- TeachingLab::get_followup_educator(update == TRUE)

## Has to be done by initials ##
initials_fix <- function(x) {
  paste0(substr(x = x, 1, 1), substr(x = x, 3, 3)) %>%
    toupper()
}

educator_initials <- pre_educator_survey %>%
  summarise(school = your_site_district_parish_network_or_school_br_br,
            initials = initials_fix(please_write_in_your_3_initials_if_you_do_not_have_a_middle_initial_please_write_x_br_this_is_used_to_link_the_diagnostic_and_follow_up_surveys_but_is_kept_confidential_br_br))

educator_df <- tibble::tibble(
  school = educator_initials$school,
  initials = educator_initials$initials
) %>%
  filter(initials != "NANA" & str_detect(school, ", MS"))
########################################

### Student Survey Matching ###
student_survey <- get_student_survey(update = T)
## Filters for Mississippi schools with str_detect ##
## Pulls from ALL select your teacher columns ##
## Replaces just one occurrence of bad formatting teacher names at the bottom since that is sufficient
## to check for string occurrence ##
teachers_names_student_survey <- student_survey %>%
  dplyr::filter(str_detect(`What is the name of your school, district, or parish?`, ", MS")) %>%
  mutate(prepost = ifelse(date_created >= as.Date("2022-05-01"), "post", "pre")) %>%
  group_by(prepost, teacher) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  tidyr::drop_na(teacher) %>%
  mutate(teacher = tolower(teacher)) %>%
  group_by(prepost) %>%
  distinct(teacher, .keep_all = T) %>%
  ungroup() %>%
  mutate(teacher = str_replace_all(teacher, c(
    "mabry" = "pearl mabry",
    "jackie leach" = "jacqueline leach",
    "rosiland norsworthy" = "rosalinda norsworthy",
    "principal - mr. mumford" = "jeff mumford",
    "dr. davenport" = "shawnnay davenport",
    "mrs. chirstmas" = "joshua christmas",
    "mrs.leach" = "jacqueline leach",
    "ms.leach" = "jacqueline leach",
    "ms.norsworthy" = "rosalinda norsworthy",
    "pearl pearl mabry" = "pearl mabry",
    "tammyclark" = "tammy clark"
  ))) %>%
  mutate(teacher = str_trim(teacher, side = "both")) %>%
  group_by(prepost) %>%
  distinct(teacher, .keep_all = T) %>%
  ungroup() %>%
  arrange(prepost) %>%
  select(-n)
########################################

### Family Survey Matching ###
family_survey <- get_family_survey(update = T)
## Filters for mississippi schools withy str_detect ##
## Pulls from ALL select your teacher columns ##
teachers_names_family_survey_pre <- family_survey %>%
  dplyr::filter(str_detect(`What is the name of your child's school, district, or parish?`, ", MS") & 
                  date_created < as.Date("2022-05-01")) %>%
  dplyr::select(`teacher`) %>%
  tidyr::pivot_longer(everything()) %>%
  tidyr::drop_na() %>%
  pull(value) %>%
  unique_lower() %>%
  str_replace_all(c("dr. carrie chadic" = "carrie chadic",
                    "mrs, robertson" = "nykol robertson"))
########################################
####### POST DATA ########
teachers_names_family_survey_post <- family_survey %>%
  dplyr::filter(str_detect(`What is the name of your child's school, district, or parish?`, ", MS") & 
                  date_created >= as.Date("2022-05-01")) %>%
  dplyr::select(`teacher`) %>%
  tidyr::pivot_longer(everything()) %>%
  tidyr::drop_na() %>%
  pull(value) %>%
  unique_lower() %>%
  str_replace_all(c("dr. carrie chadic" = "carrie chadic",
                    "mrs, robertson" = "nykol robertson"))

### Student Work Samples Matching ###
course_token <- Sys.getenv("course_token")
# 314125242
student_work_samples_survey <- surveymonkey::fetch_survey_obj(311633359, oauth_token = course_token) %>%
  surveymonkey::parse_survey(oauth_token = course_token) %>%
  group_by(respondent_id) %>%
  summarise_all(coalesce_by_column) %>%
  filter(response_status != "partial" & !is.na(`Please upload a single file of your student work here, without any identifying information._2`))
## Grabs submitted emails from gift card column ##
student_work_emails_pre <- student_work_samples_survey %>%
  filter(date_created < as.Date("2022-05-01")) %>%
  pull(`What is the teacher's name for whom you are submitting student work samples?`) %>%
  unique_lower()

student_work_emails_post <- student_work_samples_survey %>%
  filter(date_created >= as.Date("2022-05-01")) %>%
  pull(`What is the teacher's name for whom you are submitting student work samples?`) %>%
  unique_lower()

if (length(student_work_emails_post) == 0) {
  student_work_emails_post <- "fake_email123"
}
########################################

### Gets IPG Forms for matching ###
classroom_obs <- TeachingLab::get_ipg_forms(update = TRUE)

classroom_obs_names_pre <- classroom_obs %>%
  filter(`Date of Observation` < as.Date("2022-05-09")) %>%
  pull(`Name of teacher observed (for Mississippi Coaching Project - alphabetized by first name)`) %>%
  purrr::keep(~ !is.na(.x)) %>%
  unique_lower()

classroom_obs_names_post <- classroom_obs %>%
  filter(Timestamp >= as.Date("2022-05-09")) %>%
  pull(`Name of teacher observed (for Mississippi Coaching Project - alphabetized by first name)`) %>%
  purrr::keep(~ !is.na(.x)) %>%
  unique_lower()

if (length(classroom_obs_names_post) == 0) {
  classroom_obs_names_post <- "fake_name123"
}
########################################

#### Format for Sheet ####

### Get initials district combination from google sheets ###
initial_data_tracking <- data %>%
  mutate(
    District = str_replace_all(District, district_replace),
    initials_district = tolower(paste0(`Teacher code`, District))
  ) %>%
  pull(initials_district)

### Get initials district combination from educator survey, and then filter for only ones ###
### in the google sheet ###

ed_df_init <- educator_df %>%
  drop_na(school) %>%
  mutate(
    initials_district = tolower(paste0(initials, school)),
    `Educator Survey` = T
  ) %>%
  select(initials_district, `Educator Survey`) %>%
  filter(initials_district %in% initial_data_tracking)

### Get follow up data in same format ###
follow_up_initials <- follow_up_educator_survey %>%
  filter(str_detect(`Your site (district, parish, network, or school)`, "MS")) %>%
  mutate(initials = initials_fix(`Please write in your 3 initials. If you do not have a middle initial, please write X.<br>(This is used to link the diagnostic and follow-up surveys, but is kept confidential.)<br><br>`),
         school = `Your site (district, parish, network, or school)`) %>%
  summarise(initials_district = tolower(paste0(initials, school)))

### Create data with true-false format for educator survey, student survey, family survey, ###
### student work  samples, and classroom observations ###
### Educator Survey matches by initials and district ### CORRECT as of 3-1-22
### Student Survey matches by teacher name ### CORRECT as of 3-1-22
### Family Survey matches by teacher name ###
### Student Work Survey matches by email ### CORRECT as of 3-1-22
### Classroom observation matches by name ### CORRECT as of 3-1-22
sheet_with_response_tracking <- data |>
  dplyr::mutate(initials_district_pre = ifelse(tolower(
    paste0(`Teacher code`, 
           str_replace_all(District, district_replace))) %in% (ed_df_init$initials_district),
    T,
    F
  ),
  initials_district_post = ifelse(tolower(
    paste0(`Teacher code`, 
           str_replace_all(District, district_replace))) %in% (follow_up_initials$initials_district),
    T,
    F
  )) |>
  dplyr::mutate(
    `Educator Survey pre` = ifelse(initials_district_pre == T,
      T, F
    ),
    `Educator Survey post` = ifelse(initials_district_post == T,
                                   T, F
    ),
    `Student Survey pre` = ifelse(tolower(Name) %in% (teachers_names_student_survey %>% filter(prepost == "pre") %>% pull(teacher)),
      T, F
    ),
    `Student Survey post` = ifelse(tolower(Name) %in% (teachers_names_student_survey %>% filter(prepost == "post") %>% pull(teacher)),
                                  T, F
    ),
    `Family Survey pre` = ifelse(tolower(Name) %in% teachers_names_family_survey_pre,
      T, F
    ),
    `Family Survey post` = ifelse(tolower(Name) %in% teachers_names_family_survey_post,
                                 T, F
    ),
    `Ss work samples pre` = ifelse(tolower(Name) %in% student_work_emails_pre,
      T, F
    ),
    `Ss work samples post` = ifelse(tolower(Name) %in% student_work_emails_post,
                                   T, F
    ),
    `Classroom obs pre` = ifelse(tolower(Name) %in% classroom_obs_names_pre,
      T, F
    ),
    `Classroom obs post` = ifelse(tolower(Name) %in% classroom_obs_names_post,
                                 T, F
    )
  ) |>
  dplyr::select(-c(initials_district_pre, initials_district_post))

googlesheets4::gs4_auth()

googlesheets4::write_sheet(sheet_with_response_tracking,
  ss = ss,
  sheet = sheet
)


######### Individual Comparisons ###########

check_not_in_vector <- function(check_vector, data_compare) {
  sort(unique(check_vector[which(tolower(unique(check_vector)) %!in% tolower(data_compare))]))
}

## Can't really check educator survey since it is a combination id ##

###### Names to ignore (confirmed not from Mississippi) ######
names_ignore <- "evrret|everet|chimma|love|irving|mckee|forte|wilson|willson|valerie|gross|madison|aja|parker|ms\\. rice|miari franklin|ms\\. hill"

## Teacher Names in Student Survey not in Names - this seems accurate ##
## Names in list are currently correct names
# check_not_in_vector(teachers_names_student_survey$teacher, data$Name) %>%
#   purrr::keep( ~ !str_detect(.x, names_ignore))
# 
# ## Student Survey Matches ##
# check_not_in_vector(teachers_names_family_survey_post, data$Name) %>%
#   purrr::keep( ~ !str_detect(.x, names_ignore))
# 
# ## Student Work Sample Matches ##
# check_not_in_vector(student_work_emails, data$Email)
# cat(crayon::bgCyan("IGNORE FOLLOWING EMAILS: darmaniaco@schools.nyc.gov, lchiapp@schools.nyc.gov, ravenhawes@gmail.com"))
# 
# ## Classroom observation IPG Matches ##
# check_not_in_vector(classroom_obs_names_post, data$Name)
