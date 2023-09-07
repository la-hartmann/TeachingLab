library(googlesheets4)
library(qualtRics)
library(tidyverse)

######################################### Site Checking #########################################

site_list <- read_sheet(
  ss = "11jlo9UeWxZGwunhDb24hZBwAKc5b8ZKM9AYNWZaUyZY",
  sheet = "FY24 Official Site List"
)

part_feed_meta <- qualtRics::metadata(surveyID = "SV_djt8w6zgigaNq0C", questions = "site")

part_feed_choices <- tibble::tibble(
  choices = purrr::map_chr(1:length(part_feed_meta$questions$QID2$choices), ~ part_feed_meta$questions$QID2$choices[[.x]]$choiceText),
  recode = as.numeric(purrr::map(1:length(part_feed_meta$questions$QID2$choices), ~ part_feed_meta$questions$QID2$choices[[.x]]$recode)),
)

educator_survey_meta <- qualtRics::metadata(surveyID = "SV_8vrKtPDtqQFbiBM", questions = "site")

educator_survey_choices <- tibble(
  choices = purrr::map_chr(1:length(educator_survey_meta$questions$QID1316616425$choices), ~ educator_survey_meta$questions$QID1316616425$choices[[.x]]$choiceText),
  recode = as.numeric(purrr::map(1:length(educator_survey_meta$questions$QID1316616425$choices), ~ educator_survey_meta$questions$QID1316616425$choices[[.x]]$recode)),
)

### Check that participant feedback and educator survey have same dropdown choices ###
identical(part_feed_choices$choices, educator_survey_choices$choices)

### Check that there aren't any sites not listed in participant feedback survey ###
setdiff(site_list$`Partner Name in Survey`, part_feed_choices$choices) -> part_feed_check

if (length(part_feed_check) > 0) {
  print(paste0("The participant feedback survey sites list needs to be updated with: ", part_feed_check))
}

### Check that there aren't any sites not listed in overall reference survey ###
setdiff(site_list$`Partner Name in Survey`, educator_survey_choices$choices) -> ed_survey_check

if (length(ed_survey_check) > 0) {
  print(paste0("The reference survey sites list needs to be updated with: ", ed_survey_check))
}

### Check subsite references ###
subsite_list <- read_sheet("11jlo9UeWxZGwunhDb24hZBwAKc5b8ZKM9AYNWZaUyZY",
  sheet = "FY24 School/District Selection for Sites"
) |>
  dplyr::select(-c(`District 11`, `District 9`)) # Remove district 11 and district 9 since they are now manual entry
### Get vertically for later ###
subsite_list_pivot <- subsite_list |>
  tidyr::pivot_longer(tidyr::everything(), names_to = "Site", values_to = "Subsite")

### Conditionally renames labels in sheet to relevant columns in surveys ###
columns_to_check <- subsite_list |>
  dplyr::rename_with(~ tolower(ifelse(stringr::str_detect(.x, "District|Network"), stringr::str_remove_all(.x, " "), stringr::str_replace_all(.x, " ", "_")))) |>
  colnames()
### Get column metadata
subcols_part_feed_meta <- qualtRics::metadata(surveyID = "SV_djt8w6zgigaNq0C", questions = columns_to_check)
subcols_educator_survey_meta <- qualtRics::metadata(surveyID = "SV_8vrKtPDtqQFbiBM", questions = columns_to_check)

get_missing_subcol_items <- function(questions_list) {
  
  subcol_question_ids <- names(questions_list)
  subcol_choices_count <- purrr::map(
    subcol_question_ids,
    ~ names(questions_list[[.x]]$choices)
  )
  
  subcol_identifier <- tibble::tibble(
    questions_ids = subcol_question_ids,
    question_choices = subcol_choices_count,
    question_text = purrr::map_chr(
      subcol_question_ids,
      ~ questions_list[[.x]]$questionText
    )
  ) |>
    dplyr::mutate(question_text = stringr::str_remove_all(question_text, "\\n.*")) |>
    tidyr::unnest_longer(question_choices)
  
  final_subcol_choices_part_feed <- purrr::map2(
    subcol_identifier$questions_ids,
    subcol_identifier$question_choices,
    ~ questions_list[[.x]]$choices[[.y]]$choiceText
  )
  
  subcol_df <- subcol_identifier |> 
    bind_cols(choices_text = as.character(final_subcol_choices_part_feed))
  
  if (length(purrr::discard(subsite_list_pivot$Subsite, is.na) != length(subcol_df$choices_text))) {
    setdiff(purrr::discard(subsite_list_pivot$Subsite, is.na), subcol_df$choices_text) |>
      print()
  }
  
  
}

get_missing_subcol_items(questions_list = subcols_part_feed_meta$questions)
get_missing_subcol_items(questions_list = subcols_educator_survey_meta$questions)


#### End of Subsite Checking ###


######################################### End of Site Checking #########################################

###################################### Facilitator/Coach Checking #########################################

facilitator_meta <- metadata(surveyID = "SV_djt8w6zgigaNq0C", questions = "facilitator")

facilitator_choices <- tibble(
  choices = purrr::map_chr(1:length(facilitator_meta$questions$QID1316652654$choices), ~ facilitator_meta$questions$QID1316652654$choices[[.x]]$choiceText),
  recode = as.numeric(purrr::map(1:length(facilitator_meta$questions$QID1316652654$choices), ~ facilitator_meta$questions$QID1316652654$choices[[.x]]$recode)),
)

### Run python script to get monday json ###
reticulate::source_python(here::here("data_scripts/monday.com/monday_facilitators.py"))

### Read in Monday JSON ###
initial_df <- jsonlite::fromJSON(here::here("data/monday/monday_facilitators.json"))

### Get as a data.frame ###
second_df <- initial_df$data$boards$items[[1]]$column_values |>
  as.data.frame()

### Get first column separately ###
first_column <- initial_df$data$boards$items |>
  as.data.frame() |>
  dplyr::select(name) |>
  dplyr::rename(Facilitator = name)

### Compose data.frame ###
final_df <- second_df |>
  dplyr::select(title, contains("text")) |>
  tidyr::pivot_longer(!title) |>
  tidyr::pivot_wider(names_from = "title", values_from = "value") |>
  dplyr::select(-name) |>
  dplyr::bind_cols(first_column) |>
  dplyr::relocate(Facilitator, .before = 1)

### Grab Facilitator Monday Board ###
fac_board <- final_df |>
  filter(`Facilitator Onboarding` == "Done" | `Coach Onboarding` == "Done" | `Coach/Facilitator` == "FTE") |>
  mutate(Facilitator = str_remove_all(Facilitator, " \\(Reardon\\)")) ### Remove the (Reardon) since Katherine Ryan is listed

setdiff(fac_board$Facilitator, facilitator_choices$choices) -> facilitator_check

if (length(facilitator_check) > 0) {
  print(paste0("The following facilitators need to be added to the reference survey: ", facilitator_check))
}

### Need to replace coach_2 with facilitator list as well ###

###################################### End of Facilitator/Coach Checking #########################################


###################################### Course Checking #########################################


course_list <- read_sheet(
  ss = "11jlo9UeWxZGwunhDb24hZBwAKc5b8ZKM9AYNWZaUyZY",
  sheet = "FY24 Official Course Names"
)

### ISSUE: NEED TO FIGURE OUT HOW TO GET DISPLAY LOGIC ###
course_feed_meta <- qualtRics::metadata(surveyID = "SV_djt8w6zgigaNq0C", questions = "course")

course_feed_choices <- tibble(
  choices = purrr::map_chr(1:length(course_feed_meta$questions$QID6$choices), ~ course_feed_meta$questions$QID6$choices[[.x]]$choiceText),
  recode = as.numeric(purrr::map(1:length(course_feed_meta$questions$QID6$choices), ~ course_feed_meta$questions$QID6$choices[[.x]]$recode)),
)

setdiff(purrr::discard(course_list$`Course Name Official`, is.na), course_feed_choices$choices) -> course_diff

if (length(course_diff) > 0) {
  print(paste0("The following courses need to be added: ", course_diff))
} # Note that Eureka Math² 4-5 Cycle of Inquiry: Instructional Routines is actually there, I'm not sure why it comes up here

###################################### Course Checking #########################################

### Attendance Checking ###
# reticulate::import("googleapiclient")
# ### Run python script to get google forms json ###
# reticulate::source_python(here::here("data_scripts/monday.com/fy24_attendance_fetch.py"))
#
# ### Read in Monday JSON ###
# initial_df <- jsonlite::fromJSON(here::here("data/monday/monday_facilitators.json"))
#
# ### Get as a data.frame ###
# second_df <- initial_df$data$boards$items[[1]]$column_values |>
#   as.data.frame()









#### For writing to Qualtrics ####
# qualtrics_api_request(
#   verb = "GET",
#   url = description_url,
#   query = list(format = "qsf"),
#   as = "text",
#   encoding = "UTF-8" # Prevents a warning from guessing encoding
# )
