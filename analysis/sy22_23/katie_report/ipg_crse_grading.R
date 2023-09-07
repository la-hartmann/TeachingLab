library(qualtRics)
library(tidyverse)
library(TeachingLab)

### IPG Forms Read-In ###
ipg_forms <- qualtRics::fetch_survey(
  surveyID = "SV_0BSnkV9TVXK1hjw",
  verbose = FALSE
)

### Educator Survey Read-In ###
educator_survey <- qualtRics::fetch_survey(
  surveyID = "SV_8vrKtPDtqQFbiBM",
  verbose = FALSE
)

#CA|TD|AD|SP
ipg_grading <- ipg_forms |>
  relabel_qualtrics_df(switch = TRUE) |>
  dplyr::select(tidyselect::contains("CA1", ignore.case = FALSE),
                tidyselect::contains("TD", ignore.case = FALSE),
                tidyselect::contains("SP", ignore.case = FALSE),
                tidyselect::contains("AD1", ignore.case = FALSE),
                tidyselect::contains("AD2", ignore.case = FALSE)) |>
  dplyr::mutate(across(everything(), ~ as.character(.x)),
                across(everything(), ~ na_if(.x, "NA"))) |>
  janitor::remove_empty("rows") |>
  pivot_longer(everything()) |>
  drop_na(value) |>
  mutate(category = case_when(str_detect(name, "SP") ~ "SP",
                              str_detect(name, "AD") ~ "AD",
                              str_detect(name, "TD") ~ "TD",
                              str_detect(name, "CA") ~ "CA"),
         pos_neg = case_when(category == "SP" & str_detect(value, "3|4") ~ "positive",
                             category == "SP" & !str_detect(value, "3|4") ~ "negative",
                             category == "AD" & str_detect(value, "2|3") ~ "positive",
                             category == "AD" & !str_detect(value, "2|3") ~ "negative",
                             category == "TD" & str_detect(value, "3|4") ~ "positive",
                             category == "TD" & !str_detect(value, "3|4") ~ "negative",
                             category == "CA" & str_detect(value, "Yes") ~ "positive",
                             category == "CA" & !str_detect(value, "Yes") ~ "negative",
                             value == "Not observed" ~ NA_character_)) |>
  view()
