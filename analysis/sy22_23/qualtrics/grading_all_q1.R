library(qualtRics)
library(tidyverse)

### Get Educator Survey ###
educator_survey <- fetch_survey(surveyID = "SV_8vrKtPDtqQFbiBM",
                                force_request = TRUE)

### Function for scoring mindsets ###
pos_neg <- function(x, which) {
  
  x <- x[!is.na(x)]
  x <- x[!is.null(x)]
  x <- x[!str_detect(x, "Not Observed|Not observed|NULL|NA")]
  
  if (which == "pos") {
    x <- 100 * (sum(stringr::str_detect(x, "4|5"), na.rm = T)) /
      (sum(!stringr::str_detect(x, "4|5"), na.rm = T) + sum(str_detect(x, "4|5"), na.rm = T))
  } else if (which == "neg") {
    x <- 100 * (sum(stringr::str_detect(x, "1|2"), na.rm = T)) /
      (sum(!stringr::str_detect(x, "1|2"), na.rm = T) + sum(str_detect(x, "1|2"), na.rm = T))
  } else {
    print("Unknown which entry")
  }
  
  x
  
}

### For checking names ###
# actual_names <- map_chr(educator_survey, ~ attr(.x, "label"))
# view(actual_names)
### Not always necessary ###

### Filter data by date then grade mindsets by positive or negative ###
mindsets_for_scoring <- educator_survey |>
  dplyr::filter(RecordedDate <= as.Date("2022-09-30")) |>
  dplyr::select(contains("Mindsets Ts")) |>
  relabel_qualtrics_df() |>
  dplyr::select(!contains("Display")) |>
  mutate(across(everything(), ~ as.character(.x))) |>
  dplyr::rename_with( ~ str_remove_all(.x, "To what extent do you agree or disagree with the following statements\\? - "))

positive_negative_order <- c("neg", "neg",
                             "pos",
                             "neg", "neg", "neg",
                             "pos", "pos", "pos", "pos",
                             "neg", "neg", "neg", "neg", "neg")

map2_df(mindsets_for_scoring, positive_negative_order,
         ~ pos_neg(.x, .y)) |>
  pivot_longer(everything()) |>
  summarise(value = mean(value))

### Function for grading CRSE ###
pos_neg_crse <- function(x, which) {
  
  x <- x[!is.na(x)]
  x <- x[!is.null(x)]
  x <- x[!str_detect(x, "Not Observed|Not observed|NULL|NA")]
  
  if (which == "10_scale") {
    x <- 100 * (sum(stringr::str_detect(x, "8|9|10"), na.rm = T)) /
      (sum(!stringr::str_detect(x, "8|9|10"), na.rm = T) + sum(str_detect(x, "8|9|10"), na.rm = T))
  } else if (which == "normal_scale") {
    x <- 100 * (sum(stringr::str_detect(x, "4|5"), na.rm = T)) /
      (sum(!stringr::str_detect(x, "4|5"), na.rm = T) + sum(str_detect(x, "4|5"), na.rm = T))
  }
  
  x
  
}

crse_for_scoring <- educator_survey |>
  dplyr::filter(RecordedDate <= as.Date("2022-10-11")) |>
  dplyr::select(contains("Ts CRSE")) |>
  dplyr::select(!contains("Non Ts")) |>
  dplyr::mutate(dplyr::across(dplyr::contains("before"), ~ stringr::str_replace_all(.x, c("4" = "1",
                                                                                          "5" = "1",
                                                                                          "9" = "5",
                                                                                          "10" = "5",
                                                                                          "8" = "5")))) |>
  dplyr::mutate(`Ts CRSE-after 7/30_1` = dplyr::coalesce(`Ts CRSE-before 7/30_1`, `Ts CRSE-after 7/30_1`),
                `Ts CRSE-after 7/30_2` = dplyr::coalesce(`Ts CRSE-before 7/30_2`, `Ts CRSE-after 7/30_2`),
                `Ts CRSE-after 7/30_3` = dplyr::coalesce(`Ts CRSE-before 7/30_3`, `Ts CRSE-after 7/30_3`),
                `Ts CRSE-after 7/30_4` = dplyr::coalesce(`Ts CRSE-before 7/30_4`, `Ts CRSE-after 7/30_4`)) |>
  dplyr::select(!contains("before"))

old_new_scale <- c(rep("normal_scale", 4))

map2_df(crse_for_scoring, old_new_scale,
        ~ pos_neg_crse(.x, .y)) |>
  pivot_longer(everything()) |>
  summarise(value = mean(value))


### Content Area Scoring ###
### Insert Knowledge Assessment Grading here ###


### IPG Forms Grading ###
ipg_forms <- qualtRics::fetch_survey(surveyID = "SV_0BSnkV9TVXK1hjw", 
                                     verbose = TRUE,
                                     force = TRUE)

score_ipg_forms <- function(x, question) {
  
  x <- x[!is.na(x)]
  x <- x[!is.null(x)]
  x <- x[!str_detect(x, "Not Observed|Not observed|NULL|NA")]
  
  if (question == "yes_no") {
    x <- 100 * (sum(stringr::str_detect(x, "Yes"), na.rm = T)) /
      (sum(!stringr::str_detect(x, "Yes"), na.rm = T) + sum(str_detect(x, "Yes"), na.rm = T))
  } else if (question == "1_4") {
    x <- 100 * (sum(stringr::str_detect(x, "3|4"), na.rm = T)) /
      (sum(!stringr::str_detect(x, "3|4"), na.rm = T) + sum(str_detect(x, "3|4"), na.rm = T))
  }
  
  x
}

ipg_forms_for_scoring <- ipg_forms |>
  dplyr::filter(RecordedDate <= as.Date("2022-10-11")) |>
  dplyr::select(Q14, Q15, Q16, 
                Q17_1, Q17_2, Q17_3, 
                Q19_1, Q19_2, Q19_3, 
                Q21, Q22, Q23, Q24, 
                Q26, Q27, Q28, Q29, Q30,
                Q49_1, Q49_2, Q49_3,
                Q50, Q51, Q52,
                Q54, Q55, Q56, Q57,
                Q59, Q60, Q61, Q62, Q63, Q64,
                Q68, Q69_1, Q69_2, Q69_3, Q69_4,
                Q70, 
                Q72, Q73, Q74, Q75,
                Q77, Q78, Q79, Q80,
                Q82, Q83) |>
  TeachingLab::relabel_qualtrics_df() |>
  view()

ipg_question <- c(rep("yes_no", 3), )

map2_df(ipg_forms_for_scoring, ipg_question,
        ~ score_ipg_forms(.x, .y)) |>
  view()


### Knowledge Assessments Grading ###
surveys <- qualtRics::all_surveys()

tibble::view(surveys)


