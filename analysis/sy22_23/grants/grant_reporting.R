library(qualtRics)
library(tidyverse)

educator_survey <- qualtRics::fetch_survey(
  surveyID = "SV_8vrKtPDtqQFbiBM",
  verbose = FALSE
)

## Grant goal for thing Shaye slacked me ##
educator_survey |>
  dplyr::select(RecordedDate, Finished, site,
                hqim_hqpl_practices_4, hqim_hqpl_practices_7) |>
  dplyr::filter(!is.na(hqim_hqpl_practices_4) | !is.na(hqim_hqpl_practices_7)) |>
  view()

ipg_forms <- qualtRics::fetch_survey(
  surveyID = "SV_0BSnkV9TVXK1hjw",
  verbose = FALSE
)

### Q14: CA1A in math IPG, Q51: CA1B in ELA IPG, Q68: AC1 in FSOT ###
ipg_forms |>
  dplyr::filter(Finished == TRUE & ipg_rubric == "K-12: Mathematics IPG") |>
  dplyr::select(Q14) |>
  mutate(across(everything(), ~ as.character(.x)),
         across(everything(), ~ na_if(.x, "NA"))) |>
  dplyr::summarise(score = TeachingLab::grade_ipg(Q14, type = "character"))

ipg_forms |>
  dplyr::filter(Finished == TRUE & ipg_rubric == "K-12: ELA/Literacy IPG (please use this tool for K-2 observations that are not focused on foundational skills)") |>
  dplyr::select(Q51) |>
  mutate(across(everything(), ~ as.character(.x)),
         across(everything(), ~ na_if(.x, "NA"))) |>
  dplyr::summarise(score = TeachingLab::grade_ipg(Q51, type = "character"))

ipg_forms |>
  dplyr::filter(Finished == TRUE & ipg_rubric == "Foundational Skills Observational Tool - FSOT") |>
  dplyr::select(Q68) |>
  mutate(across(everything(), ~ as.character(.x)),
         across(everything(), ~ na_if(.x, "NA"))) |>
  dplyr::summarise(score = TeachingLab::grade_ipg(Q68, type = "numeric"))


