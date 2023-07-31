library(dplyr)
library(googlesheets4)
library(purrr)
library(qualtRics)

ipg_forms <- qualtRics::fetch_survey(surveyID = "SV_0BSnkV9TVXK1hjw", 
                                     verbose = TRUE,
                                     force = TRUE)

selected_ipg_forms <- ipg_forms |>
  dplyr::filter(Finished == TRUE) |>
  dplyr::select(-c(1:7, 9:17)) |>
  TeachingLab::relabel_qualtrics_df()

selected_ipg_forms |>
  googlesheets4::write_sheet(ss = "1mRZThJuslwVGXyWNY2Z-gAgh0FZKaJM1LIHRwhRvMg8",
                             sheet = "ipg_forms")
