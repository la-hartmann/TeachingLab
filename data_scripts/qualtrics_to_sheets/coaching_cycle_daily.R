library(googlesheets4)
library(qualtRics)

coaching_cycle <- qualtRics::fetch_survey(surveyID = "SV_6yDnUlKkIMogr78",
                                          force_request = TRUE,
                                          verbose = TRUE,
                                          include_display_order = FALSE)
  
coaching_cycle |>
  dplyr::filter(Finished == TRUE & !`Coachee name` %in% c("TEST", "test", "tst", "TST")) |>
  dplyr::select(-c(1:7, 9:17)) |>
  googlesheets4::write_sheet(ss = "1mpR-1PGohWflTjqpAO4nrc-Bdv04vzwK2-S7HuBIHYA",
                             sheet = "selected_coaching_cycle_report")
