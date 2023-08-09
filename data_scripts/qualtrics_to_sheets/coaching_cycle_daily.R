library(googlesheets4)
library(qualtRics)

coaching_cycle <- qualtRics::fetch_survey(surveyID = "SV_6yDnUlKkIMogr78",
                                          force_request = FALSE,
                                          verbose = TRUE,
                                          include_display_order = FALSE)

written_data <- read_sheet("1mpR-1PGohWflTjqpAO4nrc-Bdv04vzwK2-S7HuBIHYA",
                           sheet = "selected_coaching_cycle_report")
  
write_data <- coaching_cycle |>
  dplyr::filter(Finished == TRUE & !`Coachee name` %in% c("TEST", "test", "tst", "TST") & RecordedDate > max(written_data$`RecordedDate`)) |>
  dplyr::select(-c(1:7, 9:17)) 

if (nrow(write_data) >= 1) {
  write_data |>
    googlesheets4::write_sheet(ss = "1mpR-1PGohWflTjqpAO4nrc-Bdv04vzwK2-S7HuBIHYA",
                               sheet = "selected_coaching_cycle_report")
}

