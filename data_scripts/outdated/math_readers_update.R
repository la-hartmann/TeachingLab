library(googlesheets4)
library(qualtRics)

math_readers <- qualtRics::fetch_survey("SV_0qQmjyQrtk12nDU",
                                        include_display_order = FALSE,
                                        verbose = FALSE,
                                        force_request = TRUE)

data_to_write <- math_readers |>
  dplyr::filter(Finished == TRUE) |>
  dplyr::select(-c(1:17)) |>
  TeachingLab::relabel_qualtrics_df()

col_range <- paste0("A", LETTERS[ncol(data_to_write) - 26])

data_to_write |>
  googlesheets4::range_write(ss = "1hy6JHwZVgrXk-T2FeFyww1ByOJeqKX3lkrsNy5bz5_w",
                             sheet = "math_readers",
                             range = glue::glue("A2:{col_range}{nrow(data_to_write) + 1}"),
                             col_names = FALSE,
                             reformat = FALSE)
