library(googlesheets4)
library(qualtRics)
library(tidyverse)

nm_diagnostic <- fetch_survey("SV_3a2OM4f9j85EuyO",
  include_display_order = FALSE,
  verbose = FALSE,
  start_date = as.Date("2023-08-01"),
  force_request = TRUE
)

nm_google_sheet <- read_sheet("17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU",
                              sheet = "Data Tracker SY23-24")

### If count of completed is needed
# (nm_google_sheet$Email |> tolower()) %in% (nm_diagnostic$email |> tolower()) |> sum()

nm_google_sheet_with_qualtrics <- nm_google_sheet |>
  dplyr::mutate(`Educator Survey Completed (Y)` = dplyr::if_else(tolower(Email) %in% tolower(nm_diagnostic$email), "Y", "N")#,
                # test = agrep(pattern = tolower(Email), x = tolower(nm_diagnostic$email))
                ) |>
  drop_na(`Participant Name`)

sheet_range <- paste0("A2:", "AB", nrow(nm_google_sheet_with_qualtrics) + 1)

nm_google_sheet_with_qualtrics |>
  range_write(ss = "17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU",
              sheet = "Data Tracker SY23-24",
              range = sheet_range,
              reformat = FALSE,
              col_names = FALSE)
