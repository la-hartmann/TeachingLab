library(MondayR)
library(tidyverse)
library(googlesheets4)
library(reticulate)

# path_to_python <- paste0(here::here(), "/data_scripts/monday.com/env")
# use_virtualenv(path_to_python)
import("requests")
reticulate::source_python(here::here("data_scripts/monday.com/fy23_pipeline.py"))

initial_df <- jsonlite::fromJSON(here::here("data/monday/fy23_pipeline.json"))

second_df <- initial_df$data$boards$items[[1]]$column_values %>%
  as.data.frame()

first_column <- initial_df$data$boards$items |>
  as.data.frame() |>
  select(name) |>
  rename(`Open/Active` = name)

final_df <- second_df |>
  select(title, contains("text")) |>
  pivot_longer(!title) |>
  pivot_wider(names_from = "title", values_from = "value", values_fn = "list") |>
  select(-name) |>
  bind_cols(first_column) |>
  relocate(`Open/Active`, .before = 1) |>
  mutate(across(c("Passthru Amt",
                  "Probability (%)"), ~ as.numeric(.x)),
         across(c("Status date changed",
                  "Proposal sent date",
                  "Contract signed date"), ~ as.Date(as.character(.x)))#,
         # `Weighted Amt` = (`Projected Amt (no passthru)` * `Probability (%)`) / 100,
         # `Projected+Passthru` = `Projected Amt (no passthru)` + ifelse(!is.na(`Passthru Amt`), `Passthru Amt`, 0)
         )

### IF there are more than 26 columns get the range to write by concatenating LETTERS subtracted from 26
### This will ONLY work so long as it doesn't get past the "A(X)" range of google sheets letters which I don't
### foresee happening
if (ncol(final_df) <= 26) {
  end_letter <- LETTERS[ncol(final_df)]
} else {
  end_letter <- paste0("A", LETTERS[ncol(final_df) - 26])
}
end_number <- nrow(final_df) + 1

range_write(range = glue::glue("A1:{end_letter}{end_number}"),
            data = final_df,
            ss = "https://docs.google.com/spreadsheets/d/1CuHFUd_PKv0o0GGxHqUVaGcn4KddPp50d4Q36Wb_eVg/edit#gid=0",
            sheet = 1,
            col_names = TRUE,
            reformat = FALSE)
