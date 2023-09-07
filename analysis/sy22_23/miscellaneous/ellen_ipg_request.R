library(googlesheets4)
library(tidyverse)

original_col_names <- ipg_forms |>
  select(c(Q68:Q84)) |>
  colnames()

col_renames <- ipg_forms |>
  select(c(Q68:Q84)) |>
  map_chr( ~ attr(.x, "label"))

col_renames[1:3]

ipg_forms_selected <- ipg_forms |>
  filter(Site %in% c("NY_D9", "NY_D11") & `Content area` == "K-2 Early Literacy (Foundational Skills)") |>
  mutate(across(everything(), ~ as.character(.x)),
         across(everything(), ~ na_if(.x, "NA"))) |>
  janitor::remove_empty("cols") |>
  select(-c(1:13), -c(`Q47`:`Q65`))

ipg_forms_selected |>
  rename_with( ~ str_replace_all(.x, col_renames)) |>
  write_sheet()
