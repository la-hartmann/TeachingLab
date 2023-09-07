devtools::load_all()
library(TeachingLab)
library(tidyverse)

fac_board <- TeachingLab::get_monday_board(board_id = 2208860812, first_col_name = "Facilitator") |>
  mutate(across(everything(), ~ as.character(.x)))

# fac_board |>
#   mutate(across(everything(), ~ as.character(.x))) |>
#   filter(str_detect(`Coach/Facilitator`, "Coach")) |>
#   pull(Facilitator) |>
#   sort() |>
#   clipr::write_clip()

grouped_fac_board <- fac_board |>
  filter(str_detect(`Coach/Facilitator`, "Coach")) |>
  group_by(Role) |>
  summarise(Facilitator) |>
  tibble::view()
