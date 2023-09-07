library(tidyverse)

graded |> 
  filter(!is.na(Legible) & Site == "NY_D11") |> 
  select(`Submitted Grade/s`) |>
  separate_rows(`Submitted Grade/s`) |>
  filter(`Submitted Grade/s` != "NA") |>
  group_by(`Submitted Grade/s`) |>
  count(sort = T)
