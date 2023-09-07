library(TeachingLab)
library(tidyverse)


educator_initials <- educator_survey |>
  select(initials, dob) |>
  mutate(id = paste0(tolower(initials), dob)) |>
  pull(id)

followup_initials <- followup_educator |>
  select(initials, dob) |>
  mutate(id = paste0(tolower(initials), dob)) |>
  pull(id)

intersect(followup_initials, educator_initials) |> length()
sort(followup_initials)
sort(educator_initials)
