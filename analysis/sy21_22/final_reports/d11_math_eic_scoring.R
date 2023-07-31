library(TeachingLab)
library(tidyverse)



d11_student_survey |>
  select(14:17) |>
  summarise(across(everything(), ~ negative_agree(.x))) |>
  view()
