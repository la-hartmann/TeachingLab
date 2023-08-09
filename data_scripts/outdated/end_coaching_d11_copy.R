library(googlesheets4)
library(tidyverse)
library(TeachingLab)

end_coaching <- TeachingLab::get_end_coaching(update = TRUE)

coaching_responses_d11 <- end_coaching |>
  filter(site == "NY_D11") |>
  select(district11, contains("d11_math_coaching")) 

final_coaching_responses_d11 <- coaching_responses_d11 |>
  TeachingLab::relabel_qualtrics_df() |>
  filter(!is.na(coaching_responses_d11[,4]))

num_cols <- LETTERS[ncol(final_coaching_responses_d11)]
num_rows <- nrow(final_coaching_responses_d11) + 1

final_coaching_responses_d11 |>
  range_write(col_names = FALSE,
              reformat = FALSE,
              ss = "10yqmNypa1w99mWVSagYLT9SjQe4NOo1GpPA5soqsRAs",
              sheet = "coaching_responses_d11",
              range = glue::glue("A2:{num_cols}{num_rows}"))
