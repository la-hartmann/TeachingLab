library(googlesheets4)
library(tidyverse)
library(TeachingLab)

end_coaching <- get_end_coaching(year = "22_23")

pos <- function(x) {
  100 * (sum(str_detect(x, "Often|often")) / (sum(!is.na(x))))
}

end_coaching |>
  select(contains("d11_math_coaching")) |>
  janitor::remove_empty("rows") |>
  TeachingLab::relabel_qualtrics_df() |>
  summarise(across(everything(), pos)) |>
  rename_with( ~ str_remove_all(.x, "Please rate the extent to which you have engaged in the following practices, reflecting on before\\.\\.\\. -")) |>
  pivot_longer(everything()) |>
  mutate(prepost = ifelse(str_detect(name, "BEFORE"), "Pre", "Post"),
         name = str_remove_all(name, "BEFORE participating in coaching - |AFTER participating in coaching - ")) |>
  group_by(name) |>
  mutate(improve = value - lag(value)) |>
  gt::gt() |>
  gt::fmt_percent(everything(), scale_values = F) |>
  gt::grand_summary_rows(columns = c(improve),
                         fns = list(
                           avg ~ mean(., na.rm = T)
                         ),
                         fmt = fmt_percent(., scale_values = F)) |>
  TeachingLab::gt_theme_tl() |>
  gt::gtsave("~/Downloads/before_after_d11_coaching.png")
