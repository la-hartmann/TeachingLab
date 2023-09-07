library(gt)
library(qualtRics)
library(tidyverse)
library(TeachingLab)

eic_student_survey <- fetch_survey("SV_8f9l21n6ML58WFM")

rochester_race_school_count <- eic_student_survey |>
  mutate(`Rochester Schools` = as.character(`Rochester Schools`),
         Race = as.character(Race)) |>
  drop_na(`Rochester Schools`) |>
  group_by(`Rochester Schools`, Race) |>
  count(sort = T) |>
  dplyr::ungroup()

rochester_table <- rochester_race_school_count |>
  gt::gt() |>
  gt::data_color(
    columns = c(n),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>%
        as.character(),
      domain = NULL
    )
  ) |>
  TeachingLab::gt_theme_tl()

rochester_table

rochester_table |>
  gt::gtsave("~/Downloads/Rochester_student_survey_count.png")
