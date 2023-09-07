knowledge_assessments_scored |> 
  filter(site %in% c("NY_D9", "NY_D11")) |>
  distinct(know_assess)
  group_by(prepost) |>
  count()


knowledge_assessments_scored |>
  filter(site %in% c("NY_D9", "NY_D11")) |>
  group_by(prepost) |>
  summarise(score = mean(percent)) |>
  mutate(score = 100 * score,
         prepost = str_to_title(prepost)) |>
  rename(Period = prepost,
         Score = score) |>
  gt::gt() |>
  gt::tab_header(title = gt::md("**District 9 and District 11 Knowledge Assessment Scores**")) |>
  gt::fmt_percent(columns = Score,
                  scale_values = F,
                  decimals = 0) |>
  gt::data_color(
    columns = c(Score),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>%
        as.character(),
      domain = NULL
    )
  ) |>
  # gt::data_color(
  #   columns = Score,
  #   colors = scales::col_bin(
  #     palette = c(
  #       TeachingLab::tl_palette(n = 8, color = "blue") |> magrittr::extract(c(4, 6, 8))
  #     ),
  #     domain = c(0, 100),
  #     bins = c(0, 39, 79, 100)
  #   )
  # ) |>
  TeachingLab::gt_theme_tl() |>
  gt::gtsave(here::here("Images/Hollyhock/knowledge_assessment_22_23_d9_d11.png"))
