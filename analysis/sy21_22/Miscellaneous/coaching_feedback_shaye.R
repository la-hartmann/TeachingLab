library(TeachingLab)

coaching_feedback <- get_coaching_feedback()

coaching_feedback |>
  filter(str_detect(Site, "District 9")) |>
  select(Additional_feedback, Gone_well, Could_be_better) |>
  mutate(across(everything(), ~ na_if(.x, "Na"))) |>
  rename(`What additional feedback do you have about their coaching skills, if any?` =
           `Additional_feedback`,
         `What has gone well in your coaching sessions?` = Gone_well,
         `What could be better about your coaching sessions?` = Could_be_better) |>
  TeachingLab::quote_viz(text_col = c("What additional feedback do you have about their coaching skills, if any?",
                                      "What has gone well in your coaching sessions?",
                                      "What could be better about your coaching sessions?")) |>
  # gt::gt() |>
  # gt::fmt_missing(everything(),
  #                 " ") |>
  # gt_theme_tl() |>
  gtsave(here::here("images/coaching_feedback/qualitative-d9-2022-04-27.png"),
         vwidth = 1400,
         vheight = 1300)
