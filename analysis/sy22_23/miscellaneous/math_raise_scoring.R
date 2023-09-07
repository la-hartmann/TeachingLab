library(qualtRics)
library(tidyverse)
library(TeachingLab)

surveys <- all_surveys()

view(surveys)

math_raise <- fetch_survey(surveyID = "SV_9YsBPlM5jZ30Dbg", 
                               verbose = TRUE)

view(math_raise)

math_raise_n <- math_raise |>
  mutate(id = paste0(tolower(Initials), DOB),
         score = SC0/11) |>
  group_by(id) |>
  mutate(id_count = n(),
         maxdate = max(EndDate)) |>
  ungroup() |>
  mutate(prepost = ifelse(id_count > 1 & maxdate == EndDate,
                          "Post",
                          "Pre")) |>
  # select(id, id_count, maxdate, score, prepost) |>
  filter(id_count != 3 & id %!in% c("bql0315", "elm1118", "jms0915", "mai1223", "pxt0204", "riz0819")) |>
  group_by(prepost) |>
  count()

math_raise |>
  mutate(id = paste0(tolower(Initials), DOB),
         score = SC0/11) |>
  group_by(id) |>
  mutate(id_count = n(),
         maxdate = max(EndDate)) |>
  ungroup() |>
  mutate(prepost = ifelse(id_count > 1 & maxdate == EndDate,
                          "Post",
                          "Pre")) |>
  # select(id, id_count, maxdate, score, prepost) |>
  filter(id_count != 3 & id %!in% c("bql0315", "elm1118", "jms0915", "mai1223", "pxt0204", "riz0819")) |>
  group_by(prepost) |>
  summarise(score = 100 * mean(score)) |>
  left_join(math_raise_n) |>
  rename(` ` = prepost, Percent = score) |>
  arrange(desc(` `)) |>
  gt::gt() |>
  gt::tab_header("Math Raise Percent Correct") |>
  gt::fmt_percent(Percent,
                  scale_values = F) |>
  gt::data_color(
    columns = c(Percent),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>%
        as.character(),
      domain = NULL
    )
  ) |>
  TeachingLab::gt_theme_tl() |>
  gt::gtsave(here::here("images/math_raise_score.png"))

math_raise |>
  filter(EndDate > as.Date("2022-07-24")) |>
  mutate(score = SC0/11,
         prepost = ifelse(EndDate <= as.Date("2022-07-26"),
                          "Pre",
                          "Post")) |>
  group_by(prepost) |>
  summarise(score = 100 * mean(score)) |>
  rename(` ` = prepost, Percent = score) |>
  gt::gt() |>
  gt::tab_header("Math Raise Percent Correct") |>
  gt::fmt_percent(Percent,
                  scale_values = F) |>
  gt::data_color(
    columns = c(Percent),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>%
        as.character(),
      domain = NULL
    )
  ) |>
  TeachingLab::gt_theme_tl() |>
  gt::gtsave(here::here("images/math_raise_score_filtered.png"))
