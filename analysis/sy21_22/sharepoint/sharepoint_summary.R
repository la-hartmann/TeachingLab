library(readxl)
library(tidyverse)
library(gt)
library(TeachingLab)

round_score <- function(x) {
  round(mean(x, na.rm = T), 2)
}

files <- list.files("data/sharepoint", full.names = T)

math_results <- purrr::map_dfr(1:3, ~ read_xlsx(files[1], sheet = .x, skip = 4))
math_teachers <- purrr::map_dfr(1:3, ~ read_xlsx(files[2], sheet = .x, skip = 4))
algebra_results <- purrr::map_dfr(1:3, ~ read_xlsx(files[3], sheet = .x, skip = 4))

math_n <- math_results %>%
  mutate(GENDER = ifelse(FEMALE == "Y",
                         "Female",
                         "Male")) %>%
  group_by(YEAR, RACE, GENDER) %>%
  count(sort = T)

math_results_proficiency <- math_results %>%
  mutate(GENDER = ifelse(FEMALE == "Y",
                         "Female",
                         "Male")) %>%
  group_by(YEAR, RACE, GENDER, PROFICIENCY_SCORE) %>%
  count(sort = T) %>%
  ungroup() %>%
  group_by(RACE, YEAR, GENDER) %>%
  mutate(percent = round(100*n/sum(n), 2)) %>%
  select(-n) %>%
  pivot_wider(names_from = PROFICIENCY_SCORE, values_from = percent) %>%
  relocate(`2`, .after = `1`) %>%
  select(`1`, `2`, `3`, `4`, `5`)

math_results_plot <- math_results %>%
  mutate(GENDER = ifelse(FEMALE == "Y",
                         "Female",
                         "Male")) %>%
  group_by(YEAR, RACE, GENDER) %>%
  summarise(across(c(SCALE_SCORE), ~ round_score(.x))) %>%
  left_join(math_results_proficiency) %>%
  left_join(math_n) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0))) %>%
  gt::gt() %>%
  cols_label(
    `1` = "% 1",
    `2` = "% 2",
    `3` = "% 3",
    `4` = "% 4",
    `5` = "% 5"
  ) %>%
  tab_header(title = "Scaled Scores and Percentages in Proficiency by Race, Gender, and Year") %>%
  fmt_percent(columns = c(`1`, `2`, `3`, `4`, `5`),
              scale_values = F,
              decimals = 2) %>%
  data_color(
    columns = c(SCALE_SCORE, `1`, `2`, `3`, `4`, `5`),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  TeachingLab::gt_theme_tl()

gtsave(math_results_plot, here::here("images/sharepoint/math_results.png"))


math_teachers_n <- math_teachers %>%
  group_by(YEAR, RACE, GENDER) %>%
  count(sort = T)

math_teachers %>%
  group_by(YEAR, RACE, GENDER) %>%
  summarise(across(c(YEARS_EXP, YEARS_TEACH_EXP), ~ round(mean(.x, na.rm = T), 2))) %>%
  left_join(math_teachers_n) %>%
  gt::gt() %>%
  tab_header("Years of Teaching Experience by Gender, Race, and Year") %>%
  data_color(
    columns = c(YEARS_EXP, YEARS_TEACH_EXP),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  TeachingLab::gt_theme_tl() %>%
  gtsave(here::here("images/sharepoint/teachers_experience.png"))

algebra_results <- algebra_results %>%
  mutate(YEAR = lubridate::year(TEST_DATE))

algebra_n <- algebra_results %>%
  mutate(GENDER = ifelse(FEMALE == "Y",
                         "Female",
                         "Male")) %>%
  group_by(YEAR, RACE, GENDER) %>%
  count(sort = T)

algebra_results_proficiency <- algebra_results %>%
  mutate(GENDER = ifelse(FEMALE == "Y",
                         "Female",
                         "Male")) %>%
  group_by(YEAR, RACE, GENDER, PROFICIENCY_SCORE) %>%
  count(sort = T) %>%
  ungroup() %>%
  group_by(RACE, YEAR, GENDER) %>%
  mutate(percent = round(100*n/sum(n), 2)) %>%
  select(-n) %>%
  pivot_wider(names_from = PROFICIENCY_SCORE, values_from = percent) %>%
  relocate(`2`, .after = `1`) %>%
  select(`1`, `2`, `3`, `4`, `5`)

algebra_results_plot <- algebra_results %>%
  mutate(GENDER = ifelse(FEMALE == "Y",
                         "Female",
                         "Male")) %>%
  group_by(YEAR, RACE, GENDER) %>%
  summarise(across(c(SCALE_SCORE), ~ round_score(.x))) %>%
  left_join(algebra_results_proficiency) %>%
  left_join(algebra_n) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0))) %>%
  gt::gt() %>%
  cols_label(
    `1` = "% 1",
    `2` = "% 2",
    `3` = "% 3",
    `4` = "% 4",
    `5` = "% 5"
  ) %>%
  tab_header(title = "Scaled Scores and Percentages in Proficiency by Race, Gender, and Year") %>%
  fmt_percent(columns = c(`1`, `2`, `3`, `4`, `5`),
              scale_values = F,
              decimals = 2) %>%
  data_color(
    columns = c(SCALE_SCORE, `1`, `2`, `3`, `4`, `5`),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  TeachingLab::gt_theme_tl()

gtsave(math_results_plot, here::here("images/sharepoint/algebra_results.png"))
