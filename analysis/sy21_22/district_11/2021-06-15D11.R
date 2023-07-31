teaching_df <- read_rds(here("Data/Dashboard Data/dashboard_data.rds"))


teaching_df %>%
  filter(str_detect(`District, Parish, Or Network`, "District 11|Robin") & `Date for the session` < "2021-07-01" & `Date for the session` > "2020-06-30")  %>%
  summarise(TeachingLab::calc_nps(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?`),
            n())

d11_df <- teaching_df %>%
  filter(str_detect(`District, Parish, Or Network`, "District 11|Robin"))

d11_df_2021 <- teaching_df %>%
  filter(str_detect(`District, Parish, Or Network`, "District 11|Robin") & `Date for the session` < "2021-07-01" & `Date for the session` > "2020-06-30")

index <- tibble(
  question = c("% Satisfied With The Overall Quality Of Today's Professional Learning Session", 
               "S/He Facilitated The Content Clearly", 
               "How Likely Are You To Apply This Learning To Your Practice In The Next 4-6 Weeks?"),
  coding = list(c("Agree", "Strongly agree"))
)
map2_df(index$question, index$coding, ~ TeachingLab::score_question(teaching_df, .x, .y, na_type = "NR")) %>%
  arrange(desc(percent)) %>%
  gt::gt() %>%
  gt::cols_label(
    percent = md("**Percent**"),
    n = md("**N**"),
    question = md("**Question**")
  ) %>%
  gt::tab_header("% That Agree or Strongly Agree") %>%
  gt::fmt_percent(
    columns = vars(percent),
    decimals = 1, 
    scale_values = F
  ) %>%
  gt::data_color(
    columns = vars(percent),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  TeachingLab::gt_theme_tl() %>%
  gt::gtsave(here("Images/PercentAgree.png"))

map2_df(index$question, index$coding, ~ TeachingLab::score_question(teaching_df, .x, .y, na_type = "NR")) %>%
  gt::gt() %>%
  gt::tab_header("Only for SY 2020-2021") %>%
  gt::gtsave(here("Images/D11/PercentAgree2021.png"))
