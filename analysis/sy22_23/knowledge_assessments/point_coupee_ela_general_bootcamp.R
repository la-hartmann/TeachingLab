library(gt)
library(qualtRics)
library(tidyverse)

surveys <- qualtRics::all_surveys()

tibble::view(surveys)

ela_general_bootcamp <- qualtRics::fetch_survey("SV_d5nw8tm0NF56kU6",
                                                force_request = T)
math_bootcamp <- qualtRics::fetch_survey("SV_37uHoiF60EUKATQ",
                                         force_request = T)
math_raise <- qualtRics::fetch_survey("SV_9YsBPlM5jZ30Dbg",
                                         force_request = T)

math_bootcamp |>
  filter(Finished == TRUE) |>
  group_by(Site) |>
  mutate(score = round(100 * (SC0/9), 2),
         prepost = ifelse(EndDate >= mean(EndDate), "Post", "Pre")) |>
  ungroup() |>
  select(score, prepost, Site) |>
  group_by(prepost, Site) |>
  summarise(score = mean(score, na.rm = T)) |>
  ungroup() |>
  arrange(desc(prepost)) |>
  pivot_wider(names_from = Site, values_from = score) |>
  gt::gt() |>
  gt::tab_header(title = gt::md("**Math: Bootcamp - General Scores**")) |>
  gt::fmt_percent(where(is.numeric),
                  scale_values = F) |>
  TeachingLab::gt_theme_tl()

ela_general_bootcamp |>
  filter(Finished == TRUE & Site == "LA_Pointe Coupee Parish") |>
  # group_by(Site) |>
  mutate(score = round(100 * (SC0/8), 2),
         prepost = ifelse(EndDate >= mean(EndDate), "Post", "Pre")) |>
  ungroup() |>
  select(score, prepost) |>
  group_by(prepost) |>
  summarise(score = mean(score, na.rm = T)) |>
  ungroup() |>
  arrange(desc(prepost)) |>
  view()
  pivot_wider(names_from = prepost, values_from = score) |>
  gt::gt() |>
  gt::tab_header(title = gt::md("**ELA: Bootcamp - General Scores**")) |>
  gt::fmt_percent(where(is.numeric),
                  scale_values = F) |>
  TeachingLab::gt_theme_tl() #|>
  # gtsave(here::here("images/pointe_coupee_ela_bootcamp.png"))

check <- ela_general_bootcamp |>
  filter(Site == "LA_Pointe Coupee Parish" & Finished == TRUE) |>
  mutate(score = round(100 * (SC0/8), 2),
         prepost = ifelse(EndDate >= mean(EndDate), "Post", "Pre")) |>
  select(score, prepost, contains("Q")) |>
  group_by(prepost) |>
  summarise(score = mean(score, na.rm = T)) |>
  ungroup() |>
  arrange(desc(prepost)) |>
  pivot_wider(names_from = prepost, values_from = score) |>
  gt::gt() |>
  gt::tab_header(title = gt::md("**Pointe Coupee ELA: Bootcamp - General Scores**")) |>
  gt::fmt_percent(everything(),
                  scale_values = F) |>
  TeachingLab::gt_theme_tl() |>
  gtsave(here::here("images/pointe_coupee_ela_question_breakdown_bootcamp.png"))

math_raise |>
  filter(Finished == TRUE) |>
  mutate(score = round(100 * (SC0/11), 2),
         prepost = ifelse(EndDate >= mean(EndDate), "Post", "Pre")) |>
  select(score, prepost, contains("Q")) |>
  group_by(prepost) |>
  summarise(score = mean(score, na.rm = T)) |>
  ungroup() |>
  arrange(desc(prepost)) |>
  pivot_wider(names_from = prepost, values_from = score) |>
  gt::gt() |>
  gt::tab_header(title = gt::md("**Pointe Coupee ELA: Bootcamp - General Scores**")) |>
  gt::fmt_percent(everything(),
                  scale_values = F) |>
  TeachingLab::gt_theme_tl()
  
  
knowledge_assessments <- tibble::tibble(
  assessment = c("Math: Bootcamp", "Math: Bootcamp", "ELA Bootcamp - General", "ELA Bootcamp - General", "Math: RAISE", "Math: RAISE"),
  site = c("US_Open Enrollment", "US_Open Enrollment", "LA_Pointe Coupee Parish", "LA_Pointe Coupee Parish", "TX_RAISE Rice University", "TX_RAISE Rice University"),
  prepost = factor(c("Pre", "Post", "Pre", "Post", "Pre", "Post"), levels = c("Pre", "Post")),
  percent = c(61.11, 83.34, 55.20, 67.51, 58.68, 73.21)
)

library(ggtext)

knowledge_assessments |>
  ggplot(aes(x = site, y = percent)) +
  geom_col(position = position_dodge2(),
           aes(fill = prepost)) +
  geom_text(aes(label = paste0(percent, "%")), position = position_dodge2(width = 1), vjust = -1,
            family = "Calibri Bold",
            fontface = "bold",
            size = 8) +
  geom_text(aes(label = ifelse(prepost == "Pre", assessment, ""),
                y = -2,
                size = 6),
            fontface = "bold") +
  labs(x = "", y = "",
       title = "<b>% Correct on Knowledge Assessments<br>Before and <span style = 'color:#04abeb;'>After</span> Teaching Lab PL</b>") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(-5, 100)) +
  scale_fill_manual(values = c("Pre" = "#040404", "Post" = "#04abeb")) +
  TeachingLab::theme_tl(plot_title_family = "Calibri Bold") +
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        plot.title = element_markdown(face = "bold", size = 24))


ggsave(here::here("images/newsletter/knowledge_assessment_highlights.png"),
       bg = "white",
       height = 10,
       width = 12)
