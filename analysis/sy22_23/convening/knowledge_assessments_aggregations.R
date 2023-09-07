library(TeachingLab)
library(dplyr)

knowledge_assessments <- get_knowledge_assessments()

id_in_pre <- knowledge_assessments |>
  filter(prepost == "pre") |>
  pull(id)

id_in_post <- knowledge_assessments |>
  filter(prepost == "post") |>
  pull(id)

knowledge_assessments |>
  filter(id %in% id_in_pre & id %in% id_in_post) |>
  group_by(prepost, site) |>
  summarise(percent = mean(percent)) |>
  pivot_wider(names_from = prepost, values_from = percent) |>
  mutate(change = post - pre) |>
  arrange(desc(change)) |>
  head(n = 5) |>
  gt::gt() |>
  TeachingLab::gt_theme_tl() |>
  gt::gtsave(here::here("images/Convening/knowledge_assessments_matched_standouts.png"))

knowledge_assessments |>
  filter(id %in% id_in_pre & id %in% id_in_post) |>
  group_by(prepost, know_assess) |>
  summarise(percent = mean(percent)) |>
  pivot_wider(names_from = prepost, values_from = percent) |>
  mutate(change = post - pre) |>
  arrange(desc(change)) |>
  head(n = 5) |>
  gt::gt() |>
  TeachingLab::gt_theme_tl() |>
  gt::gtsave(here::here("images/Convening/knowledge_assessments_matched_construct_standouts.png"))

