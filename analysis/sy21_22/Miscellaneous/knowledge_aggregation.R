library(tidyverse)

knowledge_assessments <- read_rds(here::here("data/sy21_22/knowledge_assessments.rds"))

knowledge_assessments %>%
  group_by(prepost, know_assess) %>%
  summarise(percent = mean(percent)) %>%
  pivot_wider(names_from = "prepost", values_from = "percent") %>%
  gt::gt() %>%
  TeachingLab::gt_theme_tl()

knowledge_assessments %>%
  group_by(prepost) %>%
  summarise(percent = mean(percent)) %>%
  gt::gt() %>%
  TeachingLab::gt_theme_tl()


knowledge_assessments %>%
  filter(str_detect(site, "Rochester|11")) %>%
  # group_by(question, answer, know_assess) %>%
  # summarise(n = n()/length(unique(answer))) %>%
  # view()
  # mutate(site = ifelse(str_detect(site, "Rochester"), "Rochester", site),
  #        site = ifelse(str_detect(site, "11"), "District 11", site)) %>%
  group_by(prepost, know_assess) %>%
  summarise(percent = mean(percent)) %>%
  pivot_wider(names_from = "prepost", values_from = "percent") %>%
  gt::gt() %>%
  TeachingLab::gt_theme_tl()


ipg_forms <- TeachingLab::get_ipg_forms() %>%
  mutate(ela_math = ifelse(str_detect(`IPG Rubric`, "ELA|K-2"), "ELA", "Math")) %>%
  filter(`Date of Observation` >= as.Date("2021-07-31")) %>%
  filter(`Name of Site (Parish, District, Network)` %in% c("Lafayette Parish, LA", "Calcasieu Parish, LA")) %>%
  mutate(prepost = ifelse(`Date of Observation` >= as.Date("2022-01-01"), "post", "pre"))

different_actions <- ipg_forms %>%
  colnames() %>%
  as_tibble() %>%
  filter(str_detect(value, "CA")) %>%
  mutate(
    core_action = case_when(
      str_detect(value, "CA1|CA\\.1") ~ "ca1",
      str_detect(value, "CA2|CA\\.2") ~ "ca2",
      str_detect(value, "CA3|CA\\.3") ~ "ca3"
    ),
    value = as.character(value)
  )

ca1 <- different_actions %>%
  filter(core_action == "ca1") %>%
  pull(value)

ca2 <- different_actions %>%
  filter(core_action == "ca2") %>%
  pull(value)

ca3 <- different_actions %>%
  filter(core_action == "ca3") %>%
  pull(value)

ca1_percent <- ipg_forms %>%
  select(all_of(ca1), ela_math, prepost) %>%
  group_by(ela_math, prepost) %>%
  summarise(across(everything(), ~ grade_ipg(.x))) %>%
  pivot_longer(!c(ela_math, prepost)) %>%
  group_by(ela_math, prepost) %>%
  summarise(percent = mean(value, na.rm = T))

ca2_percent <- ipg_forms %>%
  select(all_of(ca2), ela_math, prepost) %>%
  group_by(ela_math, prepost) %>%
  summarise(across(everything(), ~ grade_ipg(.x, type = "numeric"))) %>%
  pivot_longer(!c(ela_math, prepost)) %>%
  group_by(ela_math, prepost) %>%
  summarise(percent = mean(value, na.rm = T))

ca3_percent <- ipg_forms %>%
  select(all_of(ca3), ela_math, prepost) %>%
  group_by(ela_math, prepost) %>%
  summarise(across(everything(), ~ grade_ipg(.x, type = "numeric"))) %>%
  pivot_longer(!c(ela_math, prepost)) %>%
  group_by(ela_math, prepost) %>%
  summarise(percent = mean(value, na.rm = T))

tibble::tibble(
  name = c(
    "Overall % positive IPG ratings",
    "% positive ratings Core Action 1",
    "% positive ratings Core Action 2",
    "% positive ratings Core Action 3"
  ),
  ELA = c(
    mean(c(ca1_percent$percent[1], ca2_percent$percent[1], ca3_percent$percent[1]), na.rm = T),
    ca1_percent$percent[1],
    ca2_percent$percent[1],
    ca3_percent$percent[1]
  ),
  Math = c(
    mean(c(ca1_percent$percent[2], ca2_percent$percent[2], ca3_percent$percent[2]), na.rm = T),
    ca1_percent$percent[2],
    ca2_percent$percent[2],
    ca3_percent$percent[2]
  )
) %>%
  gt::gt(rowname_col = "name") %>%
  gt::fmt_percent(c("ELA", "Math"),
                  scale_values = F,
                  decimals = 0
  ) %>%
  gt::data_color(
    columns = c("ELA", "Math"),
    colors = scales::col_bin(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  TeachingLab::gt_theme_tl() %>%
  gtsave(here::here("images/data_requests/teecee/ipg_scores_ela_math.png"))

#### Filter for Calcasieu Lafayette Fall is pre for IPG Forms ####

pre_know <- knowledge_assessments %>%
  filter(prepost == "pre") %>%
  group_by(id) %>%
  summarise(percent = mean(percent)) %>%
  pull(percent)

post_know <- knowledge_assessments %>%
  filter(prepost == "post") %>%
  group_by(id) %>%
  summarise(percent = mean(percent)) %>%
  pull(percent)

Tt <- t.test(x = pre_know, y = post_know, alternative = "two.sided",
             paired = F, var.equal = T)

effsize::cohen.d(pre_know, post_know)

effectsize::effectsize(Tt)

######### END OF EFFECT SIZE SCRIPT ############


all_knowledge_assessments %>%
  filter(str_detect(site, "11")) %>%
  mutate(ela_math = ifelse(str_detect(know_assess, "ela"), "ELA", "Math")) %>%
  group_by(prepost, ela_math) %>%
  summarise(percent = mean(percent),
            n = length(unique(id))) %>%
  gt::gt(groupname_col = "ela_math") %>%
  gt::tab_header(title = "District 11 Knowledge Assessments") %>%
  TeachingLab::gt_theme_tl() %>%
  gt::gtsave(here::here("images/knowledge_assessments/district11_metrics.png"))

all_knowledge_assessments %>%
  filter(str_detect(site, "9")) %>%
  mutate(ela_math = ifelse(str_detect(know_assess, "ela"), "ELA", "Math")) %>%
  group_by(prepost, ela_math) %>%
  summarise(percent = mean(percent),
            n = length(unique(id))) %>%
  gt::gt(groupname_col = "ela_math") %>%
  gt::tab_header(title = "District 9 Knowledge Assessments") %>%
  TeachingLab::gt_theme_tl() %>%
  gt::gtsave(here::here("images/knowledge_assessments/district9_metrics.png"))


