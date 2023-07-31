library(googlesheets4)
library(tidyverse)
library(TeachingLab)
library(ggtext)
library(gt)

data <- read_sheet("https://docs.google.com/spreadsheets/d/1ZZnizhPVjL8BBenwAeSKcTU1GYKpZYdEUe_-95V5Ej0/edit#gid=239657167",
                   sheet = "Scoring2",
                   col_names = c("Link", "ID", "Score", "Name", "Prepost", "Grade", "Curriculum"))

data_clean <- data %>% 
  filter(!str_detect(Score, "No Response|NULL|Not|No response")) %>%
  # mutate(Score = as.numeric(as.character(Score))) %>%
  # drop_na(Score) %>%
  select(-1) %>%
  mutate(Name = unlist(Name))

### TABLE 1 N SIZE CREATION

gt1 <- data_clean %>%
  group_by(Curriculum, Prepost) %>%
  summarise(n = n()) %>%
  mutate(Prepost = str_replace_all(Prepost, c("Pre" = "Fall", "Post" = "Spring"))) %>%
  pivot_wider(names_from = "Prepost", values_from = "n", names_sort = T) %>%
  ungroup()

gt1 %>%
  gt(rowname_col = "Curriculum") %>%
  tab_header(title = "\U1F605    Table 1: N Size    \U1F605") %>%
  gt_theme_tl(all_caps = T) %>%
  gtsave(here::here("Images/StudentWork/2020-2021/Table1.png"))

### TABLE 2 % ON GRADE LEVEL CREATION

gt2 <- data_clean %>%
  group_by(Curriculum, Prepost) %>%
  summarise(Percent = round((n() - sum(Score == "Below grade-level"))/n(), 2)) %>%
  mutate(Prepost = str_replace_all(Prepost, c("Pre" = "Fall", "Post" = "Spring"))) %>%
  pivot_wider(names_from = "Prepost", values_from = "Percent", names_sort = T) %>%
  ungroup()

gt2 %>%
  gt(rowname_col = "Curriculum") %>%
  fmt_percent(
    columns = c(Spring, Fall),
    scale_values = T,
    decimals = 0
  ) %>%
  data_color(
    columns = c(Spring),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  tab_source_note(source_note = "Fall ELA n = 68, Spring ELA n = 22, Fall Math n = 30, Spring Math n = 14") %>%
  tab_header(title = "\U1F605    Table 2: % On-grade Level Tasks    \U1F605") %>%
  gt_theme_tl(all_caps = T) %>%
  gtsave(here::here("Images/StudentWork/2020-2021/Table2.png"))

### TABLE 3 AVERAGE SCORE (ALL)

gt3 <- data_clean %>%
  mutate(Score = as.numeric(unlist(Score))) %>%
  mutate(Score = replace_na(Score, 0)) %>%
  group_by(Curriculum, Prepost) %>%
  summarise(Score = round(mean(Score), 2)) %>%
  mutate(Prepost = str_replace_all(Prepost, c("Pre" = "Fall", "Post" = "Spring"))) %>%
  pivot_wider(names_from = "Prepost", values_from = "Score", names_sort = T) %>%
  ungroup()

gt3 %>%
  gt(rowname_col = "Curriculum") %>%
  tab_header(title = "\U1F605    Table 3: Average Score (All)     \U1F605") %>%
  data_color(
    columns = c(Spring),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  tab_source_note(source_note = "Fall ELA n = 68, Spring ELA n = 22, Fall Math n = 30, Spring Math n = 14") %>%
  gt_theme_tl(all_caps = T) %>%
  gtsave(here::here("Images/StudentWork/2020-2021/Table3.png"))

### TABLE 4 AVERAGE SCORE (MATCHED)

gt4 <- data_clean %>%
  mutate(Score = as.numeric(unlist(Score))) %>%
  mutate(Score = replace_na(Score, 0)) %>%
  group_by(ID) %>% 
  filter(n() > 1) %>% 
  ungroup() %>%
  mutate(Score = unlist(Score)) %>%
  group_by(Curriculum, Prepost) %>%
  summarise(Score = round(mean(Score), 2)) %>%
  mutate(Prepost = str_replace_all(Prepost, c("Pre" = "Fall", "Post" = "Spring"))) %>%
  pivot_wider(names_from = "Prepost", values_from = "Score", names_sort = T) %>%
  ungroup() %>%
  mutate(across(c(2), ~ if_else(.x == 2, 0, .x))) %>%
  mutate(Spring = replace_na(Spring, 2))

gt4 %>%
  gt(rowname_col = "Curriculum") %>%
  tab_header(title = "\U1F605    Table 4: Average Score (Matched)    \U1F605") %>%
  tab_footnote(
    footnote = "n = 5",
    locations = cells_body(
      columns = c(Fall, Spring),
      rows = c(2)
    )
  ) %>%
  tab_footnote(
    footnote = "n = 18",
    locations = cells_body(
      columns = c(Fall, Spring),
      rows = c(1)
    )
  ) %>%
  data_color(
    columns = c(Spring),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  gt_theme_tl(all_caps = T) %>%
  gtsave(here::here("Images/StudentWork/2020-2021/Table4.png"))

### TABLE 5 AVERAGE SCORE (% WHO SCORE 2)

gt5 <- data_clean %>%
  mutate(Score = as.numeric(unlist(Score))) %>%
  mutate(Score = replace_na(Score, 0)) %>%
  group_by(Curriculum, Prepost) %>%
  summarise(Score = round(sum(Score == 2)/n(), 2)) %>%
  mutate(Prepost = str_replace_all(Prepost, c("Pre" = "Fall", "Post" = "Spring"))) %>%
  pivot_wider(names_from = "Prepost", values_from = "Score", names_sort = T) %>%
  ungroup() %>%
  

gt5 %>%
  gt(rowname_col = "Curriculum") %>%
  tab_header(title = "\U1F605    Table 5: % Who Score a 2 (All)    \U1F605") %>%
  data_color(
    columns = c(Spring),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  fmt_percent(
    columns = c(Spring, Fall),
    scale_values = T,
    decimals = 0
  ) %>%
  gt_theme_tl(all_caps = T) %>%
  gtsave(here::here("Images/StudentWork/2020-2021/Table5.png"))

gt5 %>%
  gt(rowname_col = "Curriculum") %>%
  tab_header(title = "\U1F605    Table 5: % Who Score a 2 (All schools)    \U1F605") %>%
  data_color(
    columns = c(Spring),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  fmt_percent(
    columns = c(Spring, Fall),
    scale_values = T,
    decimals = 0
  ) %>%
  tab_source_note(source_note = "Fall ELA n = 68, Spring ELA n = 22, Fall Math n = 30, Spring Math n = 14") %>%
  gt_theme_tl(all_caps = T) %>%
  grand_summary_rows(
    columns = c(Fall, Spring),
    fns = list(Overall = ~ mean(.)),
    formatter = fmt_percent,
    use_seps = F
  ) %>%
  gtsave("~/Teaching Lab/Coding/TeachingLab/Images/2021-2022/Student/Table52020-2021.png")

fake_table <- tibble(
  Curriculum = c("ELA", "Math"),
  `Overall %` = c(0.475, 0.62)
)

fake_table %>%
  gt(rowname_col = "Curriculum") %>%
  tab_header(title = "\U1F605    % Who Score a 2 (All schools)    \U1F605") %>%
  data_color(
    columns = c(`Overall %`),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  fmt_percent(
    columns = c(`Overall %`),
    scale_values = T,
    decimals = 0
  ) %>%
  tab_source_note(source_note = "ELA n = 68, Math n = 30") %>%
  gt_theme_tl(all_caps = T) %>%
  grand_summary_rows(
    columns = c(`Overall %`),
    fns = list(Overall = ~ mean(.)),
    formatter = fmt_percent,
    use_seps = F
  ) %>%
  gtsave("~/Teaching Lab/Coding/TeachingLab/Images/2021-2022/Student/StudentTablefrom2020-2021.png")

### TABLE 6 AVERAGE SCORE (% WHO SCORE 2 MATCHED)

gt6 <- data_clean %>%
  mutate(Score = as.numeric(unlist(Score))) %>%
  mutate(Score = replace_na(Score, 0)) %>%
  group_by(ID) %>% 
  filter(n() > 1) %>% 
  ungroup() %>%
  group_by(Curriculum, Prepost) %>%
  summarise(Score = round(sum(Score == 2)/n(), 2)) %>%
  mutate(Prepost = str_replace_all(Prepost, c("Pre" = "Fall", "Post" = "Spring"))) %>%
  pivot_wider(names_from = "Prepost", values_from = "Score", names_sort = T) %>%
  ungroup()

gt6 %>%
  gt(rowname_col = "Curriculum") %>%
  tab_header(title = "\U1F605    Table 6: % Who Score a 2 (Matched)    \U1F605") %>%
  data_color(
    columns = c(Spring),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  fmt_percent(
    columns = c(Spring, Fall),
    scale_values = T,
    decimals = 0
  ) %>%
  tab_source_note(source_note = "ELA n = 18, Math n = 5") %>%
  gt_theme_tl(all_caps = T) %>%
  gtsave(here::here("Images/StudentWork/2020-2021/Table6.png"))

### END OF TABLES SECTION START OF CUSTOM DATA
data_compare <- data %>% 
  filter(!str_detect(Score, "No Response|Below|Not")) %>%
  mutate(Score = as.numeric(as.character(Score))) %>%
  drop_na(Score) %>%
  group_by(Prepost) %>%
  summarise(Score = sum(Score)/length(Score))

data_compare %>%
  ggplot(aes(x = factor(Prepost, levels = c("Pre", "Post")), y = Score, fill = Prepost, color = Prepost)) +
  geom_col() +
  geom_text(position = position_stack(vjust = 1.03), aes(label = round(Score, 2)), fontface = "bold") +
  scale_x_discrete(labels = c("Before", "After")) +
  scale_fill_manual(values = tl_palette(n = 5, color = "blue", theme = "dark")[c(2, 4)]) +
  scale_color_manual(values = tl_palette(n = 5, color = "blue", theme = "dark")[c(2, 4)]) +
  labs(x = "", y = "Average Score", title = "Difference in Student Scores<br> <span style = 'color:#0182B4;'>**Before**</span> & <span style = 'color:#032E3F;'>**After**</span> Teaching Lab Services") +
  ylim(c(0, 2)) +
  theme_tl() +
  theme(
    legend.position = "none",
    plot.title = element_markdown(lineheight = 1.1)
  )

ggsave(here::here("Images/StudentScores.png"), bg = "white")

data_compare2 <- data %>% 
  filter(!str_detect(Score, "No Response|Below|Not")) %>%
  mutate(Score = as.numeric(as.character(Score))) %>%
  drop_na(Score)

data_compare2 %>%
  ggplot(aes(x = factor(Prepost, levels = c("Pre", "Post")), y = Score, fill = Prepost, color = as.factor(Prepost))) +
  geom_jitter(width = 0.2) +
  geom_hline(data = data_compare, aes(yintercept = Score, color = Prepost), linetype = "dashed") +
  scale_x_discrete(labels = c("Before", "After")) +
  scale_fill_manual(values = tl_palette(n = 5, color = "blue", theme = "dark")[c(2, 4)]) +
  scale_color_manual(values = tl_palette(n = 5, color = "blue", theme = "dark")[c(2, 4)]) +
  labs(x = "", y = "Score", 
       title = "Jittered Difference in Student Scores<br> <span style = 'color:#0182B4;'>**Before**</span> & <span style = 'color:#032E3F;'>**After**</span> Teaching Lab Services",
       caption = glue::glue("Before n = {data_compare2 %>% filter(Prepost == 'Pre') %>% select(Prepost) %>% summarise(len = length(Prepost) %>% as_vector())}, After n = {data_compare2 %>% filter(Prepost == 'Post') %>% select(Prepost) %>% summarise(len = length(Prepost) %>% as_vector())}")) +
  ylim(c(0, 2)) +
  theme_tl() +
  theme(
    legend.position = "none",
    plot.title = element_markdown(lineheight = 1.1)
  )

ggsave(here::here("Images/StudentScoresJittered.png"), bg = "white")

data_compare_facet <- data %>% 
  filter(!str_detect(Score, "No Response|Below|Not")) %>%
  mutate(Score = as.numeric(as.character(Score))) %>%
  drop_na(Score) %>%
  group_by(Prepost, Curriculum) %>%
  summarise(Score = sum(Score)/length(Score),
            n = n())

data_compare2 %>%
  ggplot(aes(x = factor(Prepost, levels = c("Pre", "Post")), y = Score, fill = Prepost, color = as.factor(Prepost))) +
  geom_jitter(width = 0.2) +
  geom_hline(data = data_compare_facet, aes(yintercept = Score, color = Prepost), linetype = "dashed") +
  facet_wrap( ~ Curriculum) +
  scale_x_discrete(labels = c("Before", "After")) +
  scale_fill_manual(values = tl_palette(n = 5, color = "blue", theme = "dark")[c(2, 4)]) +
  scale_color_manual(values = tl_palette(n = 5, color = "blue", theme = "dark")[c(2, 4)]) +
  labs(x = "", y = "Score", 
       title = "Jittered Difference in Student Scores<br> <span style = 'color:#0182B4;'>**Before**</span> & <span style = 'color:#032E3F;'>**After**</span> Teaching Lab Services",
       caption = glue::glue("Before ELA n = {data_compare_facet %>% filter(Prepost == 'Pre' & Curriculum == 'ELA') %>% ungroup() %>% select(n) %>% as_vector()}, After ELA n = {data_compare_facet %>% filter(Prepost == 'Post' & Curriculum == 'ELA') %>% ungroup() %>% select(n) %>% as_vector()}<br>Before Math n = {data_compare_facet %>% filter(Prepost == 'Pre' & Curriculum == 'Math') %>% ungroup() %>% select(n) %>% as_vector()}, After Math n = {data_compare_facet %>% filter(Prepost == 'Post' & Curriculum == 'Math') %>% ungroup() %>% select(n) %>% as_vector()}")) +
  ylim(c(0, 2)) +
  theme_tl() +
  theme(
    legend.position = "none",
    plot.title = element_markdown(lineheight = 1.1),
    strip.text = element_text(hjust = 0.5),
    plot.caption = element_markdown(lineheight = 1.1)
  )

ggsave(here::here("Images/StudentScoresJitteredCurriculum.png"), bg = "white")


