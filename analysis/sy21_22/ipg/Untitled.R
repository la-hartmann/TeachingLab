library(tidyverse)
library(gt)

ipg_forms <- TeachingLab::get_ipg_forms()

ipg_forms %>% 
  filter(`Your Name` == "Pamela Simon") %>% 
  select(-iry) %>%
  mutate_all( ~ as.character(.x)) %>%
  pivot_longer(everything(), names_to = "Question", values_to = "Answer") %>% 
  drop_na(Answer) %>%
  filter(Answer != "NULL") %>%
  gt::gt() %>%
  TeachingLab::gt_theme_tl() %>%
  gtsave(here::here("images/ipg_forms/diana_mcnairy_1.png"))
