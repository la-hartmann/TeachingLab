library(tidyverse)
library(gt)
library(googlesheets4)

diagnostic <- read_sheet("https://docs.google.com/spreadsheets/d/1DhVmQWqOzXab1EARPuleWSdZirNNuAcENhG2Wn2IVwU/edit#gid=87988906", 
                         skip = 1, sheet = "Form Responses 1")

# Only Yes
diagnostic %>%
  filter(pre_priorpart == "Yes" & pre_subject == "Mathematics") %>%
  group_by(pre_site) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  janitor::adorn_totals() %>%
  gt() %>%
  tab_header(title = md("**Teaching Lab Sites Count**"),
             subtitle = md("Filtering for only first time Teaching Lab participants\nin Mathematics from the diagnostic survey.")) %>%
  cols_label(
    pre_site = md("**Site**"),
    n = md("**N**")
  ) %>%
  gt_theme_tl() %>%
  gtsave(here::here("Images/SY20-21 Tables/TLSitesMathCount.png"))

# Either Yes or No
diagnostic %>%
  # filter(pre_subject == "Mathematics") %>%
  group_by(pre_site) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  janitor::adorn_totals() %>%
  gt() %>%
  tab_header(title = md("**Teaching Lab Sites Count**"),
             subtitle = md("Filtering for only first time Teaching Lab participants\nin Mathematics from the diagnostic survey.")) %>%
  cols_label(
    pre_site = md("**Site**"),
    n = md("**N**")
  ) %>%
  gt_theme_tl() %>%
  gtsave(here::here("Images/SY20-21 Tables/TLSitesMathCount2.png"))
