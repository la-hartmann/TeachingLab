library(googlesheets4)
library(tidyverse)
library(TeachingLab)
library(gt)

attendance <- read_sheet("https://docs.google.com/spreadsheets/d/15mSsAWPq-d0s-syNHKhgUPf4iHYr-zP0uslxKcRheIc/edit#gid=1817148802")

attendance %>%
  filter(`Date for this training` == as.Date("2022-04-06") & 
           `Name of district/parish/network/school` == "Orleans Central Supervisory Union, VT") %>%
  select(5, 6, 12, 14) %>%
  gt::gt() %>%
  tab_header(title = md("**Attendance Report OCSU Virtual EL K-2 Skills Inquiry Cycle 2 for 4/6**")) %>%
  gt_theme_tl() %>%
  gtsave(here::here("images/attendance/04_06_2022_ocsu_elk2.png"))

attendance %>%
  filter(`Date for this training` == as.Date("2022-03-29") & 
           `Name of district/parish/network/school` == "Orleans Central Supervisory Union, VT") %>%
  select(5, 6, 12, 14) %>%
  gt::gt() %>%
  tab_header(title = md("**Attendance Report OCSU Virtual EL K-2 FSOT School Leader Series for 3/29**")) %>%
  gt_theme_tl() %>%
  gtsave(here::here("images/attendance/03_29_2022_ocsu_elk2.png"))

