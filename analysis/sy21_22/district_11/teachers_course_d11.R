library(tidyverse)
library(gt)
library(googlesheets4)

# rename_vector <- c(
#   "EL EL Bootcamp: Modules (K-8)" = "EL Bootcamp: Modules (K-8)",
#   "EL EL Bootcamp: Skills (K-2)" = "EL Bootcamp: Skills (K-2)",
#   "NA EL School Leaders Module 1" = "EL School Leaders Module 1",
#   "NA EL Inquiry Cycle 1: Speaking & Listening (K-8)" = "EL Inquiry Cycle 1: Speaking & Listening (K-8)",
#   "NA EL School Leaders Module 1" = "EL School Leaders Module 1"
# )
# 
# rename_vector <- c(
#   "EL EL" = "EL",
#   "NA EL" = "EL"
# )
# 
# teaching_df %>%
#   filter(str_detect(`District, Parish, Or Network`, "District 11|RobinHood|Robinhood|Robin Hood")) %>%
#   mutate(`Professional Training Session` = str_replace_all(`Professional Training Session`, rename_vector)) %>%
#   mutate(`District, Parish, Or Network` = case_when(str_detect(`Professional Training Session`, "Robin") ~ "District 11 General",
#                                                     !str_detect(`Professional Training Session`, "Robin") ~ "Robinhood")) %>%
#   group_by(`Professional Training Session`, `District, Parish, Or Network`) %>%
#   summarise(n()) %>%
#   arrange(desc(`n()`)) %>%
#   mutate(Rank = row_number()) %>%
#   drop_na() %>%
#   gt::gt() %>%
#   gt::tab_header(title = md("**District 11: Teachers Per Course**")) %>%
#   TeachingLab::gt_theme_tl() %>%
#   gt::cols_label(
#     `District, Parish, Or Network` = md("**District**"),
#     `n()` = md("**N**"),
#     `Rank` = md("**Rank**"),
#     `Professional Training Session` = md("**Professional Training Session**")
#   ) %>%
#   cols_align(align = "center") %>%
#   data_color(
#     columns = vars(`n()`),
#     colors = scales::col_numeric(
#       palette = paletteer::paletteer_d(
#         palette = "ggsci::red_material"
#       ) %>% as.character(),
#       domain = NULL
#     )
#   ) %>%
#   gtsave(here::here("Images/D11/course_data_save.png"))
# 
# df <- read_sheet("https://docs.google.com/spreadsheets/d/1DhVmQWqOzXab1EARPuleWSdZirNNuAcENhG2Wn2IVwU/edit?resourcekey#gid=87988906", skip = 2, sheet = 4) %>%
#   distinct(`Your work email address.`, .keep_all = T) %>%
#   filter(str_detect(`Your site (district, parish, network, or school)`, "District 11"))
# 
# teaching_df %>%
#   mutate(`Professional Training Session` = str_replace_all(`Professional Training Session`, rename_vector)) %>%
#   group_by(`Professional Training Session`) %>%
#   summarise(n()) %>%
#   arrange(desc(`n()`)) %>%
#   mutate(Rank = row_number()) %>%
#   gt::gt() %>%
#   gt::tab_header(title = md("**District 11: Teachers Per Course**")) %>%
#   TeachingLab::gt_theme_tl() %>%
#   gt::cols_label(
#     `n()` = md("**N**"),
#     `Rank` = md("**Rank**"),
#     `Professional Training Session` = md("**Professional Training Session**")
#   ) %>%
#   cols_align(align = "center") %>%
#   data_color(
#     columns = vars(`n()`),
#     colors = scales::col_numeric(
#       palette = paletteer::paletteer_d(
#         palette = "ggsci::red_material"
#       ) %>% as.character(),
#       domain = NULL
#     )
#   )

# Write fall data
read_sheet("https://docs.google.com/spreadsheets/d/1DhVmQWqOzXab1EARPuleWSdZirNNuAcENhG2Wn2IVwU/edit?resourcekey#gid=87988906", skip = 2, sheet = 4) %>%
  distinct(`Your work email address.`, .keep_all = T) %>%
  filter(str_detect(`Your site (district, parish, network, or school)`, "District 11")) %>%
  select(2) %>%
  range_write(ss = "https://docs.google.com/spreadsheets/d/11tuxxvnoSSFGeAgVaV8zwIpSpghR6S_BOmHMybY-r9s/edit#gid=0", range = "A29:A", sheet = 1, col_names = F)


# Write spring data
read_sheet("https://docs.google.com/spreadsheets/d/1e3IvUw36O9jjYqZOU6alOClBsGmA6Sy6Wzr4Bz7fNG4/edit#gid=180928328", skip = 1, sheet = 1) %>% 
  distinct(`Your work email address.`, .keep_all = T) %>%
  filter(str_detect(`Your site (district, parish, network, or school)`, "District 11")) %>%
  select(2) %>%
  range_write(ss = "https://docs.google.com/spreadsheets/d/11tuxxvnoSSFGeAgVaV8zwIpSpghR6S_BOmHMybY-r9s/edit#gid=0", range = "A3:A", sheet = 1, col_names = F)


fall <- read_sheet("https://docs.google.com/spreadsheets/d/1DhVmQWqOzXab1EARPuleWSdZirNNuAcENhG2Wn2IVwU/edit?resourcekey#gid=87988906", skip = 2, sheet = 4) %>%
  distinct(`Your work email address.`, .keep_all = T) %>%
  filter(str_detect(`Your site (district, parish, network, or school)`, "District 11")) %>%
  select(2) %>%
  mutate(Diagnostic = "Y")

spring <- read_sheet("https://docs.google.com/spreadsheets/d/1e3IvUw36O9jjYqZOU6alOClBsGmA6Sy6Wzr4Bz7fNG4/edit#gid=180928328", skip = 1, sheet = 1) %>% 
  distinct(`Your work email address.`, .keep_all = T) %>%
  filter(str_detect(`Your site (district, parish, network, or school)`, "District 11")) %>%
  select(2) %>%
  mutate(`Follow-up` = "Y")

# Check Diagnostic
read_sheet("https://docs.google.com/spreadsheets/d/11tuxxvnoSSFGeAgVaV8zwIpSpghR6S_BOmHMybY-r9s/edit?usp=sharing", skip = 1, sheet = 1) %>%
  full_join(fall, by = c("Email" = "Your work email address.", "Diagnostic (Y/N)" = "Diagnostic")) %>%
  full_join(spring, by = c("Email" = "Your work email address.", "Follow up (Y/N)" = "Follow-up")) %>%
  filter(if_any(c(2,3), ~ str_detect(.x, "Y"))) %>%
  select(2, 3) %>%
  range_write(ss ="https://docs.google.com/spreadsheets/d/11tuxxvnoSSFGeAgVaV8zwIpSpghR6S_BOmHMybY-r9s/edit?usp=sharing", col_names = F, range = "B3:C", sheet = 1)






