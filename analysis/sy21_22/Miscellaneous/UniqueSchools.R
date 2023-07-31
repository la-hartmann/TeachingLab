library(tidyverse)
library(stringdist)
library(reshape2)
library(googlesheets4)

schools <- read_sheet("https://docs.google.com/spreadsheets/d/15mSsAWPq-d0s-syNHKhgUPf4iHYr-zP0uslxKcRheIc/edit#gid=1817148802")

schools %>% dplyr::filter(`Date for this training` > "2020-06-30" & `Date for this training` < "2021-06-30") %>% 
  count(`Your school. If you do not work at a school site, please leave this field blank.`) %>%
  drop_na(`Your school. If you do not work at a school site, please leave this field blank.`) %>%
  mutate(unique_schools = nrow(.)) %>%
  arrange(desc(n)) -> schools_count

schools_count_likely <- schools_count %>%
  dplyr::filter(n > 5) %>%
  select(school = `Your school. If you do not work at a school site, please leave this field blank.`) %>%
  as_vector() %>%
  str_to_title()

schools_count %>%
  rename(school = `Your school. If you do not work at a school site, please leave this field blank.`) %>%
  mutate(school = str_to_lower(school)) %>%
  select(-2, -3) -> just_schools
  # pivot_wider(names_from = school, values_from = school) %>%
  # mutate(stringsim = stringdist::stringsim())
stringdist::stringdistmatrix(just_schools$school, just_schools$school, method = "jw") -> overall_sim

rownames(overall_sim) <- just_schools$school
colnames(overall_sim) <- just_schools$school

m = as.matrix(overall_sim)

out <- melt(m) %>% distinct() %>% filter(value < 0.075)

out %>%
  dplyr::filter(value != 0) -> matches

matches %>%
  mutate(Var1_title = str_to_title(Var1),
         exists = Var1_title %in% schools_count_likely) -> check_matches


