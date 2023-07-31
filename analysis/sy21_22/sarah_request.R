library(googlesheets4)
library(tidyverse)
library(TeachingLab)

ss <- "https://docs.google.com/spreadsheets/d/1dn7EqK-T3IsWoCO0wDUC6m_V8HojurrkudsBurpNGHc/edit#gid=0"

ela_crse <- surveymonkey::fetch_survey_obj(312484554) %>%
  surveymonkey::parse_survey()

replace_vector <- c("Never or rarely" = "1",
  "Sometimes" = "2",
  "Often" = "3",
  "Very often" = "4",
  "All the time" = "5")

percentages_ela_crse <- ela_crse %>%
  filter(str_detect(`Please select your site (district, parish, network, or school)`, "11|Open") |
           str_detect(`Please select your site (district, parish, network, or school) - Other (please specify)`, "11|Open")) %>%
  group_by(respondent_id) %>%
  mutate(n_response = dplyr::n(),
         maxdate = max(date_created)) %>%
  ungroup() %>%
  mutate(prepost = ifelse((date_created > mean(date_created)) | (n_response > 1 & maxdate == date_created), 
                          "post", 
                          "pre")) %>%
  select(prepost, 12:25) %>%
  drop_na(2) %>%
  pivot_longer(!prepost) %>%
  group_by(name, value, prepost) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(name, prepost) %>%
  mutate(percent = round(100*n/sum(n), 2)) %>%
  mutate(value = str_replace_all(value, replace_vector)) %>%
  select(-n) %>%
  pivot_wider(names_from = c(prepost, value), values_from = percent) %>%
  view()

percentages_ela_crse %>%
  select(pre_1, pre_2, pre_3, pre_4, pre_5) %>%
  mutate(name = str_remove_all(name, "How often have you in the last month... - ")) %>%
  ungroup() %>%
  mutate(name = factor(name, levels = c("Drawn on knowledge of students’ cultural backgrounds and identities in daily lessons?",
                                        "Created opportunities for students to share about their cultural background and identities?",
                                        "Chosen texts that highlight themes involving race or ethnicity?",
                                        "Chosen texts that highlight themes involving gender or sexual orientation?",
                                        "Chosen texts that highlight themes involving socioeconomic status?",
                                        "Modified your lessons to include culturally relevant activities and question prompts?",
                                        "Modified your assessments to include culturally relevant references and questions?",
                                        "Chosen or developed assessments that give students a choice about how to demonstrate knowledge and skills?",
                                        "Chosen or developed assessments that allow students to demonstrate their knowledge and skills orally or in other non-text media?",
                                        "Reflected on your identity and cultural background?",
                                        "Engaged in challenging conversations about your identity?",
                                        "Interacted with people who have different lived experiences from yourself?",
                                        "Learned about others’ cultural background and identity by doing things such as reading (books, magazines, newspapers), searching the internet, or keeping up with current events?",
                                        "Learned about your cultural background and identity by doing things such as reading (books, magazines, newspapers), searching the internet, or keeping up with current events?"))) %>%
  arrange(name) %>%
  select(-name) %>%
  mutate(across(everything(), ~ replace_na(.x, 0))) %>%
  googlesheets4::range_write(ss = ss,
                             data = .,
                             sheet = 1,
                             range = "B3:F16",
                             col_names = F,
                             reformat = F)


percentages_ela_crse %>%
  select(post_1, post_2, post_3, post_4, post_5) %>%
  mutate(name = str_remove_all(name, "How often have you in the last month... - ")) %>%
  ungroup() %>%
  mutate(name = factor(name, levels = c("Drawn on knowledge of students’ cultural backgrounds and identities in daily lessons?",
                                        "Created opportunities for students to share about their cultural background and identities?",
                                        "Chosen texts that highlight themes involving race or ethnicity?",
                                        "Chosen texts that highlight themes involving gender or sexual orientation?",
                                        "Chosen texts that highlight themes involving socioeconomic status?",
                                        "Modified your lessons to include culturally relevant activities and question prompts?",
                                        "Modified your assessments to include culturally relevant references and questions?",
                                        "Chosen or developed assessments that give students a choice about how to demonstrate knowledge and skills?",
                                        "Chosen or developed assessments that allow students to demonstrate their knowledge and skills orally or in other non-text media?",
                                        "Reflected on your identity and cultural background?",
                                        "Engaged in challenging conversations about your identity?",
                                        "Interacted with people who have different lived experiences from yourself?",
                                        "Learned about others’ cultural background and identity by doing things such as reading (books, magazines, newspapers), searching the internet, or keeping up with current events?",
                                        "Learned about your cultural background and identity by doing things such as reading (books, magazines, newspapers), searching the internet, or keeping up with current events?"))) %>%
  arrange(name) %>%
  select(-name) %>%
  mutate(across(everything(), ~ replace_na(.x, 0))) %>%
  googlesheets4::range_write(ss = ss,
                             data = .,
                             sheet = 1,
                             range = "G3:K16",
                             col_names = F,
                             reformat = F)

ela_crse %>%
  filter(str_detect(`Please select your site (district, parish, network, or school)`, "11|Open") |
           str_detect(`Please select your site (district, parish, network, or school) - Other (please specify)`, "11|Open")) %>%
  group_by(respondent_id) %>%
  mutate(n_response = dplyr::n(),
         maxdate = max(date_created)) %>%
  ungroup() %>%
  mutate(prepost = ifelse((date_created > mean(date_created)) | (n_response > 1 & maxdate == date_created), 
                          "post", 
                          "pre")) %>%
  group_by(prepost) %>%
  count(sort = T)
