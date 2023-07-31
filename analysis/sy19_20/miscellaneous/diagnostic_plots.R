diagnostic_black <- diagnostic %>%
  filter(how_do_you_describe_your_race_you_may_check_more_than_one_box_black_or_african_american == "Black or African American") %>%
  nrow()

diagnostic_no_pref <- diagnostic %>%
  filter(is.na(how_do_you_describe_your_race_you_may_check_more_than_one_box_i_prefer_not_to_say) &
           is.na(how_do_you_describe_your_race_you_may_check_more_than_one_box_i_prefer_to_self_describe)) %>%
  nrow()

diagnostic_black/diagnostic_no_pref

diagnostic_black_woman <- diagnostic %>%
  filter(how_do_you_describe_your_race_you_may_check_more_than_one_box_black_or_african_american == "Black or African American" &
           gender_how_do_you_identify == "Woman") %>%
  nrow()

diagnostic_black_woman/diagnostic_no_pref

quick_race_plot <- diagnostic %>%
  select(contains("race_you_may")) %>%
  pivot_longer(everything()) %>%
  drop_na(value) %>%
  rename(race = value) %>%
  group_by(race) %>%
  mutate(race = fct_lump(race, n = 5, other_level = "other")) %>%
  count(race, sort = T) %>%
  filter(n > 4) %>%
  ggplot(aes(x = fct_reorder(race, n, .desc = T), y = n, fill = race)) +
  geom_col() +
  scale_fill_tl(n = 5) +
  labs(x = "race") +
  theme_tl()

quick_race_plot %>%
  ggsave(filename = here::here("images/race_plot.png"),
         width = 9, height = 9, bg = "white")



student_survey <- TeachingLab::get_student_survey()

female_black_student_count <- student_survey %>%
  filter(`What is your gender?` == "Female" &
           `What is your race? Select all that apply. - Black or African American` == "Black or African American") %>%
  nrow()

female_black_student_count/nrow(student_survey)

unique(diagnostic$how_do_you_describe_your_race_you_may_check_more_than_one_box_black_or_african_american)

colnames(student_survey) %>%
  as_tibble() %>%
  view()

quick_plot <- student_survey %>%
  select(contains("race") | contains("gender")) %>%
  pivot_longer(cols = contains("race"), names_to = "Race", values_to = "is_race") %>%
  mutate(Race = html_wrap(str_remove_all(Race, "What is your race\\? Select all that apply\\. - "),
                          n = 15)) %>%
  rename(gender = `What is your gender?`) %>%
  drop_na(is_race) %>%
  group_by(Race, gender) %>%
  count(sort = T) %>%
  ggplot(aes(x = fct_reorder2(Race, n, gender), y = n, fill = gender)) +
  geom_col(position = position_dodge()) +
  scale_fill_tl(n = 4, color = "blue") +
  labs(x = "Race", fill = "Gender") +
  TeachingLab::theme_tl(markdown = T,
                        legend = T) +
  theme(legend.position = "top")

ggsave(plot = quick_plot,
       filename = here::here("images/gender_race_student_survey.png"),
       width = 12, 
       height = 10,
       bg = "white")
