library(readxl)
library(tidyverse)
library(here)

og_df <- read_excel(here("data/sy19_20/SY19-20 Fall Spring merged.xlsx"))


### Work from here
identifiers <- og_df %>%
  select(district, school, portfolio, id, role, role2...10, role_txt)

fall_df <- og_df %>%
  select_at(vars(contains("pre"))) %>%
  bind_cols(identifiers)


spring_df <- og_df %>%
  select_at(vars(contains("post"))) %>%
  bind_cols(identifiers)


### ELA CONTENT: 12a-23d
## Average Scores
# 14a-14d, 18a-18d need recoding
fall_averages_ela <- fall_df %>%
  select(14:40) %>%
  summarise(
    pre12a = sum(pre12a == "Yes", na.rm = T)/sum(!is.na(pre12a)),
    pre12b = sum(pre12b == "Yes", na.rm = T)/sum(!is.na(pre12b)),
    pre12c = sum(pre12c == "No", na.rm = T)/sum(!is.na(pre12c)),
    pre12d = sum(pre12d == "No", na.rm = T)/sum(!is.na(pre12d)),
    pre13 = sum(pre13 == "A complex text that is worthy of reading multiple times.", na.rm = T)/sum(!is.na(pre13)),
    pre14a = sum(pre14a == "True", na.rm = T)/sum(!is.na(pre14a)),
    pre14b = sum(pre14b == "True", na.rm = T)/sum(!is.na(pre14b)),
    pre14c = sum(pre14c == "False", na.rm = T)/sum(!is.na(pre14c)),
    pre14d = sum(pre14d == "False", na.rm = T)/sum(!is.na(pre14d)),
    pre15 = sum(pre15 == "Students independently read aloud texts at their reading level.", na.rm = T)/sum(!is.na(pre15)),
    pre16 = sum(pre16 == "Ability to read complex text independently and proficiently.", na.rm = T)/sum(!is.na(pre16)),
    pre17a = sum(pre17a == "Yes", na.rm = T)/sum(!is.na(pre17a)),
    pre17b = sum(pre17b == "Yes", na.rm = T)/sum(!is.na(pre17b)),
    pre17c = sum(pre17c == "No", na.rm = T)/sum(!is.na(pre17c)),
    pre17d = sum(pre17d == "No", na.rm = T)/sum(!is.na(pre17d)),
    pre18a = sum(pre18a == "True", na.rm = T)/sum(!is.na(pre18a)),
    pre18b = sum(pre18b == "True", na.rm = T)/sum(!is.na(pre18b)),
    pre18c = sum(pre18c == "False", na.rm = T)/sum(!is.na(pre18c)),
    pre18d = sum(pre18d == "False", na.rm = T)/sum(!is.na(pre18d)),
    pre19 = sum(pre19 == "Students pull out evidence from the text to explain their thinking in response to questions.", na.rm = T)/sum(!is.na(pre19)),
    pre20 = sum(pre20 == "Students with low reading ability and a lot of knowledge about the food chain.", na.rm = T)/sum(!is.na(pre20)),
    pre21 = sum(pre21 == "Have students read a series of additional texts at a variety of complexity levels on the topic.", na.rm = T)/sum(!is.na(pre21)),
    pre22 = sum(pre22 == "Provide students with lower reading abilities an audio version of the main text to listen to before reading the main text in class.", na.rm = T)/sum(!is.na(pre22)),
    pre23a = sum(pre23a == "Yes", na.rm = T)/sum(!is.na(pre23a)),
    pre23b = sum(pre23b == "Yes", na.rm = T)/sum(!is.na(pre23b)),
    pre23c = sum(pre23c == "No", na.rm = T)/sum(!is.na(pre23c)),
    pre23d = sum(pre23d == "No", na.rm = T)/sum(!is.na(pre23d))
  ) %>%
  mutate(across(everything(), ~ .x * 100))

## Constructs within


### MATH CONTENT: 24a-32
fall_averages_math <- fall_df %>%
  select(40:61) %>%
  summarise(
    pre24a = sum(pre24a == "Yes", na.rm = T)/sum(!is.na(pre24a)),
    pre24b = sum(pre24b == "Yes", na.rm = T)/sum(!is.na(pre24b)),
    pre24c = sum(pre24c == "No", na.rm = T)/sum(!is.na(pre24c)),
    pre24d = sum(pre24d == "No", na.rm = T)/sum(!is.na(pre24d)),
    pre25 = sum(pre25 == "Giving students opportunities to develop deep commands of the how, why, and when of mathematical concepts.", na.rm = T)/sum(!is.na(pre25)),
    pre26a = sum(pre26a == "Yes", na.rm = T)/sum(!is.na(pre26a)),
    pre26b = sum(pre26b == "Yes", na.rm = T)/sum(!is.na(pre26b)),
    pre26c = sum(pre26c == "No", na.rm = T)/sum(!is.na(pre26c)),
    pre26d = sum(pre26d == "No", na.rm = T)/sum(!is.na(pre26d)),
    pre27 = sum(pre27 == "Students’ beliefs about whether or not they are a “math person” influences their performance in math class.", na.rm = T)/sum(!is.na(pre27)),
    pre28 = sum(pre28 == "Students need to receive on grade-level instruction every year in order to graduate high school ready for college and career.", na.rm = T)/sum(!is.na(pre28)),
    pre29 = sum(pre29 == "Having the teacher simplify the language and task complexity for students who are English learners.", na.rm = T)/sum(!is.na(pre29)),
    pre30a = sum(pre30a == "Yes", na.rm = T)/sum(!is.na(pre30a)),
    pre30b = sum(pre30b == "Yes", na.rm = T)/sum(!is.na(pre30b)),
    pre30c = sum(pre30c == "No", na.rm = T)/sum(!is.na(pre30c)),
    pre30d = sum(pre30d == "No", na.rm = T)/sum(!is.na(pre30d)),
    pre31a = sum(pre31a == "Yes", na.rm = T)/sum(!is.na(pre31a)),
    pre31b = sum(pre31b == "Yes", na.rm = T)/sum(!is.na(pre31b)),
    pre31c = sum(pre31c == "No", na.rm = T)/sum(!is.na(pre31c)),
    pre31d = sum(pre31d == "No", na.rm = T)/sum(!is.na(pre31d)),
    pre32 = sum(pre32 == "They describe how students will demonstrate their understanding.", na.rm = T)/sum(!is.na(pre32))
  ) %>%
  mutate(across(everything(), ~ .x * 100))


### ELA CONTENT: 12a-23d
## Average Scores
# 14a-14d, 18a-18d need recoding
spring_averages_ela <- spring_df %>%
  select(21:47) %>%
  summarise(
    post12a = sum(post12a == "Yes", na.rm = T)/sum(!is.na(post12a)),
    post12b = sum(post12b == "Yes", na.rm = T)/sum(!is.na(post12b)),
    post12c = sum(post12c == "No", na.rm = T)/sum(!is.na(post12c)),
    post12d = sum(post12d == "No", na.rm = T)/sum(!is.na(post12d)),
    post13 = sum(post13 == "A complex text that is worthy of reading multiple times.", na.rm = T)/sum(!is.na(post13)),
    post14a = sum(post14a == "True", na.rm = T)/sum(!is.na(post14a)),
    post14b = sum(post14b == "True", na.rm = T)/sum(!is.na(post14b)),
    post14c = sum(post14c == "False", na.rm = T)/sum(!is.na(post14c)),
    post14d = sum(post14d == "False", na.rm = T)/sum(!is.na(post14d)),
    post15 = sum(post15 == "Students independently read aloud texts at their reading level.", na.rm = T)/sum(!is.na(post15)),
    post16 = sum(post16 == "Ability to read complex text independently and proficiently.", na.rm = T)/sum(!is.na(post16)),
    post17a = sum(post17a == "Yes", na.rm = T)/sum(!is.na(post17a)),
    post17b = sum(post17b == "Yes", na.rm = T)/sum(!is.na(post17b)),
    post17c = sum(post17c == "No", na.rm = T)/sum(!is.na(post17c)),
    post17d = sum(post17d == "No", na.rm = T)/sum(!is.na(post17d)),
    post18a = sum(post18a == "True", na.rm = T)/sum(!is.na(post18a)),
    post18b = sum(post18b == "True", na.rm = T)/sum(!is.na(post18b)),
    post18c = sum(post18c == "False", na.rm = T)/sum(!is.na(post18c)),
    post18d = sum(post18d == "False", na.rm = T)/sum(!is.na(post18d)),
    post19 = sum(post19 == "Students pull out evidence from the text to explain their thinking in response to questions.", na.rm = T)/sum(!is.na(post19)),
    post20 = sum(post20 == "Students with low reading ability and a lot of knowledge about the food chain.", na.rm = T)/sum(!is.na(post20)),
    post21 = sum(post21 == "Have students read a series of additional texts at a variety of complexity levels on the topic.", na.rm = T)/sum(!is.na(post21)),
    post22 = sum(post22 == "Provide students with lower reading abilities an audio version of the main text to listen to before reading the main text in class.", na.rm = T)/sum(!is.na(post22)),
    post23a = sum(post23a == "Yes", na.rm = T)/sum(!is.na(post23a)),
    post23b = sum(post23b == "Yes", na.rm = T)/sum(!is.na(post23b)),
    post23c = sum(post23c == "No", na.rm = T)/sum(!is.na(post23c)),
    post23d = sum(post23d == "No", na.rm = T)/sum(!is.na(post23d))
  ) %>%
  mutate(across(everything(), ~ .x * 100))

## Constructs within


### MATH CONTENT: 24a-32
spring_averages_math <- spring_df %>%
  select(48:68) %>%
  summarise(
    post24a = c(sum(post24a == "Yes", na.rm = T)/sum(!is.na(post24a)), length(which(!is.na(post24a)))),
    post24b = c(sum(post24b == "Yes", na.rm = T)/sum(!is.na(post24b)), length(which(!is.na(post24b)))),
    post24c = c(sum(post24c == "No", na.rm = T)/sum(!is.na(post24c)), length(which(!is.na(post24c)))),
    post24d = c(sum(post24d == "No", na.rm = T)/sum(!is.na(post24d)), length(which(!is.na(post24d)))),
    post25 = c(sum(post25 == "Giving students opportunities to develop deep commands of the how, why, and when of mathematical concepts.", na.rm = T)/sum(!is.na(post25)), length(which(!is.na(post25)))),
    post26a = c(sum(post26a == "Yes", na.rm = T)/sum(!is.na(post26a)), length(which(!is.na(post26a)))),
    post26b = c(sum(post26b == "Yes", na.rm = T)/sum(!is.na(post26b)), length(which(!is.na(post26b)))),
    post26c = c(sum(post26c == "No", na.rm = T)/sum(!is.na(post26c)), length(which(!is.na(post26c)))),
    post26d = c(sum(post26d == "No", na.rm = T)/sum(!is.na(post26d)), length(which(!is.na(post26d)))),
    post27 = c(sum(post27 == "Students’ beliefs about whether or not they are a “math person” influences their performance in math class.", na.rm = T)/sum(!is.na(post27)), length(which(!is.na(post27)))),
    post28 = c(sum(post28 == "Students need to receive on grade-level instruction every year in order to graduate high school ready for college and career.", na.rm = T)/sum(!is.na(post28)), length(which(!is.na(post28)))),
    post29 = c(sum(post29 == "Having the teacher simplify the language and task complexity for students who are English learners.", na.rm = T)/sum(!is.na(post29)), length(which(!is.na(post29)))),
    post30a = c(sum(post30a == "Yes", na.rm = T)/sum(!is.na(post30a)), length(which(!is.na(post30a)))),
    post30b = c(sum(post30b == "Yes", na.rm = T)/sum(!is.na(post30b)), length(which(!is.na(post30b)))),
    post30c = c(sum(post30c == "No", na.rm = T)/sum(!is.na(post30c)), length(which(!is.na(post30c)))),
    post30d = c(sum(post30d == "No", na.rm = T)/sum(!is.na(post30d)), length(which(!is.na(post30d)))),
    post31a = c(sum(post31a == "Yes", na.rm = T)/sum(!is.na(post31a)), length(which(!is.na(post31a)))),
    post31b = c(sum(post31b == "Yes", na.rm = T)/sum(!is.na(post31b)), length(which(!is.na(post31b)))),
    post31c = c(sum(post31c == "No", na.rm = T)/sum(!is.na(post31c)), length(which(!is.na(post31c)))),
    post31d = c(sum(post31d == "No", na.rm = T)/sum(!is.na(post31d)), length(which(!is.na(post31d)))),
    post32 = c(sum(post32 == "They describe how students will demonstrate their understanding.", na.rm = T)/sum(!is.na(post32)), length(which(!is.na(post32))))
  )# %>%
  mutate(across(everything(), ~ .x * 100))

### JOINING FALL AND SPRING
## EL
fall_spring_el_averages <- bind_rows(fall_averages_ela %>% rename_with( ~ str_remove_all(.x, "pre")),
                                     spring_averages_ela %>% rename_with( ~ str_remove_all(.x, "post")),
                                     spring_averages_ela %>% rename_with( ~ str_remove_all(.x, "post")) - 
                                       fall_averages_ela %>% rename_with( ~ str_remove_all(.x, "pre"))) %>%
  rownames_to_column() %>%
  pivot_longer(-rowname, 'variable', 'value') %>%
  pivot_wider(variable, rowname) %>%
  rename(Question = variable, Fall = `1`, Spring = `2`, Improvement = `3`)
## MATH
fall_spring_math_averages <- bind_rows(fall_averages_math %>% rename_with( ~ str_remove_all(.x, "pre")), 
                                       spring_averages_math %>% rename_with( ~ str_remove_all(.x, "post")),
                                       spring_averages_math %>% rename_with( ~ str_remove_all(.x, "post")) - 
                                         fall_averages_math %>% rename_with( ~ str_remove_all(.x, "pre"))) %>%
  rownames_to_column() %>%
  pivot_longer(-rowname, 'variable', 'value') %>%
  pivot_wider(variable, rowname) %>%
  rename(Question = variable, Fall = `1`, Spring = `2`, Improvement = `3`)

### Making Summary Tables
## EL
(el_table <- fall_spring_el_averages %>%
  gt::gt() %>%
  tab_header(
    title = md("**EL Scores in Fall vs. Spring**")
  ) %>%
  tab_style(
    style = cell_fill(color = "forestgreen", alpha = 0.2),
    locations = cells_body(
      columns = vars(Improvement),
      rows = Improvement > 0
      )
  ) %>%
  fmt_percent(
    columns = vars(Fall, Spring, Improvement),
    decimals = 2,
    scale_values = F
  ) %>%
  grand_summary_rows(
    columns = vars(Fall, Spring, Improvement),
    fns = list(
      Average = ~ mean(.)
    ),
    formatter = fmt_percent,
    scale_values = F
  ))
el_table %>%
  gtsave(here("Images/SY19-20ELScores.png"))
## Math
(math_table <- fall_spring_math_averages %>%
  gt::gt() %>%
  tab_header(
    title = md("**Math Scores in Fall vs. Spring**")
  ) %>%
    tab_style(
      style = cell_fill(color = "forestgreen", alpha = 0.2),
      locations = cells_body(
        columns = vars(Improvement),
        rows = Improvement > 0
      )
    ) %>%
  fmt_percent(
    columns = vars(Fall, Spring, Improvement),
    decimals = 2,
    scale_values = F
  ) %>%
  grand_summary_rows(
    columns = vars(Fall, Spring, Improvement),
    fns = list(
      Average = ~ mean(.)
    ),
    formatter = fmt_percent,
    scale_values = F
  ))
math_table %>%
  gtsave(filename = "SY19-20MathScores.png", path = here("Images/"))

### ALSO BY CONSTRUCT WHICH IS FLUENCY, RACE & ETHNICITY, ETC.
###
### REMEMBER TO DO MASSACHEUSSET'S WHEN YOU FINISH ANALYSIS
