library(tidyverse)
library(readxl)
library(here)
library(TeachingLab)


round3 <- readxl::read_excel(here::here("Data/Spring 2021 Survey Data Sent to TL.xlsx")) %>%
  slice(-1) #%>%
  # filter(ID %!in% c(2, 27))

index1 <- tibble(question = c("school", "school_role", "BennK2"),
                coding = list("Bennington", "Teacher", "Yes"))

initial_info <- pmap_df(list(index1$question, index1$coding), 
                                   ~ score_question(data = round3, question = ..1, coding = ..2)) %>%
  mutate(answer = index1$coding)

positive_vector <- c("True", "Very True")
negative_vector <- c("Untrue", "Very Untrue")


## Mindsets
index2 <- tibble(question = c("TL_mindsets_1", "TL_mindsets_2", "TL_mindsets_3", "TL_mindsets_4", "TL_mindsets_5"),
                coding = list("negative", "negative", "positive", "negative", "negative"))

mindsets <- map2_df(.x = index2$question, .y = index2$coding, 
                                   ~ score_one_question_mindsets(data = round3, question = .x, coding = .y, na_remove = T, likert = 5))
# Growth
mindsets_growth <- mindsets %>%
  slice(c(1:2)) %>%
  select(percent = score) %>%
  summarise(average = mean(percent))
mindsets_growth
# High Expectations and Beliefs
mindsets_expectations <- mindsets %>%
  slice(c(3:5)) %>%
  select(percent = 1) %>%
  summarise(average = mean(percent))
mindsets_expectations

## Instructional Shifts
round3 %>% 
  select(TL_instshifts_4) %>% 
  drop_na() %>% 
  group_by(TL_instshifts_4) %>% 
  summarise(n = n()) %>%
  mutate(percent = n/sum(n))
            

index3 <- tibble(question = c("TL_instshifts_1", "TL_instshifts_2", "TL_instshifts_3", "TL_instshifts_4", "TL_designshifts"),
                 coding = list("Literacy Instructional Shift", "Literacy Instructional Shift", "Not a Literacy Instructional Shift", "Not a Literacy Instructional Shift", "A complex text that is worthy of reading multiple times."))

inst_shifts <- map2_df(index3$question, index3$coding, 
                    ~ score_question(data = round3, question = .x, coding = .y, na_type = "NA"))
inst_shifts %>%
  select(percent = 1) %>%
  slice(c(1, 3, 5, 7, 9)) %>%
  summarise(mean = mean(percent))

## Fluency
index4 <- tibble(question = c("TL_fluency_1", "TL_fluency_2", "TL_fluency_3", "TL_fluency_4"),
                 coding = list("True", "True", "False", "False"))

fluency <- map2_df(index4$question, index4$coding, 
                       ~ score_question(data = round3, question = .x, coding = .y, na_type = "NA"))

fluency %>%
  select(percent = 1) %>%
  summarise(mean = mean(percent))

## Text Complexity

index5 <- tibble(question_pre = c("TL_complextext_1", "TL_complextext_2", "TL_complextext_3", "TL_complextext_4"),
                 question_post = c("TL_instshifts_1", "TL_instshifts_2", "TL_instshifts_3", "TL_instshifts_4"),
                 coding = list("Aligned", "Aligned", "Not Aligned", "Not Aligned"))

text_comp <- pmap_df(list(index5$question_pre, index5$question_post, index5$coding), 
                   ~ score_question_improved(round3, question_pre = ..1, question_post = ..2, 
                                             coding = ..3, middle_value = "I'm not sure")) %>%
  select(-c(n2, percent_improve_sustain, post_percent))

text_comp %>%
  select(percent = 1) %>%
  summarise(mean = mean(percent))

## Building Knowledge

index6 <- tibble(question = c("TL_buildknow"),
                 coding = list("Students with low reading ability and a lot of knowledge about the food chain."))

build <- map2_df(index6$question, index6$coding, 
                     ~ score_question(round3, question = .x, coding = .y))

build %>%
  select(percent = 1) %>%
  summarise(mean = mean(percent))

## Supporting Students

index7 <- tibble(question = c("TL_supstud"),
                 coding = list("Provide students with lower reading abilities an audio version of the main text"))

sup <- map2_df(index7$question, index7$coding, 
                 ~ score_question(round3, question = .x, coding = .y))

sup %>%
  select(percent = 1) %>%
  summarise(mean = mean(percent))

# Teachers

index8 <- tibble(question = c("TL_teachers_1", "TL_teachers_2", "TL_teachers_3", "TL_teachers_4"),
                 coding = list(c("Agree", "Strongly Agree"), c("Agree", "Strongly Agree"), c("Agree", "Strongly Agree"), c("Agree", "Strongly Agree")))

teachers <- map2_df(index8$question, index8$coding, 
               ~ score_question(round3, question = .x, coding = .y, na_type = "NA"))

teachers

# Bennington Table 4

Bennington_Culture <- round3 %>% filter(school == "Bennington") %>% select(25:28) %>%
  drop_na()

bennington_index <- tibble(questions = colnames(Bennington_Culture),
                           coding = list(c("Agree", "Strongly Agree"), c("Agree", "Strongly Agree"), 
                                         c("Agree", "Strongly Agree"), c("Agree", "Strongly Agree")))

map2_df(.x = bennington_index$questions, .y = bennington_index$coding, ~ TeachingLab::score_question(data = Bennington_Culture,
                                                                                                    question = .x,
                                                                                                    coding = .y,
                                                                                                    na_type = "NA"))

# Steam Table 5
Steam_Culture <- round3 %>% filter(school == "STEAM Bridge") %>% select(25:28) %>%
  drop_na()

steam_index <- tibble(questions = colnames(Steam_Culture),
                           coding = list(c("Agree", "Strongly Agree"), c("Agree", "Strongly Agree"), 
                                         c("Agree", "Strongly Agree"), c("Agree", "Strongly Agree")))

map2_df(.x = steam_index$questions, .y = steam_index$coding, ~ TeachingLab::score_question(data = Steam_Culture,
                                                                                                     question = .x,
                                                                                                     coding = .y,
                                                                                                     na_type = "NA"))



