library(googlesheets4)
library(tidyverse)
library(tidytext)
library(ggwordcloud)
library(cowplot)
library(ggtext)
library(gt)

ed_survey <- read_sheet("https://docs.google.com/spreadsheets/d/1e3IvUw36O9jjYqZOU6alOClBsGmA6Sy6Wzr4Bz7fNG4/edit#gid=118561712",
                        sheet = 1, skip = 1)

course_survey <- read_sheet("https://docs.google.com/spreadsheets/d/1xfhI6jwUpNdAg4cE3BVrBckvhgV7pgD4ryS4kQDI4S0/edit#gid=704521182",
                            sheet = 1, skip = 1)

### QUOTES PART 1
quotes1 <- ed_survey %>%
  filter(`Do you give us permission to use any of your written comments? Only your role and district will be included.` == "Yes") %>%
  select(3, 5, 103, 105) %>%
  filter(if_any(!c(1, 2), ~ !is.na(.))) %>%
  filter(if_any(!c(1, 2), ~ str_length(.x) > 100))

quasiquote1 <- quotes1 %>% 
  select(1,2,3) %>%
  rename(Quotes = 3)

quasiquote2 <- quotes1 %>% 
  select(1,2,4) %>%
  rename(Quotes = 3)

quotes_all <- bind_rows(quasiquote1, quasiquote2) %>%
  drop_na(Quotes) %>%
  relocate(Quotes, .before = 1)

# ss <- gs4_create(name = "Educator Survey Quotes")
ss <- gs4_find("Educator Survey Quotes")

quotes_all %>%
  write_sheet(data = ., ss = ss, sheet = "Sheet1")

### QUOTES PART 2

quotes2 <- course_survey %>%
  filter(str_detect(`Select your course.`, "IM|Math") & `Do you give us permission to include your feedback in promotional materials?` == "Yes") %>%
  select(3, 4, 5, 13, 12, 22, 26, 27, 31) %>%
  filter(if_any(!c(1, 2, 3), ~ !is.na(.))) %>%
  filter(if_any(!c(1, 2, 3), ~ str_length(.x) > 100))

sentimented <- quotes2 %>%
  mutate(across(everything(), ~ as.character(.x))) %>%
  pivot_longer(!c(1, 2, 3), names_to = "Question", values_to = "Comment") %>%
  select(-4) %>%
  drop_na(Comment) %>%
  filter(Comment != "NULL" & Comment != "N/A") %>%
  tidytext::unnest_tokens(word, Comment, token = "words", drop = F) %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(Comment) %>%
  summarize(sentiment = sum(value),
            School = `Select your site (district, parish, network, or school).`,
            Course = `Select your course.`) %>%
  distinct(Comment, .keep_all = T) %>%
  arrange(desc(sentiment))

# ss <- gs4_create(name = "Course Survey Quotes")
ss <- gs4_find("Course Survey Quotes")

course_quotes_all <- sentimented %>%
  ungroup() %>%
  slice(c(1:100)) %>%
  arrange(desc(sentiment)) %>%
  select(-2) %>%
  mutate(Rank = row_number()) %>%
  relocate(Rank, .before = 1)

course_quotes_all %>%
  write_sheet(data = ., ss = ss, sheet = "Sheet1")
  
  
quotes2 <- course_survey %>%
  filter(str_detect(`Select your course.`, "IM|Math") & `Do you give us permission to include your feedback in promotional materials?` == "Yes") %>%
  select(3, 13, 12, 22, 26, 27, 31) %>%
  filter(if_any(!c(1), ~ !is.na(.))) #%>%
  # filter(if_any(!c(1), ~ str_length(.x) > 100))


affin <- quotes2 %>%
  mutate(across(everything(), ~ as.character(.x))) %>%
  pivot_longer(!c(1), names_to = "Question", values_to = "Comment") %>%
  group_by(`Select your course.`, Question) %>%
  tidytext::unnest_tokens(word, Comment, token = "words") %>% 
  anti_join(stop_words)

affin2 <- affin %>%
  group_by(`Select your course.`) %>%
  summarise(word = last(word)) %>%
  slice(-c(5, 10, 19, 20)) %>%
  mutate(word = "loved") %>%
  bind_rows(affin) %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(`Select your course.`, word) %>%
  summarize(
    n = n(),
    contribution = sum(value),
    value = unique(value)
  ) %>%
  group_by(`Select your course.`) %>% 
  mutate(contr_rel = value * n / sum(n)) %>% 
  arrange(`Select your course.`, -abs(contribution)) %>% 
  group_by(`Select your course.`) %>%
  mutate(contribution_love = if_else(str_detect(word, "love"), 0, contr_rel)) %>% 
  arrange(-contribution_love) %>% 
  mutate(rank = row_number()) %>% 
  mutate(line = if_else(word == "love" | word == "loved", rank, NA_integer_)) %>% 
  ungroup() %>% 
  mutate(
    word = str_to_upper(word),
    group = case_when(
      contribution_love == 0 ~ "love",
      contribution_love > 0 ~ "positive", 
      contribution_love < 0 ~ "negative"
    ),
    course_lab = glue::glue("{str_wrap(`Select your course.`, width = 10)}"),
    course_lab = fct_reorder(course_lab, `Select your course.`)
  ) %>%
  mutate(`Select your course.` = as.numeric(as.factor(`Select your course.`))) %>%
  mutate(`Select your course.` = as.numeric(factor(`Select your course.`, levels = c(1, 20, 5, 14, 16, 12, 18, 2, 7, 13, 8, 10, 19, 4, 6, 11, 15, 3)))) %>%
  drop_na(`Select your course`)

affin2$word[1] <- "GOOD"
affin2$course_lab <- factor(affin2$course_lab, levels = c("Accelerating\nLearning in Math", 
       "Nebraska IM-PL\nFellowship Virtual\nSeries", 
       "IM Inquiry Cycle\nIII: Facilitating\nMath Discourse", 
       "Math Curriculum\nFlexible: Day 5\nEliciting Student\nThinking through\nQuestioning Inquiry\nCycle 1 Close", 
       "Math Curriculum\nFlexible: Day 6\nMaking Math Visible\nInquiry Cycle 2 Open", 
       "Math Curriculum\nFlexible: Day 3\nEliciting Student\nThinking through\nQuestioning Inquiry\nCycle 1 Open", 
       "Math Curriculum\nFlexible: Day 8\nStudent Discourse\nIC3 Open", 
       "Illustrative\nMathematics Bootcamp", 
       "IM Lab Leaders\nSession 3", 
       "Math Curriculum\nFlexible: Day\n4 Observing and\nCoaching Grounded in\nthe IPG", 
       "IM School Leaders\nModule 3", 
       "Math Curriculum\nFlexible Day 1", 
       "Math Curriculum\nFlexible: Day 9\nStudent Discourse\nIC3 Close", 
       "IM Inquiry Cycle II:\nMaking Math Visible", 
       "IM Inquiry Cycle\nIV: Checking for\nUnderstanding", 
       "Math Curriculum\nFlexible Day 2", 
       "Math Curriculum\nFlexible: Day 6", 
       "IM Inquiry Cycle I:\nEliciting Student\nThinking"))

# Reorder Numbers 

affin2 %>% 
  ggplot(aes(`Select your course.`, rank,
             label = word, 
             color = group,
             size = abs(contr_rel))) +
  geom_line(data = affin2 %>% filter(str_detect(word, "LOVE|LOVED")),
            aes(`Select your course.`, rank),
            inherit.aes = F,
            color = "white") +
  ggtext::geom_richtext(family = "Calibri",
                        fontface = "bold",
                        fill = "grey7",
                        label.color = NA,
                        label.padding = unit(c(.1, .15, -.1, .05), "lines"),
                        label.r = unit(0.05, "lines")) +
  scale_x_continuous(expand = c(.03, .03),
                     position = "top",
                     breaks = 1:18,
                     labels = levels(affin2$course_lab)[-c(19, 20)]) +
  scale_y_continuous(expand = c(.01, .01)) +
  scale_color_manual(values = c("white", "#c3573f", "#04ABEB"),
                     guide = F) +
  scale_radius(range = c(3, 12),
               guide = F) +
  labs(
    title = 'TEACHING LAB MATH COURSES ARE <b style="color:white;">LOVED</b>,  AN ORGANIZED WORDCLOUD',
    subtitle = '<span style="font-size:15pt;"><br><br>Based on a sentiment analysis of user reviews, each "word stripe" shows the words that contributed the most<br>to each course, either in a <b style="color:#04abeb;">positive</b> or in a <b style="color:#c3573f;">negative</b> way. The size of each word indicates its contribution per course.</span>',
    caption = '© Teaching Lab, 2021'
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(color = "grey60"),
        plot.background = element_rect(fill = "grey7", color = "grey7"),
        plot.margin = margin(30, 80, 10, 50),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        axis.text.x.top = element_text(color = "grey55", 
                                       size = 8.5, 
                                       face = "bold",
                                       margin = margin(b = 9), lineheight = 1),
        plot.title = element_markdown(size = 36, family = "Calibri",
                                      face = "bold", hjust = .5, margin = margin(25, 0, 15, 0),
                                      color = "grey75"),
        plot.subtitle = element_markdown(lineheight = .9,
                                         margin = margin(b = 30),
                                         hjust = 0.5,
                                         size = 20,
                                         face = "bold",
                                         color = "grey55"),
        plot.caption = element_text(size = 14, margin = margin(t = 20, b = 10), color = "grey55", hjust = 0.5, face = "bold"))


ggsave(here::here("Images/Wordcloudalt2.pdf"), 
       width = 23, height = 14, device = cairo_pdf)












