library(ggraph)
library(gt)
library(tidyverse)
library(tidytext)

session_survey <- get_session_survey()

session_responses <- session_survey |>
  select(went_well_today, been_better_today, take_back_class) |>
  pivot_longer(everything(), names_to = "Topic", values_to = "Response") |>
  drop_na(Response) |>
  filter(!Response %in% na_df) |>
  mutate(Topic = str_replace_all(Topic, c("been_better_today" = "What could have been better today?",
                                          "went_well_today" = "What went well today?",
                                          "take_back_class" = "What is one thing from today's learning that you plan to take back to your classroom?")))

session_word_count <- session_responses |>
  unnest_tokens(output = word,
                input = Response) |>
  anti_join(stop_words) |>
  group_by(Topic, word) |>
  count(sort = T) |>
  ungroup()

session_word_count |>
  filter(n > 20) |>
  ggplot(aes(x = reorder(word, n),
             y = n)) +
  geom_col() +
  coord_flip() +
  facet_wrap( ~ Topic, scales = "free_y") +
  theme_tl()

wordcloud::wordcloud(words = session_word_count$word,
                     freq = session_word_count$n,
                     random.order = FALSE,
                     scale = c(2, 0.5),
                     min.freq = 1,
                     max.words = 100,
                     colors = c("#6FA8F5",
                                "#FF4D45",
                                "#FFC85E")
)

synopsis <- session_responses %>%
  unnest_tokens(bigram, Response, token = "ngrams", n = 2) %>%
  select(bigram) |>
  drop_na()

synopsis %>% count(bigram, sort = TRUE)

bigram_split <- synopsis %>%
  separate(col = bigram,
           into = c("word1", "word2"),
           sep = " ")

bigram_cleaned <-
  bigram_split %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_cleaned <- bigram_cleaned %>%
  count(word1, word2, sort = TRUE)

bigram_cleaned %>%
  unite(col = bigram,
        word1, word2,
        sep = " ")

set.seed(1234)

# Create the special igraph object
graph <- bigram_cleaned %>%
  filter(n > 3) %>%
  igraph::graph_from_data_frame()

# Plot the network graph
graph %>%
  ggraph(layout = "kk") +  # Choose a layout
  geom_edge_link(aes(col = factor(n),
                     alpha = after_stat(index))) +     # Draw lines between nodes
  geom_node_point() +
  geom_node_text(aes(label = name),
                 vjust = 1,
                 hjust = 1) +
  theme_tl() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

ggsave(here::here("Images/session_feedback/network_diagram_session.png"),
       bg = "white",
       width = 10, height = 10)

session_survey1 <- session_survey |>
  select(facilitator1, fac_feedback_2, fac_feedback_1, fac_feedback_3, fac_feedback_4, fac_feedback_5) |>
  rename(facilitator = facilitator1)

session_survey2 <- session_survey |>
  select(facilitator2, fac_feedback_2_1, fac_feedback_2_2, fac_feedback_2_3, fac_feedback_2_4, fac_feedback_2_5) |>
  rename(facilitator = facilitator2, 
         fac_feedback_2 = fac_feedback_2_2,
         fac_feedback_1 = fac_feedback_2_1,
         fac_feedback_3 = fac_feedback_2_3,
         fac_feedback_4 = fac_feedback_2_4,
         fac_feedback_5 = fac_feedback_2_5)

session_survey_all_numeric <- bind_rows(session_survey1, session_survey2) |>
  janitor::remove_empty("rows") |>
  mutate(across(contains("fac_feedback"), ~ readr::parse_number(.x)))

session_survey_all_numeric |>
  group_by(facilitator) |>
  mutate(n = n()) |>
  summarise(across(contains("fac_feedback"), ~ mean(.x)),
            n = first(n)) |>
  ungroup() |>
  rowwise() |>
  mutate(all_time_average = mean(c_across(starts_with("fac_feedback")))) |>
  arrange(desc(all_time_average)) |>
  filter(n > 10) |>
  gt::gt() |>
  gt::tab_header(title = md("**Best Facilitator Feedback 2022-2023**"),
                 subtitle = md("*Filtered for n > 10*")) |>
  gt::cols_label(
    fac_feedback_1 = "They demonstrated deep knowledge of the content they facilitated",
    fac_feedback_2 = "They facilitated the content clearly",
    fac_feedback_3 = "They effectively built a safe learning community",
    fac_feedback_4 = "They were fully prepared for the session",
    fac_feedback_5 = "They responded to the group’s needs",
  ) |>
  data_color(
    columns = c(contains("fac_feedback"), "all_time_average"),
    palette = "ggsci::blue_material"
  ) |>
  TeachingLab::gt_theme_tl() |>
  gtsave(here::here("Images/session_feedback/fac_feedback_all_time.png"))



session_survey_qualitative_numeric <- session_survey |>
  select(fac_feedback_2,
         went_well_today) |>
  drop_na(fac_feedback_2, went_well_today) |>
  mutate(fac_feedback_2 = readr::parse_number(fac_feedback_2)) |>
  unnest_tokens(output = word,
                input = went_well_today) |>
  anti_join(stop_words) |>
  group_by(fac_feedback_2, word) |>
  count(sort = T) |>
  ungroup()

corr <- lm(fac_feedback_2 ~ n, session_survey_qualitative_numeric)
summary(corr)

corr2 <- session_survey_qualitative_numeric |>
  select(fac_feedback_2, n) |>
  correlation::correlation()

effectsize::interpret_r(corr2$r, rules = "cohen1988")

session_survey |>
  select(went_well_today, been_better_today, take_back_class,
         contains("fac_feedback")) |>
  pivot_longer(!contains("fac_feedback")) |>
  mutate(across(contains("fac_feedback"), ~ readr::parse_number(.x))) |>
  view()
  
