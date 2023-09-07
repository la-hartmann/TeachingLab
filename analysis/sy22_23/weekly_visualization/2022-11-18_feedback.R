###Tidytuesday 21/01/2020 Spotify songs
library(tidyverse)
library(scico)
library(gridExtra)
library(tidytext)
library(tm)
library(Matrix)
library(widyr)
library(glmnet)
library(qualtRics)

participant_feedback <- qualtRics::fetch_survey(surveyID = "SV_djt8w6zgigaNq0C", 
                        verbose = TRUE,
                        force_request = TRUE)

just_text_nps_feedback <- participant_feedback |>
  select(`Overall, what went well in this course?` = Q38,
         `Overall, what could have been better in this course?` = Q39,
         `What is the learning from this course that you are most excited to try out?` = Q36,
         `Which activities best supported your learning in this course?` = Q37,
         `Feel free to leave us any additional comments, concerns, or questions.` = Q40,
         NPS = Q35) |>
  janitor::remove_empty("rows") |>
  mutate(review_id = row_number())

#Extract words from album and track names
text_nps_words <- just_text_nps_feedback |>
  pivot_longer(!c(NPS, review_id), names_to = "Question", values_to = "Review") |>
  drop_na(Review) |>
  select(review_id, NPS, Review) |>
  unnest_tokens(word, Review)

text_nps_words_filtered <- text_nps_words |>
  anti_join(stop_words, by = "word") |>
  distinct(review_id, word) |>
  add_count(word) |>
  filter(n >= 5)

text_nps_words_matrix <- text_nps_words_filtered %>%
  cast_sparse(review_id, word)

review_ids <- as.integer(rownames(text_nps_words_matrix))

scores <- just_text_nps_feedback$NPS[review_ids]

cv_glmnet_model <- cv.glmnet(text_nps_words_matrix, scores)
plot(cv_glmnet_model)

###lexicon
lexicon <- cv_glmnet_model$glmnet.fit %>%
  tidy() %>%
  filter(#lambda == cv_glmnet_model$lambda.1se,
         term != "(Intercept)",
         term != "log_price") %>%
  select(word = term, coefficient = estimate) |>
  distinct(word, .keep_all = T)

###Positive words
positive <- lexicon  %>%
  arrange(coefficient) %>%
  mutate(direction = ifelse(coefficient < 0, "Negative", "Positive")) %>%
  filter(direction == "Positive") %>%
  top_n(200, abs(coefficient)) %>%
  rowid_to_column()

positive_words_labels <- positive %>%
  mutate(n = n(),
         angle = 90 - 360 * (rowid - 0.5) / n,
         hjust = ifelse(angle < -90, 0, 1),
         angle = ifelse(angle < -90, angle + 180, angle))

# spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')
# 
# spotify_songs %>%
#   ggplot(aes(x = track_popularity))+
#   geom_density()

# track_name_words <- spotify_songs %>%
#   mutate(song_id = row_number()) %>%
#   select(song_id, track_popularity, track_name) %>%
#   unnest_tokens(word, track_name) 
# 
# album_name_words <- spotify_songs %>%
#   mutate(song_id = row_number()) %>%
#   select(song_id, track_popularity, track_album_name) %>%
#   unnest_tokens(word, track_album_name) 


# track_words <- bind_rows(track_name_words, album_name_words) |>
#   anti_join(stop_words, by = "word")  |>
#   filter(!word %in% c("version", "radio", "mix", "remastered", "edition", "remaster", "vol"),
#          str_detect(word, "[a-z]"))

##filter on most frequent words
# track_words_filtered <- track_words %>%
#   distinct(song_id, word) %>%
#   add_count(word) %>%
#   filter(n >= 10)

####

# track_word_matrix <- track_words_filtered %>%
#   cast_sparse(song_id, word)
# 
# song_ids <- as.integer(rownames(track_word_matrix))
# 
# scores <- spotify_songs$track_popularity[song_ids]
# 
# cv_glmnet_model <- cv.glmnet(track_word_matrix, scores)
# plot(cv_glmnet_model)


# ###lexicon
# lexicon <- cv_glmnet_model$glmnet.fit %>%
#   tidy() %>%
#   filter(lambda == cv_glmnet_model$lambda.1se,
#          term != "(Intercept)",
#          term != "log_price") %>%
#   select(word = term, coefficient = estimate)
# 
# ###Positive words
# positive <- lexicon  %>%
#   arrange(coefficient) %>%
#   mutate(direction = ifelse(coefficient < 0, "Negative", "Positive")) %>%
#   filter(direction == "Positive") %>%
#   top_n(200, abs(coefficient)) %>%
#   rowid_to_column()
# 
# positive_words_labels <- positive %>%
#   mutate(n = n(),
#          angle = 90 - 360 * (rowid - 0.5) / n,
#          hjust = ifelse(angle < -90, 0, 1),
#          angle = ifelse(angle < -90, angle + 180, angle))

pos <- positive %>%
  mutate(rowid= factor(rowid)) %>%
  ggplot(aes(rowid, coefficient * 350)) +
  geom_col(aes(fill = coefficient), color = "#414141", show.legend = FALSE) +
  scale_y_continuous(limits = c(-110, 40), expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_scico(palette = "lisbon", direction = -1)+
  coord_polar()+
  geom_text(data = positive_words_labels,
            aes(x = factor(rowid),
                y = -1,
                label = word,
                hjust = hjust,
                angle = angle),
            color = "white",
            size = 8,
            inherit.aes = FALSE )+
  ggplot2::annotate("text", x = 0, y =-110, label = "Which words in TL surveys\nare associated with high\nNPS ratings?",
                    color = "#E5E068", size = 11)  +
  theme(plot.background = element_rect(fill = "#414141", color = "#414141"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank())

ggsave("Tidytuesday_2020_w3_positive_words.png", plot = pos, width = 14.5, height = 14.5)

###Negative words
negative <- lexicon  %>%
  arrange(coefficient) %>%
  mutate(direction = ifelse(coefficient < 0, "Negative", "Positive")) %>%
  filter(direction == "Negative") %>%
  top_n(200, abs(coefficient)) %>%
  rowid_to_column()

negative_words_labels <- negative %>%
  mutate(n = n(),
         angle = 90 - 360 * (rowid - 0.5) / n,
         hjust = ifelse(angle < -90, 1,0),
         angle = ifelse(angle < -90, angle + 180, angle))

neg <- negative %>%
  mutate(rowid= factor(rowid)) %>%
  ggplot(aes(rowid, coefficient)) +
  geom_col(aes(fill = coefficient), color = "#414141",show.legend = FALSE) +
  scale_y_continuous(limits = c(-50, 10), expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_scico(palette = "devon", begin = 0.1, end = 0.7)+
  coord_polar()+
  geom_text(  data = negative_words_labels,
              aes(x = factor(rowid),
                  y = 1,
                  label = word,
                  hjust = hjust,
                  angle = angle ),
              color = "white",
              size = 5,
              inherit.aes = FALSE) +
  ggplot2::annotate("text", x = 0, y =-50, label = "Which words in track and\nalbum names are associated\nwith low popularity?",
                    color = "#DFDDF9", size = 13)  +
  theme(plot.background = element_rect(fill = "#414141", color = "#414141"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(), 
        panel.grid = element_blank(),
        panel.background = element_blank())

ggsave("Tidytuesday_2020_w3_negative_words.png", plot = neg, width = 14.5, height = 14.5)