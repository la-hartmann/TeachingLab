library(officer)
library(tidyverse)
library(tidytext)

sample_doc <- read_docx("~/Downloads/CTAPracticeData.docx")

content <- docx_summary(sample_doc)

### 16 Paragraphs
content$text[-c(1, 3, 4, 6, 7, 9, 10, 11, 12, 13, 15, 16)]

df_text <- tibble(text = content$text[-c(1, 3, 4, 6, 7, 9, 10, 11, 12, 13, 15, 16)])

(df_text <- df_text %>% unnest_tokens(output = word,
                                      input = text))

word_frequencies <-
  df_text %>% 
  count(word, sort = T) %>%
  anti_join(stop_words)

df_text %>% 
  count(word, sort = T) %>%
  anti_join(stop_words) %>%
  filter(n > 4) %>%
  ggplot(aes(x = reorder(word, n),
             y = n)) +
  geom_col() +
  coord_flip()

wordcloud::wordcloud(words = word_frequencies$word,
                     freq = word_frequencies$n,
                     random.order = FALSE,
                     scale = c(2, 0.5),
                     min.freq = 1,
                     max.words = 100,
                     colors = c("#6FA8F5",
                                "#FF4D45",
                                "#FFC85E")
)

cat_text <- tibble(text = content$text[-c(1, 3, 4, 6, 7, 9, 10, 12, 13, 15, 16)],
                    synopsis = c("Turnaround story", "Preparing sequence & experience",
                                 "Changing mindset for the teachers", "Data", 
                                 "Knew when approach was working"))

synopsis <- cat_text %>%
  unnest_tokens(bigram, synopsis, token = "ngrams", n = 2) %>%
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

library(ggraph)

set.seed(1234)

# Create the special igraph object
graph <- igraph::graph_from_data_frame(bigram_cleaned)

# Plot the network graph
graph %>%
  ggraph(layout = "kk") +  # Choose a layout
  geom_edge_link() +       # Draw lines between nodes
  geom_node_point() +
  geom_node_text(aes(label = name),
                 vjust = 1,
                 hjust = 1)
