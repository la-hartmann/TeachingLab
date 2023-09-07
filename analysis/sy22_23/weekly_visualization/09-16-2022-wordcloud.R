library(TeachingLab)
library(tidyverse)
library(ggwordcloud)

data("love_words_small")

wordcloud_data <- readr::read_csv("~/Downloads/dashboard-export-10-58-pm-2022-09-15.csv") |>
  mutate(angle = 45 * sample(-2:2, n(), replace = TRUE, prob = c(1, 1, 4, 1, 1)))

ggplot(
  wordcloud_data,
  aes(
    label = `Q15 - What went well in today’s session?`, size = Count,
    color = Count, angle = angle
  )
) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 24) +
  theme_minimal() +
  scale_color_gradient(low = "#040404", high = "#04abeb")

ggsave(here::here("images/quotes/went_well_quotes.png"),
       bg = "white",
       dpi = 500,
       units = "px",
       width = 1920*5,
       height = 1080*5)
