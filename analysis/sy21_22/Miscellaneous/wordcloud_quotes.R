library(TeachingLab)
library(tidyverse)

session_survey <- get_session_survey()
course_survey <- get_course_survey()

quotes1 <- session_survey %>%
  select(12:14) %>%
  pivot_longer(everything())

quotes2 <- course_survey %>%
  select(6:10) %>%
  pivot_longer(everything())

all_quotes <- quotes1 %>%
  bind_rows(quotes2)


all_quotes %>%
  TeachingLab::tl_wordcloud(text_col = value,
                            color = c("#032533", "#024762", "#016891", "#008AC0", "#00ACF0"))

ggsave(here::here("images/wordclouds/all_quotes.png"),
       width = 10,
       height = 10,
       bg = "white",
       units = "in")

all_quotes %>%
  TeachingLab::tl_wordcloud(text_col = value)

ggsave(here::here("images/wordclouds/all_quotes.png"),
       width = 1920*5,
       height = 1080*5,
       dpi = 500,
       bg = "white",
       units = "px")

knitr::plot_crop(here::here("images/wordclouds/all_quotes.png"))
  
all_quotes %>%
  dplyr::filter(name == "What went well in today’s session?") %>%
  TeachingLab::tl_wordcloud(text_col = value,
                            size = 40,
                            color = TeachingLab::tl_palette(color = "orange")[c(1, 6)])

ggsave(here::here("images/wordclouds/quotes_went_well.png"),
       width = 1920*5,
       height = 1080*5,
       dpi = 500,
       bg = "white",
       units = "px")

knitr::plot_crop(here::here("images/wordclouds/quotes_went_well.png"))

all_quotes %>%
  dplyr::filter(name == "What could have been better about today’s session?") %>%
  TeachingLab::tl_wordcloud(text_col = value,
                            color = TeachingLab::tl_palette(color = "purple")[c(2, 6)],
                            size = 75)

ggsave(here::here("images/wordclouds/been_better.png"),
       width = 1920*5,
       height = 1080*5,
       dpi = 500,
       bg = "white",
       units = "px")
knitr::plot_crop(here::here("images/wordclouds/been_better.png"))
