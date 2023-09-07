library(qualtRics)
library(TeachingLab)
library(tidyverse)

participant_feedback <- fetch_survey(surveyID = "SV_djt8w6zgigaNq0C", 
                                     verbose = TRUE)

column_names <- participant_feedback |>
  select(contains("Q8")) |>
  map_chr( ~ attr(.x, "label"))

participant_feedback |>
  filter(!is.na(Q7)) |>
  select(contains("Q8")) |>
  pivot_longer(everything()) |>
  drop_na(value) |>
  group_by(name, value) |>
  count() |>
  ungroup() |>
  group_by(name) |>
  mutate(percent = 100 * (n/sum(n)),
         name = str_wrap(str_replace_all(name, column_names), width = 20)) |>
  ggplot(aes(x = name, y = percent, fill = value)) +
  geom_col(position = position_stack(vjust = 0.5)) +
  geom_text(aes(label = paste0(round(percent, 2), "%")),
            position = position_stack(vjust = 0.5)) +
  coord_flip() +
  guides(fill = guide_legend(reverse = T)) +
  theme_tl(legend = T) +
  theme(legend.position = "bottom",
        legend.title = element_blank())

ggsave(here::here("images/qualtrics/demo_image.png"),
       bg = "white",
       width = 15,
       height = 15)
