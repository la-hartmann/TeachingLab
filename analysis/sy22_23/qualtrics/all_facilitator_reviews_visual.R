all_facilitator_reviews |>
  ggplot(aes(x = Question, y = Percent, fill = Response)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = ifelse(Percent > 4, paste0(Percent, "%"), "")),
            position = position_stack(vjust = 0.5, reverse = TRUE),
            fontface = "bold") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = tl_palette(color = "blue", n = 5)) +
  labs(x = "", y = "", title = "October Facilitator Feedback",
       subtitle = "On average 93% agree/strongly agree!!") +
  theme_tl() +
  theme(legend.position = "bottom")

ggsave(here::here("images/weekly_visual_2.png"),
       bg = "white",
       width = 10,
       height = 10)
