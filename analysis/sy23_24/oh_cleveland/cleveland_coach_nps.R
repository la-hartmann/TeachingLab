course_survey |>
  filter(str_detect(site, "OH_Cleveland")) |>
  mutate(month = lubridate::month(RecordedDate, label = TRUE, abbr = FALSE)) |>
  group_by(month) |>
  summarise(nps = TeachingLab::calc_nps(nps)) |>
  mutate(month = factor(month, levels = c("August", "September", "October", "November", "December",
                                          "January", "February", "March", "May", "June"))) |>
  ggplot(aes(x = month, y = nps)) +
  geom_line(group = 1) +
  labs(title = "Cleveland NPS 2022-2023", subtitle = "Overall average of 63.1") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_tl() +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "bold"))

ggsave("~/Downloads/plot1.png",
       bg = "white",
       height = 10, width = 12)
