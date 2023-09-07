participant_feedback$Q35_NPS_GROUP |>
   table()

median(participant_feedback$Q35, na.rm = T)


participant_feedback |>
  mutate(week = lubridate::week(RecordedDate)) |>
  group_by(week) |>
  summarise(nps = mean(Q35, na.rm = T),
            date = RecordedDate) |>
  ggplot(aes(x = date, y = nps)) +
  geom_line(size = 2, color = "#04abeb") +
  geom_smooth(se = F, color = "#04abeb", linewidth = 0.5, linetype = "dashed") +
  scale_y_continuous(limits = c(0, 10), n.breaks = 6) +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b") +
  labs(x = "", y = "How likely are you to recommend this PL to a friend or colleague?", title = "Teaching Lab median NPS Rating 2022-2023",
       subtitle = "152 Promoters out of 310 Reviewers, with an overall median of 8") +
  theme_tl() +
  theme(plot.subtitle = element_text(hjust = 0.5))


ggsave("~/Downloads/nps_teachinglab.png",
       bg = "white",
       height = 10,
       width = 10)
