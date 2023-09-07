beg_end_compare <- tibble::tibble(percent = c(59, 71, 36, 57, 47, 57),
                                  statement = c(rep("% Agreeing that the adopted curriculum materials offered students high quality opportunities to learn", 2),
                                                rep("% Agreeing that the adopted curriculum materials are well-organized and easy to use", 2),
                                                rep("% Agreeing that they like the adopted curriculum materials", 2)),
                                  bef_after = c("Beginning of Year", "End of Year",
                                                "Beginning of Year", "End of Year",
                                                "Beginning of Year", "End of Year"))

beg_end_compare |>
  mutate(statement = str_wrap(statement, 30)) |>
  ggplot(aes(x = statement, y = percent, fill = bef_after)) +
  geom_col(position = position_dodge()) +
  geom_text(aes(label = paste0(percent, "%")), vjust = -1, position = position_dodge(width = 1), fontface = "bold") +
  scale_fill_manual(values = tl_palette(color = "blue", n = 2)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
  guides(fill = guide_legend(override.aes = list(size=10))) +
  labs(title = "Views of the curriculum before and after engaging in Teaching Lab coaching") +
  theme_tl(legend = TRUE) +
  theme(legend.position = c(0.85, 0.85),
        legend.title = element_blank(),
        legend.text = element_text(size = 13),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_blank(),
        plot.title = element_text(face = "bold"))

ggsave("~/Downloads/new_image.png",
       bg = "white",
       width = 11, height = 10)
