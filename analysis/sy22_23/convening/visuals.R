library(tidyverse)
library(ggforce)
library(patchwork)

data <- tibble::tibble(
  site = c(
    rep("Aggregate", 2),
    rep("Rochester", 2),
    rep("New Mexico (6-8)", 2),
    rep("New Mexico (9-12)", 2),
    rep("Mississippi", 2)
  ),
  prepost = rep(c("Pre", "Post"), 5),
  percent = c(35, 57, 37, 41, 24, 70, 49, 65, 31, 53)
) |>
  dplyr::mutate(site = factor(site, levels = c(
    "Aggregate",
    "Mississippi",
    "New Mexico (6-8)",
    "New Mexico (9-12)",
    "Rochester"
  ))) |>
  dplyr::arrange(site) |>
  mutate(color = ifelse(prepost == "Pre",
    "#040404",
    "#04ABEB"
  ))

data_pivot <- data |>
  pivot_wider(!color, names_from = "prepost", values_from = "percent") |>
  mutate(color = "#04abeb")

p1 <- data |>
  ggplot(aes(x = percent, y = factor(site), color = prepost)) +
  geom_text(aes(label = paste0(percent, "%")),
    color = data$color,
    nudge_y = 0.15,
    size = 5
  ) +
  ggforce::geom_link(
    data = data_pivot,
    aes(
      x = Pre + 0.1, xend = Post - 0.25, y = site, yend = site,
      color = color, alpha = stat(index) / 2, size = stat(index)
    ),
    color = "#04abeb",
    lineend = "butt",
    show.legend = F
  ) +
  geom_point(color = data$color, size = 5) +
  labs(title = "Percent of students who demonstrated proficiency on grade-level tasks") +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  scale_y_discrete(limits = rev) +
  TeachingLab::theme_tl() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 18),
    legend.position = "bottom"
  )

p2 <- ggplot(data, aes(x = percent, y = site, color = factor(prepost, levels = c("Pre", "Post")))) +
  geom_point() +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_color_manual(values = c("#040404", "#04ABEB")) +
  TeachingLab::theme_tl() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 20))

legend_plot <- cowplot::get_legend(p2)

p1 + 
  inset_element(legend_plot, 0.9, 0.9, 0.9, 1)

ggsave(here::here("images/sy22_23/Convening/student_work_scores1.png"),
       bg = "white",
       width = 1500*5,
       height = 800*5,
       dpi = 500,
       units = "px")
