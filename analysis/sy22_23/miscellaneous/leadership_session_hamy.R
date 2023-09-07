library(tidyverse)
library(ggforce)

minard_data <- tibble::tibble(
  lat = c(5, 4, 10, 14, 18, 14, 8),
  long = c(10, 20, 25, 27, 23, 10, 5),
  size = c(1, 2, 3, 3, 4, 4, 4),
  color = c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#f26419", "#04abeb", "#040404"),
  label = c("Birth", "School\n1, 2, 3", "Reading", "Technology", "College", "Work", "Travel")
)

ggplot(minard_data, aes(x = lat, y = long, color = color)) +
  geom_path(aes(size = size, group = 1),
            lineend = "round",
            arrow = arrow()) +
  geom_text(aes(label = label), vjust = -1, size = 10, family = "Tisa Sans Pro",
            color = "black") +
  scale_color_identity() +
  coord_cartesian(clip = "off") +
  # DGThemes::theme_duncan() +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = margin(50, 30, 30, 30))

ggsave(here::here("images/random_life_plot.png"),
       bg = "white",
       width = 10,
       height = 10)
