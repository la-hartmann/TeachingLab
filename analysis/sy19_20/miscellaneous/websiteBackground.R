library(tidyverse)

set.seed(77)

color <- colorRampPalette(c("gray90", "gray10"))

n <- 10000
x <- runif(n)
y <- runif(n)

for (i in seq(0, 2 * pi, by = 0.2)) {
  a = x * sin(i) - y * cos(i)
  for (j in 1:12) {
    c = y * sin(i) - x * sin(j)
    for (k in 1:5) {
      s = c + x * sin(j)
      xend <- x + y * cos(2 * k)
      yend <- y - x * sin(2 * k)
    }
  }
}

ggplot(data = tibble(x = rnorm(n = 1000, mean = 100, sd = 3), y = rnorm(n = 1000, mean = 0, sd = 20), width = 0.5 - s, height = 0.5 + s, fill = color(1000), alpha = a), 
       aes(x = x, y = y, width = width, height = height, fill = fill, alpha = alpha)) +
  scale_fill_identity() +
  scale_alpha_continuous(range = c(0, 0.1)) +
  geom_tile() +
  coord_cartesian(clip = "off", expand = FALSE) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "gray10", color = NA)) +
  ggsave(here::here("Images", "website_background.png"), dpi = 700, width = 22, height = 14)

# ggplot() +
#   geom_tile(aes(x, y, alpha = a, width = 0.5 - s, height = 0.5 + s, fill = factor(c))) +
#   geom_tile(aes(y, x, alpha = a, width = 0.5 - c, height = 0.5 + a, fill = factor(c))) +
#   scale_fill_manual(values = c(color(1000))) +
#   scale_alpha_continuous(range = c(0, 0.17)) +
#   coord_cartesian(clip = "off", expand = FALSE) +
#   theme_void() +
#   theme(
#     legend.position = "none",
#     plot.background = element_rect(fill = "#8c92ac", color = NA)
#   ) +
#   ggsave(here::here("Images", "website_background.png"), dpi = 700, width = 22, height = 14)

color <- colorRampPalette(c("#b5ffe1","#93e5ab","#65b891","#4e878c","#00241b"))

ggplot(data = tibble(x = runif(n = 10000, 0, 100), y = rnorm(n = 10000, mean = 0, sd = 3), width = 0.5 - s, height = 0.5 + s, fill = color(10000), alpha = a), 
       aes(x = x, y = y, width = width, height = height, fill = fill, alpha = alpha)) +
  scale_fill_identity() +
  scale_alpha_continuous(range = c(0, 1)) +
  geom_tile() +
  coord_cartesian(clip = "off", expand = FALSE) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "gray30", color = NA)) #+
  # ggsave(here::here("Images", "colorful_background.png"), dpi = 700, width = 22, height = 14)

