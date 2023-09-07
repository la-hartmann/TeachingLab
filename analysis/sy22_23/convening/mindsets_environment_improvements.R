library(ggtext)
library(ggforce)
library(TeachingLab)
library(tidyverse)

### Visual 1: High expectations and Beliefs ###
### Initial Idea - a comparison bar chart ###

expectations_beliefs <- tibble::tibble(
  Pre = c(40, 64, 76),
  Post = c(65, 72, 83),
  Site = c("Delaware Department of Education, DE", 
           "Rochester City School District", 
           "Cleveland Metropolitan School District, OH")
) |>
  pivot_longer(!Site, names_to = "prepost", values_to = "percent") |>
  mutate(prepost = factor(prepost, levels = c("Pre", "Post")),
         Site = TeachingLab::html_wrap(Site, n = 15))

pivot_expectations_belief <- expectations_beliefs |>
  pivot_wider(names_from = prepost, values_from = percent) |>
  mutate(Improvement = Post - Pre)

ggplot(expectations_beliefs, aes(x = reorder(Site, rev(percent)), y = percent, fill = prepost)) +
  geom_col(position = position_dodge2(reverse = T)) +
  geom_text(aes(label = paste0(percent, "%"),
                color = prepost),
            position = position_dodge2(reverse = T,
                                       width = 1), 
            hjust = 1.25,
            size = 5,
            fontface = "bold") +
  geom_richtext(data = pivot_expectations_belief, aes(x = Site, y = Post + 10, 
                                                  label = paste0("<br>+", Improvement, "%<br>")),
            color = "#04ABEB",
            fill = "white",
            fontface = "bold",
            size = 11,
            show.legend = F,
            label.r = unit(1.5, "lines"),
            label.padding = unit(c(0.05, 0.15, 0.05, 0.15), "lines"),
            label.size = unit(1, "cm"),
            lineheight = 1.025
            ) +
  labs(x = "", y = "", title = "Standout Improvements in High Expectations and Beliefs") +
  coord_flip() +
  scale_fill_manual(values = c("Pre" = "#040404", "Post" = "#04ABEB")) +
  scale_color_manual(values = c("Post" = "#040404", "Pre" = "#04ABEB")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
  guides(color = "none") +
  TeachingLab::theme_tl() +
  theme(legend.position = c(0.9, 0.95),
        legend.title = element_blank(),
        axis.text.y = ggtext::element_markdown(lineheight = 1.1, size = 20, family = "Calibri"),
        plot.title = element_text(family = "Calibri Bold"),
        legend.key.size = unit(1, "cm"))

ggsave(here::here("images/Convening/high_expectations_standouts.png"),
       bg = "white",
       width = 10,
       height = 10)

### Visual 2: Recognition of Race & Culture ###

race_culture <- tibble::tibble(
  Pre = c(53, 62),
  Post = c(60, 70),
  Site = c("Delaware Department of Education, DE", 
           "San Diego Unified School District, CA")
) |>
  pivot_longer(!Site, names_to = "prepost", values_to = "percent") |>
  mutate(prepost = factor(prepost, levels = c("Pre", "Post")),
         Site = TeachingLab::html_wrap(Site, n = 15))

pivot_race_culture <- race_culture |>
  pivot_wider(names_from = prepost, values_from = percent) |>
  mutate(Improvement = Post - Pre)

ggplot(race_culture, aes(x = reorder(Site, rev(percent)), y = percent, fill = prepost)) +
  geom_col(position = position_dodge2(reverse = T)) +
  geom_text(aes(label = paste0(percent, "%"),
                color = prepost),
            position = position_dodge2(reverse = T,
                                       width = 1), 
            hjust = 1.25,
            size = 5,
            fontface = "bold") +
  geom_richtext(data = pivot_race_culture, aes(x = Site, y = Post + 10, 
                                                      label = paste0("<br>+", Improvement, "%<br>")),
                color = "#04ABEB",
                fill = "white",
                fontface = "bold",
                size = 11,
                show.legend = F,
                label.r = unit(1.5, "lines"),
                label.padding = unit(c(0.05, 0.15, 0.05, 0.15), "lines"),
                label.size = unit(1, "cm"),
                lineheight = 1.025
  ) +
  labs(x = "", y = "", title = "Standout Improvements in Recognition of Race & Culture") +
  coord_flip() +
  scale_fill_manual(values = c("Pre" = "#040404", "Post" = "#04ABEB")) +
  scale_color_manual(values = c("Post" = "#040404", "Pre" = "#04ABEB")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
  guides(color = "none") +
  TeachingLab::theme_tl() +
  theme(legend.position = c(0.9, 0.95),
        legend.title = element_blank(),
        axis.text.y = ggtext::element_markdown(lineheight = 1.1, size = 20, family = "Calibri"),
        plot.title = element_text(family = "Calibri Bold"),
        legend.key.size = unit(1, "cm"))

ggsave(here::here("images/Convening/race_culture_standouts.png"),
       bg = "white",
       width = 10,
       height = 10)


### Visual 3: Self Efficacy in Crse Practices ###

self_efficacy <- tibble::tibble(
  Pre = c(69, 62, 62, 72),
  Post = c(85, 76, 72, 81),
  Site = c("Connecticut Partnership (with UnboundEd)", 
           "Calcasieu Parish, LA", 
           "San Diego Unified School District, CA",
           "Rochester City School District - District-wide")
) |>
  pivot_longer(!Site, names_to = "prepost", values_to = "percent") |>
  mutate(prepost = factor(prepost, levels = c("Pre", "Post")),
         Site = TeachingLab::html_wrap(Site, n = 15), 
         fake_num = c(1, 1, 2, 2, 3, 3, 4, 4))

pivot_self_efficacy <- self_efficacy |>
  select(-fake_num) |>
  pivot_wider(names_from = prepost, values_from = percent) |>
  mutate(Improvement = Post - Pre)

ggplot(self_efficacy, aes(x = fct_reorder(Site, fake_num, .desc = T), y = percent, fill = prepost)) +
  geom_col(position = position_dodge2(reverse = T)) +
  geom_text(aes(label = paste0(percent, "%"),
                color = prepost),
            position = position_dodge2(reverse = T,
                                       width = 1), 
            hjust = 1.25,
            size = 5,
            fontface = "bold") +
  geom_richtext(data = pivot_self_efficacy, aes(x = Site, y = Post + 10, 
                                                      label = paste0("<br>+", Improvement, "%<br>")),
                color = "#04ABEB",
                fill = "white",
                fontface = "bold",
                size = 11,
                show.legend = F,
                label.r = unit(1.5, "lines"),
                label.padding = unit(c(0.05, 0.15, 0.05, 0.15), "lines"),
                label.size = unit(1, "cm"),
                lineheight = 1.025
  ) +
  labs(x = "", y = "", title = "Standout Improvements in Self-efficacy in CRSE Practices") +
  coord_flip() +
  scale_fill_manual(values = c("Pre" = "#040404", "Post" = "#04ABEB")) +
  scale_color_manual(values = c("Post" = "#040404", "Pre" = "#04ABEB")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
  guides(color = "none") +
  TeachingLab::theme_tl() +
  theme(legend.position = c(0.8, 0.95),
        legend.title = element_blank(),
        axis.text.y = ggtext::element_markdown(lineheight = 1.1, size = 20, family = "Calibri"),
        plot.title = element_text(family = "Calibri Bold"),
        legend.key.size = unit(1, "cm"))

ggsave(here::here("images/Convening/self_efficacy_standouts.png"),
       bg = "white",
       width = 15,
       height = 10)

### Visual 4: Self Efficacy in Crse Practices ###

school_climate <- tibble::tibble(
  Pre = c(38, 52, 48, 64),
  Post = c(70, 84, 76, 80),
  Site = c("I have influence over the professional learning that I receive through my school or district",
           "I am confident that I am implementing the curriculum in a way that maximizes positive impact for student learning",
           "I feel connected to my fellow teachers in the school",
           "I trust my fellow teachers in the school")
) |>
  pivot_longer(!Site, names_to = "prepost", values_to = "percent") |>
  mutate(prepost = factor(prepost, levels = c("Pre", "Post")),
         Site = TeachingLab::html_wrap(Site, n = 15), 
         fake_num = c(1, 1, 2, 2, 3, 3, 4, 4)) |>
  slice(5:8)

pivot_school_climate <- school_climate |>
  select(-fake_num) |>
  pivot_wider(names_from = prepost, values_from = percent) |>
  mutate(Improvement = Post - Pre)

ggplot(school_climate, aes(x = fct_reorder(Site, fake_num, .desc = T), y = percent, fill = prepost)) +
  geom_col(position = position_dodge2(reverse = T)) +
  geom_text(aes(label = paste0(percent, "%"),
                color = prepost),
            position = position_dodge2(reverse = T,
                                       width = 1), 
            hjust = 1.25,
            size = 5,
            fontface = "bold") +
  geom_richtext(data = pivot_school_climate, aes(x = Site, y = Post + 10, 
                                                label = paste0("<br>+", Improvement, "%<br>")),
                color = "#04ABEB",
                fill = "white",
                fontface = "bold",
                size = 11,
                show.legend = F,
                label.r = unit(1.5, "lines"),
                label.padding = unit(c(0.05, 0.15, 0.05, 0.15), "lines"),
                label.size = unit(1, "cm"),
                lineheight = 1.025
  ) +
  labs(x = "", y = "", title = "Cleveland School Culture & Climate Standouts") +
  coord_flip() +
  scale_fill_manual(values = c("Pre" = "#040404", "Post" = "#04ABEB")) +
  scale_color_manual(values = c("Post" = "#040404", "Pre" = "#04ABEB")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
  guides(color = "none") +
  TeachingLab::theme_tl() +
  theme(legend.position = c(0.9, 0.95),
        legend.title = element_blank(),
        axis.text.y = ggtext::element_markdown(lineheight = 1.1, size = 20, family = "Calibri"),
        plot.title = element_text(family = "Calibri Bold"),
        legend.key.size = unit(1, "cm"))

ggsave(here::here("images/Convening/cleveland_school_climate_standouts.png"),
       bg = "white",
       width = 15,
       height = 10)

