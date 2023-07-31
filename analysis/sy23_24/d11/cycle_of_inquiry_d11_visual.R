library(qualtRics)
library(tidyverse)

cycle_of_inquiry_self_reported <- qualtRics::fetch_survey("SV_2mCAWxvQuy2RnBY",
                                                          include_display_order = FALSE)

plot_data <- cycle_of_inquiry_self_reported |>
  filter(Finished == TRUE) |>
  select(contains("t_"), contains("sl_")) |>
  TeachingLab::relabel_qualtrics_df() |>
  pivot_longer(everything()) |>
  drop_na(value) |>
  mutate(prepost = ifelse(str_detect(name, "Before"), "Pre", "Post")) |>
  group_by(name, value, prepost) |>
  count(sort = T) |>
  ungroup() |>
  group_by(name, prepost) |>
  mutate(percent = 100*(n/sum(n))) |>
  ungroup() |>
  mutate(name = str_remove_all(name, "Please rate your confidence level in engaging in the following practices, reflecting on before yo\\.\\.\\. - Before Cycle of Inquiry - "),
         name = str_remove_all(name, "Please rate your confidence level in engaging in the following practice, reflecting on before you\\.\\.\\. - Before Cycle of Inquiry - "),
         name = str_remove_all(name, "Please rate your confidence level in engaging in the following practices, reflecting on before yo\\.\\.\\. - After Cycle of Inquiry - "),
         name = str_remove_all(name, "Please rate your confidence level in engaging in the following practice, reflecting on before you\\.\\.\\. - After Cycle of Inquiry - "),
         name = str_remove_all(name, "Please compare teachers that have participated in Teaching Lab PL and those who have not\\."),
         name = str_remove_all(name, "How oft\\.\\.\\. - "),
         name = str_wrap(name, width = 35),
         prepost = factor(prepost, levels = c("Pre", "Post")))

plot_data |>
  filter(str_detect(name, "could")) |>
  mutate(value = factor(value, levels = rev(c("Not at all confident",
                                              "Not very confident",
                                              "Somewhat confident",
                                              "Fairly confident",
                                              "Very confident")))) |>
  ggplot(aes(x = name, y = percent, group = value)) +
  geom_col(aes(fill = value)) +
  geom_text(aes(label = paste0(percent, "%\n(n = ", n, ")"),
                color = ifelse(value %in% c("Not at all confident", "Not very confident"), "white", "black")),
            size = 7,
            position = position_stack(vjust = 0.5), fontface = "bold",
            show.legend = FALSE) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(x = "", y = "", title = "Please rate your confidence level in engaging in the following practices") +
  coord_flip() +
  facet_wrap( ~ prepost) +
  scale_fill_manual(values = rev(tl_palette(n = 5, color = "blue"))) +
  scale_color_manual(values = c("black", "white")) +
  theme_tl() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 22),
        strip.text = element_text(hjust = 0.5, face = "bold", size = 20),
        axis.text.y = element_text(size = 20),
        legend.text = element_text(size = 18))

ggsave("~/Downloads/image.png",
       width = 22,
       height = 12, 
       bg = "white")

plot_data |>
  filter(!str_detect(name, "could")) |>
  mutate(value = factor(value, levels = rev(c("Not at all confident",
                                              "Not very confident",
                                              "Somewhat confident",
                                              "Fairly confident",
                                              "Very confident")))) |>
  ggplot(aes(x = name, y = percent, group = value)) +
  geom_col(aes(fill = value)) +
  geom_text(aes(label = paste0(percent, "%\n(n = ", n, ")"),
                color = ifelse(value %in% c("Not at all confident", "Not very confident"), "white", "black")),
            size = 7,
            position = position_stack(vjust = 0.5), fontface = "bold",
            show.legend = FALSE) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(x = "", y = "", title = "Please rate your confidence level in engaging in the following practices") +
  coord_flip() +
  facet_wrap( ~ prepost) +
  scale_fill_manual(values = rev(tl_palette(n = 5, color = "blue"))) +
  scale_color_manual(values = c("black", "white")) +
  theme_tl() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 22),
        strip.text = element_text(hjust = 0.5, face = "bold", size = 20),
        axis.text.y = element_text(size = 20),
        legend.text = element_text(size = 18))
  
  
coaching_cycle <- read_sheet("https://docs.google.com/spreadsheets/d/10yqmNypa1w99mWVSagYLT9SjQe4NOo1GpPA5soqsRAs/edit#gid=680706047")

coaching_cycle |>
  view()
