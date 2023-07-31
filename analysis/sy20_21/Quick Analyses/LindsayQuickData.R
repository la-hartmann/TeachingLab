library(here)
library(tidyverse)
devtools::load_all()

library(TeachingLab)
library(ggtext)

data <- read_csv(here("Data/SY20-21/SurveyMonkey/SY21-22 End-of-Course Participant Feedback Survey.csv"), skip = 1) %>%
  select(c(34:42))


data_plot <- data %>% 
  pivot_longer(everything(), names_to = "Question", values_to = "Response") %>%
  drop_na() %>%
  group_by(Question, Response) %>%
  count() %>%
  ungroup() %>%
  group_by(Question) %>%
  mutate(Question = html_wrap(Question, interval = 30)) %>%
  summarise(n = n,
            Response = Response,
            Percent = round(n/sum(n)*100))

ggplot(data = data_plot, aes(x = Question, y = Percent, fill = factor(Response))) +
  geom_col() +
  geom_text(aes(label = if_else(Percent != 4, paste0(Percent, "%"), "")), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = tl_palette(color = "blue", theme = "dark", n = length(unique(data_plot$Response)))) +
  labs(fill = "", title = "Percent that Agree/Strongly Agree with<br>Each of the Following Statements",
       x = "", y = "") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_tl(legend = T) +
  theme(axis.text.y = element_markdown(lineheight = 1.1, size = 12),
        axis.text.x = element_text(size = 11),
        legend.position = "bottom",
        plot.title = element_markdown(lineheight = 1.1))

ggsave(here("Images/2020-2021/CourseSummary.png"), width = 16, height = 10)
