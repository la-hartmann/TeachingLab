library(tidyverse)

df <- readr::read_csv("~/Downloads/Staffing Input Request - Input.csv")

df %>% 
  ggplot(aes(x = `Response Time`)) +
  geom_bar(fill = "#04abeb") +
  scale_x_date(date_minor_breaks = "1 day") +
  labs(y = "Daily Requests", x = "") +
  TeachingLab::theme_tl(axis_title_size = 20,
                        axis_text_size = 18)
ggsave(here::here("Images/Quick/staffing_usage.png"), height = 10, width = 10, bg = "white")

df %>%
  group_by(PMs, `Response Time`) %>%
  count(sort = T) %>%
  ggplot(aes(x = `Response Time`, y = n)) +
  geom_col(aes(fill = PMs)) +
  scale_x_date(date_minor_breaks = "1 day") +
  scale_y_continuous(limits = c(0, 6)) +
  labs(y = "Daily Requests", x = "Date") +
  TeachingLab::theme_tl(axis_title_size = 20,
                        axis_text_size = 18,
                        legend = T) +
  theme(legend.position = "top")

ggsave(here::here("Images/Quick/staffing_usage2.png"), height = 10, width = 10, bg = "white")

df %>%
  group_by(Curriculum, `Response Time`) %>%
  count(sort = T) %>%
  ggplot(aes(x = `Response Time`, y = n)) +
  geom_col(aes(fill = Curriculum)) +
  scale_x_date(date_minor_breaks = "1 day") +
  scale_y_continuous(limits = c(0, 6)) +
  labs(y = "Daily Requests", x = "Date") +
  TeachingLab::theme_tl(axis_title_size = 20,
                        axis_text_size = 18,
                        legend = T) +
  theme(legend.position = "top")

ggsave(here::here("Images/Quick/staffing_usage3.png"), height = 10, width = 10, bg = "white")

