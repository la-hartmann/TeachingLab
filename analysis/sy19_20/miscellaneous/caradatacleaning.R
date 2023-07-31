library(tidyverse)
library(lubridate)
cara_data <- read_csv("~/Downloads/Day_8_Course_Feedback_Survey_May_3.csv")

cara_data_clean <- cara_data %>%
  mutate(`Submitted on:` = mdy_hms(`Submitted on:`)) %>%
  filter(month(`Submitted on:`) == month(Sys.Date())) %>%
  rename(Date = `Submitted on:`) %>%
  mutate(Date = as.Date(Date)) %>%
  distinct(ID, .keep_all = T)


cara_data_clean %>% write_csv("~/Downloads/Caras_Data.csv")
