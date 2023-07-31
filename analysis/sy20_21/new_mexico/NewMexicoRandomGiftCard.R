library(googlesheets4)
library(tidyverse)

emails <- read_sheet("https://docs.google.com/spreadsheets/d/1aj3MxmGRkN9CUABxOZUOhTqWpz96Flbidxc3hcp_Q_A/edit#gid=1112537703", sheet = 1) %>%
  select(`Participant Name`, `Email`)


read_sheet("https://docs.google.com/spreadsheets/d/1-P6ZazMnVnH2Cr8WsbcG4RUINI_1HRsDpN0Tiuc_w0I/edit?resourcekey#gid=1365509288", sheet = 1) %>% 
  select(2) %>% 
  distinct() %>%
  mutate(`Please type in your name using the format Last Name, First Name.` = sub("(\\w+),\\s(\\w+)","\\2 \\1", `Please type in your name using the format Last Name, First Name.`)) %>%
  slice_sample(n = 10) %>%
  left_join(emails, by = c("Please type in your name using the format Last Name, First Name." = "Participant Name")) %>%
  gt::gt() %>%
  tab_header(
    title = html("&#129395; Winners!!! &#129395;")
  )
