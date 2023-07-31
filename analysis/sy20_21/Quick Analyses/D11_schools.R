library(rvest)
library(tidyverse)

content <- read_html("https://ny.chalkbeat.org/2021/7/27/22596666/these-72-nyc-schools-more-teachers-lower-class-sizes")

content %>%
  html_table(fill = T) %>%
  enframe() -> schools