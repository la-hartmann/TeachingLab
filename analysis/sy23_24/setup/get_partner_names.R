library(googlesheets4)
library(tidyverse)


df <- read_sheet("https://docs.google.com/spreadsheets/d/11jlo9UeWxZGwunhDb24hZBwAKc5b8ZKM9AYNWZaUyZY/edit#gid=253301460", 
           sheet = "FY24 Overview Board Automation")

df$`Partner name` |> unique() |> sort()

df$`Course name` |> unique() |> sort()

df |>
  group_by(`Course name`, `Partner name`) |>
  count() |>
  view()
