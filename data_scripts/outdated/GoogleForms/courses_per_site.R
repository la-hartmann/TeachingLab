library(googlesheets4)
library(tidyverse)

courses_per_site <- read_sheet("https://docs.google.com/spreadsheets/d/11jlo9UeWxZGwunhDb24hZBwAKc5b8ZKM9AYNWZaUyZY/edit#gid=811998430",
                               sheet = "FY23 sites and courses")

courses_site <- courses_per_site |>
  group_by(`Site in Survey`) |>
  summarise(courses = unique(Course))

courses_site |>
  filter(`Site in Survey` == "CA_San Diego") |>
  pull(courses) |>
  unique() |>
  sort() |>
  clipr::write_clip()

courses_site |>
  filter(`Site in Survey` == "DE_DE Department of Education") |>
  pull(courses) |>
  unique() |>
  sort() |>
  clipr::write_clip()

courses_site |>
  filter(`Site in Survey` == "IL_Kankakee District 111") |>
  pull(courses) |>
  unique() |>
  sort() |>
  clipr::write_clip()

courses_site |>
  filter(`Site in Survey` == "TX_RAISE Rice University") |>
  pull(courses) |>
  unique() |>
  sort() |>
  clipr::write_clip()

courses_site |>
  filter(`Site in Survey` == "US_Open Enrollment") |>
  pull(courses) |>
  unique() |>
  sort() |>
  clipr::write_clip()

### OTHER (GET ALL COURSES) ###
courses_site |>
  pull(courses) |>
  unique() |>
  sort() |>
  clipr::write_clip()
