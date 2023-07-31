# Week 38: Gov Spending on Kids

library(tidykids)
library(tidyverse)
library(gt)
library(purrr)

# datawrangling

data <- tidykids %>%
  select(state, variable, year, inf_adj_perchild)

new_data <- readr::read_csv(here::here("data/sy22_23/visualizations/per_pupil_spending.csv")) |>
  mutate(year = "2022",
         inf_adj_perchild = amountPerPupil/1000,
         variable = "PK12ed") |>
  select(-amountPerPupil) |>
  rename(state = State) |>
  bind_rows(tibble::tibble(state = "District of Columbia",
                           inf_adj_perchild = 22.856,
                           variable = "PK12ed",
                           year = "2022"))

relevantyears <- c(1997, 2007, 2016, 2022)

table <- data |>
  filter(variable == "PK12ed") |>
  mutate(inf_adj_perchild = 1.2 * inf_adj_perchild) |> # inflation rate since 2016
  bind_rows(new_data) |>
  filter(
    year %in% relevantyears
  ) |>
  mutate(inf_adj_perchild = round(inf_adj_perchild, digits = 2)) %>%
  pivot_wider(names_from = year, values_from = inf_adj_perchild) %>%
  rename(State = state) %>%
  select(-variable)

# sparklines for every state

plot_group <- function(df) {
  plot_object <-
    ggplot(data = df, aes(x = year, y = inf_adj_perchild, group = 1)) +
    geom_line(color = "#7C7287", size = 5) +
    geom_point(data = dplyr::filter(df, year == 2022), color = "#7C7287", size = 20) +
    theme_void()
  return(plot_object)
}

df <- data |>
  filter(variable == "PK12ed") |>
  mutate(inf_adj_perchild = 1.2 * inf_adj_perchild) |> # inflation rate since 2016
  bind_rows(new_data) |>
  filter(
    year %in% relevantyears
  ) |>
  mutate(inf_adj_perchild = round(inf_adj_perchild, digits = 2)) %>%
  rename(State = state)

tibble_plot <- df |>
  group_by(State) |>
  nest() |>
  mutate(plot = map(data, plot_group)) |>
  select(-data)

# creating the table

table |>
  mutate(ggplot = NA) |>
  gt() |>
  tab_header(
    title = gt::html("<b>Public spending on elementary and<br>secondary education in the US</b>"),
    subtitle = md("Amount of USD spent in $1000s per child (inflation-adjusted)<br><br>")
  ) %>%
  tab_source_note(source_note = md("**Data**: Urban Institute | **Table**: @gates_duncan")) |>
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_column_labels(c(State))
  ) |>
  data_color(
    columns = c(`2022`),
    colors = scales::col_numeric(tl_palette(color = "blue", n = 6), domain = NULL)
  ) |>
  cols_align(
    align = "center",
    columns = 2:4
  ) |>
  gt_theme_tl() |>
  tab_options(
    heading.title.font.size = 30,
    heading.subtitle.font.size = 15,
    heading.align = "left",
    column_labels.border.bottom.color = "grey",
    column_labels.border.bottom.width = px(1)
  ) |>
  cols_label(ggplot = "Trend") %>%
  text_transform(
    locations = cells_body(columns = c(ggplot)),
    fn = function(x) {
      map(tibble_plot$plot, ggplot_image, height = px(20), aspect_ratio = 5)
    }
  ) %>%
  gtsave("images/visualizations_proj/childhoodspending.html")

file.copy(from = here::here("images/visualizations_proj/childhoodspending.html"),
          to = "~/Teaching Lab/Coding/teachinglab.github.io/USStateSpendingperKid.html")
