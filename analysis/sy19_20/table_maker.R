library(TeachingLab)
library(tidyverse)
library(gt)
library(here)
library(scales)
library(glue)

data <- tibble(
  rowname = c("Overall score", "Recognition of race & culture", "Holding growth mindsets", "Having high expectations and beliefs", "Beliefs in the impact of professional learning"),
  `% of educators who held growth mindsets and/or high expectations Fall` = c(31, 21, 49, 26, 29),
  `% of educators who held growth mindsets and/or high expectations Spring` = c(33, 19, 52, 29, 31),
  `Percentage point change of educators who held growth mindsets and/or high expectations Fall to Spring` = c(2, -2, 3, 3, 2),
  `% of educators who improved or sustained growth mindsets and/or high expectations` = c(33, 19, 52, 29, 31)
)

table_maker <- function(data, column_names, title, spanner, n1, n2) {
  (sy1920_report <- data %>% 
     gt(rowname_col = "rowname") %>%
     # Add spanner
     tab_spanner(label = spanner,
                 columns = c(2:3)) %>%
     # Add title
     tab_header(title = md(glue::glue("**{title}**"))) %>%
     # Make labels colorful
     cols_label(
       column_names[1] = html("<strong><center><span style = 'color:#00acf0;'>Diagnostic Survey</span></center></strong>"),
       column_names[2] = html("<strong><center><span style = 'color:#00acf0;'>Follow-up Survey</span></center></strong>"), 
       column_names[3] = html("<strong><center><span style = 'color:#00acf0;'>% Change from Diagnostic to Follow-up</span></center></strong>"), 
       column_names[4] = html("<strong><center><span style = 'color:#43c6b9;'>% of Educators that Improved or Sustained</span></center></strong>")
     ) %>%
     # Column widths
     cols_width(
       1 ~ px(200),
       2 ~ px(125),
       3 ~ px(125),
       4 ~ px(125),
       5 ~ px(200)
     ) %>%
     # Percent format the data
     fmt_percent(
       columns = c(2, 3, 5),
       scale_values = F,
       decimals = 0
     ) %>%
     # Add + symbol where needed
     fmt_percent(
       columns = c(4),
       scale_values = F,
       decimals = 0,
       rows = c(1,3:5),
       pattern = "+{x}"
     ) %>%
     # For - make percent as well
     fmt_percent(
       columns = c(4),
       scale_values = F,
       decimals = 0,
       rows = c(2)
     ) %>%
     # Color all the data columns (2-5)
     data_color(
       columns = c(2),
       colors = scales::col_numeric(
         # custom defined values - notice that order matters!
         palette = tl_palette(n = 6, theme = "light", color = "blue"),
         domain = NULL
       )
     ) %>%
     data_color(
       columns = c(3),
       colors = scales::col_numeric(
         # custom defined values - notice that order matters!
         palette = tl_palette(n = 6, theme = "light", color = "blue"),
         domain = NULL
       )
     ) %>%
     data_color(
       columns = c(4),
       colors = scales::col_numeric(
         # custom defined values - notice that order matters!
         palette = tl_palette(n = 6, theme = "light", color = "blue"),
         domain = NULL
       )
     ) %>%
     data_color(
       columns = c(5),
       colors = scales::col_numeric(
         # custom defined values - notice that order matters!
         palette = tl_palette(n = 6, theme = "light", color = "green"),
         domain = NULL
       )
     ) %>%
     # Add black line next to row group
     tab_style(
       style = list(
         cell_borders(
           sides = c("right"),
           color = "black",
           weight = px(3)
         )
       ),
       locations = cells_stub()
     ) %>%
     # Add black line next to tab spanner
     tab_style(
       style = list(
         cell_borders(
           sides = c("right"),
           color = "black",
           weight = px(3)
         )
       ),
       locations = cells_body(
         columns = c(4)
       )
     ) %>%
     # Footnotes
     tab_footnote(
       footnote = md("Note: The number of observations varies between items from 118 to 143."),
       locations = cells_column_labels(
         columns = c(1:3)
       )
     ) %>%
     tab_footnote(
       footnote = md("n = 143"),
       locations = cells_column_labels(
         columns = c(4)
       )
     ) %>%
     # Final theming
     gt_theme_tl() %>%
     tab_options(column_labels.border.lr.style = "solid",
                 column_labels.vlines.style = "solid",
                 heading.border.lr.style = "solid",
                 heading.border.bottom.width = px(3),
                 heading.border.bottom.color = "black",
                 heading.border.lr.width = px(3),
                 heading.border.lr.color = "black"
     )
  )
  
  sy1920_report %>% 
    gtsave(here("Images/SY19-20ReportTable.png"))
}

