library(googlesheets4)
library(tidyverse)

cps_overall <- read_sheet("https://docs.google.com/spreadsheets/d/1QqsTVWRSr5PUnA4WKfZoqhQZkCLUwVk2mi3-9JtkbHU/edit#gid=1373827455",
                          sheet = "Overall",
                          range = "B5:E15")

cps_overall_edited <- cps_overall |>
  select(1, 4) |>
  mutate(Total = 100 * Total,
         `Type of Co-Lab` = str_wrap(`Type of Co-Lab`, width = 50))


cps_overall_edited |>
  ggplot(aes(x = fct_reorder(`Type of Co-Lab`, Total), y = Total)) +
  geom_col(color = NA, width = 0.95, fill = "#04abeb") +
  geom_text(aes(label = paste0(round(Total), "%")), hjust = -0.75, fontface = "bold",
            family = "Calibri", size = 8) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
  coord_flip() +
  labs(x = "", y = "",
       title = "CPS Co-Labs % Agree or Strongly Agree (n = 857)") +
  theme_tl(legend = TRUE) +
  theme(plot.title = element_text(face = "bold", size = 22),
        axis.text.y = element_text(size = 15))

ggsave(here::here("images/ma_dese_rfr/sy22_23/cps_colabs.png"),
       bg = "white",
       dpi = 500,
       units = "px",
       height = 1080*5,
       width = 1920*5)
