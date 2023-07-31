library(tidyverse)

session_survey <- FacilitatorSheets::get_session_survey()

channelview_session <- session_survey %>%
  # filter(str_detect(`Select your site (district, parish, network, or school). - Other (please specify)`,
  #                   "channel|27")) %>%
  slice_sample(n = 4)

channelview_session %>%
  session_agree_plot_ts()

channelview_session %>%
  session_agree_plot()

ggsave(here::here("images/data_requests/hanady/channelview_agree.png"), width = 10, height = 10,
       bg = "white")

channelview_session %>%
  session_quotes()
