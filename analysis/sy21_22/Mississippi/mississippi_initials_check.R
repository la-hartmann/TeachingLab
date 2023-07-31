library(googlesheets4)
library(tidyverse)

mississippi_sheet <- read_sheet("https://docs.google.com/spreadsheets/d/1RwTgvE_vp7dqxbRSAXXPiN_t3AVfVtN0fNoGDz49HlY/edit#gid=0")

diagnostic <- get_diagnostic_survey()

initials_fix <- function(x) {
  paste0(substr(x = x, 1, 1), substr(x = x, 3, 3)) %>%
    toupper()
}

not_initials <- diagnostic %>%
  filter(str_detect(your_site_district_parish_network_or_school_br_br, ", MS")) %>%
  mutate(initials = initials_fix(please_write_in_your_3_initials_if_you_do_not_have_a_middle_initial_please_write_x_br_this_is_used_to_link_the_diagnostic_and_follow_up_surveys_but_is_kept_confidential_br_br)) %>%
  pull(initials) %>%
  unique() %>%
  sort()

### Compare diagnostic initials to misssissippi teacher codes ###
initials_not_in_sheet <- setdiff(not_initials, mississippi_sheet$`Teacher code`)

diagnostic %>%
  filter(str_detect(your_site_district_parish_network_or_school_br_br, ", MS")) %>%
  mutate(initials = initials_fix(please_write_in_your_3_initials_if_you_do_not_have_a_middle_initial_please_write_x_br_this_is_used_to_link_the_diagnostic_and_follow_up_surveys_but_is_kept_confidential_br_br)) %>%
  filter(initials %in% initials_not_in_sheet) %>%
  select(initials, your_site_district_parish_network_or_school_br_br) %>%
  clipr::write_clip()
  view()

mississippi_sheet %>%
  filter(`Teacher code` %in% initials_not_in_sheet) %>%
  view()
  filter(`Teacher code` %!in% not_initials) %>%
  view()
  clipr::write_clip()
