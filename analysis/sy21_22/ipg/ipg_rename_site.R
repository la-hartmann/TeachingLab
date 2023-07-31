library(googlesheets4)
library(tidyverse)
library(TeachingLab)

ipg_data <- read_sheet("https://docs.google.com/spreadsheets/d/1L33wVpPERyUQdG8WO3sZiyjnHzPvDL91O4yVUQTN14A/edit#gid=1455024681",
                       sheet = 1)


name_replace <- read_sheet("https://docs.google.com/spreadsheets/d/1gAAREhW2A6TQWz1vLvafvwjw4EVT-gmsZnEboROf7pw/edit#gid=1038196930",
                           sheet = "IPG data cleaning") %>%
  filter(`(in IPG responses currently) Name of Site (Parish, District, Network)` %!in%
           c("Springfield Empowerment Zone", "TEST")) %>%
  distinct(`(in IPG responses currently) Name of Site (Parish, District, Network)`,
           .keep_all = T) %>%
  mutate(`(in IPG responses currently) Name of Site (Parish, District, Network)` =
           paste0("^", `(in IPG responses currently) Name of Site (Parish, District, Network)`,
                  "$"))

what_i_want <- name_replace$`Update the name in Sheet responses to:`
names(what_i_want) <- name_replace$`(in IPG responses currently) Name of Site (Parish, District, Network)`

new_names <- ipg_data %>%
  mutate(new_name = str_replace_all(`Name of Site (Parish, District, Network)`,
                                    what_i_want)) %>%
  select(new_name)

new_names$new_name %>%
  clipr::write_clip()
# Doesn't work for some reason so I just manually copy pasted
  # range_write("https://docs.google.com/spreadsheets/d/1L33wVpPERyUQdG8WO3sZiyjnHzPvDL91O4yVUQTN14A/edit#gid=1455024681",
  #             sheet = 1,
  #             col_names = F,
  #             range = "E2:E332",
  #             reformat = F)
