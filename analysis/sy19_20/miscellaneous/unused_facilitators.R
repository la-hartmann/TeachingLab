library(googlesheets4)
library(tidyverse)

all_facs <- read_sheet("https://docs.google.com/spreadsheets/d/1nqtFK9_HBMBU6XQwCmmPYkhwBXZUj8wmevF2WGYUlaM/edit#gid=1933413518",
                       range = "D:D") %>%
  slice(-2)

scored_facs <- read_sheet("https://docs.google.com/spreadsheets/d/1isE5j1XERQHVxTCb01RZXFRLvJRiEW-iH-N-AslMtxo/edit#gid=1421474322",
                          range = "A:A")

setdiff(all_facs$Name, scored_facs$Facilitator) %>%
  clipr::write_clip()
