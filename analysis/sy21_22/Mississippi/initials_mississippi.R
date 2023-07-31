library(googlesheets4)

df <- read_sheet("https://docs.google.com/spreadsheets/d/1RwTgvE_vp7dqxbRSAXXPiN_t3AVfVtN0fNoGDz49HlY/edit#gid=0")

final_df <- df %>% 
  select(Name) %>%
  transmute(code = paste0(str_sub(Name, 1, 1), str_extract(Name, "(?<= ).")))

final_df %>% 
  range_write(ss = "https://docs.google.com/spreadsheets/d/1RwTgvE_vp7dqxbRSAXXPiN_t3AVfVtN0fNoGDz49HlY/edit#gid=0",
              col_names = F,
              range = "E2:E118")

