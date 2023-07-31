#### End of Session Survey ####

## Load pipe ##
library(magrittr)

## Read in data ##
session_survey <- readr::read_rds("data/session_survey_21_22data.rds")

## NAs dataframe
na_df <- tlShiny::na_df

#### Finding most recent groupings ####
recent_choices <- session_survey %>% 
  dplyr::filter(Date > Sys.Date() - 14 & !is.na(Facilitator)) %>% # CURRENTLY SET TO LAST TWO WEEKS
  dplyr::group_by(`Select your site (district, parish, network, or school).`, Facilitator) %>%
  dplyr::summarise() %>%
  tidyr::drop_na() %>%
  dplyr::ungroup() %>%
  dplyr::mutate(id = dplyr::row_number())

## Create dataframe of format ~(site, facilitator) and ~(id)
recent_choices_final <- tibble::tibble(choice = paste(recent_choices$`Select your site (district, parish, network, or school).` %>% as.character() %>% stringr::str_replace_all(., ", ", " "),
                              recent_choices$Facilitator, sep = ", ")) %>%
  dplyr::mutate(id = dplyr::row_number())
  

