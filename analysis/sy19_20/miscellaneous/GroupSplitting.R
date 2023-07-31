library(tidyverse)
library(googlesheets4)

group <- c("Melissa", "HaMy", "Duncan", "Taylor", "Shaye", "Nichole")

n <- 3
# Currently finds every possible combination with greater than 7 characters and less than 35
unique_sets <- map(1:1000, ~split(group, lex.order = T, sample(1:2, length(group), replace = T))) %>%
  flatten() %>%
  enframe() %>%
  dplyr::select(-name) %>%
  dplyr::filter(str_length(value) > 7 & str_length(value) < 35) %>% # Super dumb way to do it
  # mutate(value = as_vector(value))
  # mutate(string_length = length(as_vector(value)))
  # dplyr::filter(length(as_vector(value)) >= 2) %>%
  unique() %>%
  # unnest(cols = c(value)) %>%
  # mutate(id = row_number()) %>%
  # Convert to a string
  mutate(value = map(value, ~ toString(.x)),
         value = as_vector(value)) %>%
  # Need to filter so no double uniques
  dplyr::filter() 

ss <- gs4_create("Zoom-Groups", sheets = "Sheet1")

unique_sets %>%
  sheet_write(ss, sheet = "Sheet1")

sheet_properties(ss)

#### AS A FUNCTION

group_maker <- function(names, group_size_lower, group_size_upper) {
  lower_restrict <- if (group_size_lower == 1) {
    2
  } else if (group_size_lower == 2) {
    9
  }
  upper_restrict <- if (group_size_lower == 1) {
    9
  } else if (group_size_lower == 2) {
    13
  }
  unique_sets <- map(1:1000, ~split(names, lex.order = T, sample(1:2, length(names), replace = T))) %>%
    flatten() %>%
    enframe() %>%
    dplyr::select(-name) %>%
    dplyr::filter(str_length(value) > 7 & str_length(value) < 35) %>% # Super dumb way to do it
    # mutate(value = as_vector(value))
    # mutate(string_length = length(as_vector(value)))
    # dplyr::filter(length(as_vector(value)) >= 2) %>%
    unique() %>%
    # unnest(cols = c(value)) %>%
    # mutate(id = row_number()) %>%
    # Convert to a string
    mutate(value = map(value, ~ toString(.x)),
           value = as_vector(value)) %>%
    dplyr::filter()
}