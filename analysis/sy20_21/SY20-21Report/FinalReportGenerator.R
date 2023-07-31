library(tidyverse)
library(encryptedRmd) # For passwords

params_list <- list(
  partner = list(
    "All Partners",
    "Evangeline Parish, LA", "Louisiana State Content Leader Training, LA",
    "Lafayette Parish, LA", "East Baton Rouge Parish, LA",
    "Freire Charter Schools, PA/DE", "Pointe Coupee Parish, LA",
    "Building 21", "NYC District 11 - District-wide, NY",
    "NYC District 11 - PS 89, NY", "NYC District 11 - PS 21, NY",
    "NYC District 11 - IS 355, NY", "NYC District 11 - PS 96, NY",
    "NYC District 11 - PS 87, NY", "Legacy Early College, SC", "NYC District 11 - PS 103, NY",
    "Brownington Central School, VT", "NYC District 11 - PS 189, NY",
    "NYC District 11 - PS 498, NY", "Kankakee School District, IL",
    "Tangipahoa Parish, LA", "Mississippi Department of Education, MS",
    "Delaware Department of Education, DE"
  ),
  matched = list("matched", "unmatched")
)

groups <- crossing(partner = params_list$partner, matched = params_list$matched) %>%
  filter(matched != "matched") %>%
  mutate(across(everything(), ~ as.character(.x))) %>%
  add_row(matched = "matched", partner = "All Partners") %>%
  mutate(password = paste0(map2(partner, matched, ~ tolower(paste0(substring(.x, 1, 1), substring(.y, 1, 1), collapse = ""))), row_number()))

partner <- groups$partner
matched <- groups$matched
password <- groups$password

# Test

# Output to folder in R Teaching Lab
walk2(partner, matched, ~ rmarkdown::render(
  input = here::here("analysis/sy20_21/SY20-21Report/FinalReport.rmd"),
  output_file = paste0("2021 Report_", .y, "_", .x),
  output_dir = here::here("analysis/sy20_21/SY20-21Report/Reports"),
  params = list(partner = .x, matched = .y)
))


# Test File

# rmd_full <- list.files(here::here("analysis/sy20_21/SY20-21Report/Reports"), full.names = T, pattern = "*.html")
# rmd_partial <- list.files(here::here("analysis/sy20_21/SY20-21Report/Reports"), pattern = "*.html")

# Looping

# walk2(.x = list.files(here::here("analysis/sy20_21/SY20-21Report/Reports"), full.names = T, pattern = "*.html"), 
#       .y = list.files(here::here("analysis/sy20_21/SY20-21Report/Reports"), pattern = "*.html"),
#      ~ encryptedRmd::encrypt_html_file(path = .x, message_key = T,
#                                        output_path = paste0(here::here("analysis/sy20_21/SY20-21Report/Encrypted"),
#                                                             .y)))

# Output to website folder Teaching Lab
walk(list.files(here::here("analysis/sy20_21/SY20-21Report/Reports"), full.names = T, pattern = "*.html"), 
     ~ file.copy(from = .x, to = "/Users/dunk/Projects/NewWebsite.io/2021Reports", overwrite = T))

walk(list.files(here::here("analysis/sy20_21/SY20-21Report/Reports"), full.names = T, pattern = "*.html"), 
     ~ file.copy(from = .x, to = "/Users/dunk/Teaching Lab/Coding/teachinglab.github.io/2021Reports", overwrite = T))





