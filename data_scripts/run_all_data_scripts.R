library(googlesheets4)
library(purrr)
library(stringr)
## Get All Folders ##
files <- list.files(here::here("data_scripts"), full.names = T)

## Get all files within folders ##
r_scripts <- unlist(purrr::map(files, ~ list.files(.x, full.names = T)))

##### Currently only running qualtrics and qualtrics_to_sheets scripts 06/13/2023 #####
r_scripts_final <- purrr::keep(r_scripts, ~ stringr::str_detect(.x, "qualtrics|monday_to_sheets"))

### Function to run and print script name ###
run_script <- function(script) {
  
  print(paste0("Now running", script))
  source(script)
  
}

devtools::load_all()
googlesheets4::gs4_auth()

## Run all scripts based on location ##
purrr::walk(r_scripts_final[-c(4)], run_script)
### REMINDER TO WORK ON STUDENT WORK APP ###

usethis::edit_file(r_scripts_final[4]) ### So it can be ran and checked at each point

############################################# End Script ######################################################
