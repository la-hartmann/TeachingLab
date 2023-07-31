### Default Read in Data ###
# source(here::here("data_scripts/Monday/get_fac_board.R"))


PMs_Emails <- readr::read_rds("data/PMs.rds") |>
  bind_rows(tibble::tibble(PMs = c("Michele Morenz"),
                           Email = "michele.morenz@teachinglab.org"))
Courses <- readr::read_rds("data/Courses.rds") |>
  dplyr::bind_rows(tibble::tibble(Courses = c("K-2 Supported Planning", 
                                              "Adaptive Curriculum Foundational Skills Bootcamp")))
Facilitators_Emails <- readr::read_rds("data/Facilitators.rds")
Sites <- readr::read_rds("data/Site.rds") %>%
  dplyr::bind_rows(tibble::tibble(Site = c("McNairy TN"), `Time Zone` = ("EST")))









###### Old Method of finding Courses, PMs, Sites ########
## Get all Courses ##
# course_list <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1yJinWbTyMZf0R8FiFiAUk1RBC7thwuUVSRGW8YGjEdg/edit#gid=307365662",
#                             range = "A:A") %>%
#   dplyr::rename(Courses = 1) %>%
#   unique()
## Get all PMs ##
# pm_list <- read_sheet("https://docs.google.com/spreadsheets/d/1nqtFK9_HBMBU6XQwCmmPYkhwBXZUj8wmevF2WGYUlaM/edit?ts=5f5a8a55#gid=1933413518",
#                       sheet = "PMs",
#                       range = "A:C") %>%
#   rename(PMs = 1,
#          Email = 3) %>%
#   select(-2) %>%
#   bind_rows(tibble::tibble(PMs = c("Diana Bowles", "Alicia Faust"), 
#                            Email = c("diana.bowles@teachinglab.org", "alicia.faust@teachinglab.org")))
## Get Sites ##
# sites_list <- read_sheet("https://docs.google.com/spreadsheets/d/1nqtFK9_HBMBU6XQwCmmPYkhwBXZUj8wmevF2WGYUlaM/edit#gid=1070048971",
#                          sheet = "Sites") %>%
#   select(c(1, 2)) %>%
#   dplyr::bind_rows(tibble::tibble(Site = "Jefferson Davis", 
#                                   `Time Zone` = "CST"))

## Walk through list of dataframes to write to data folder ##
# readr::write_rds(facilitator_names_emails_list, 
#                  paste0("data/Facilitators.rds"))
# walk(list(pm_list, sites_list, facilitator_names_emails_list, course_list),
#      ~ write_rds(x = .x, file = paste0("data/", colnames(.x)[1], ".rds")))

# readr::write_rds(facilitator_names_emails_list, 
#                  paste0("dashboards/Staffing/data/Facilitators.rds"))
# walk(list(pm_list, sites_list, facilitator_names_emails_list, course_list),
#      ~ write_rds(x = .x, file = paste0("dashboards/Staffing/data/", colnames(.x)[1], ".rds")))
