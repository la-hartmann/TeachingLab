#### End of Course Survey Dashboard ####

## Load pipe ##
library(magrittr)

## Read in data ##
course_survey <- readr::read_rds("data/course_surveymonkey.rds")

## NAs dataframe ##
na_df <- tlShiny::na_df

## Get recent choices from the last 2 weeks, create a dataframe of Site, Course, id ##
recent_choices <- course_survey |>
  dplyr::filter(date_created > Sys.Date() - 14 & !is.na(`Select your course.`)) |> # CURRENTLY SET TO LAST TWO WEEKS
  dplyr::group_by(`Select your site (district, parish, network, or school).`, `Select your course.`) |>
  dplyr::summarise() |>
  tidyr::drop_na() |>
  dplyr::ungroup() |>
  dplyr::mutate(id = dplyr::row_number())

## Create a dataframe of choice (site, course) and id ##
recent_choices_final <- tibble::tibble(choice = paste(recent_choices$`Select your site (district, parish, network, or school).` |> 
                                                        as.character() |> 
                                                        stringr::str_replace_all(pattern = ", ", replacement = " "),
                                                      recent_choices$`Select your course.`, sep = ", ")) |>
  dplyr::mutate(id = dplyr::row_number())

## Special approvals to the dashboard that are not @teachinglab.org domain
special_approvals <- c("duncan.gates123@gmail.com",
                       "ryan.stewart@gmail.com")


