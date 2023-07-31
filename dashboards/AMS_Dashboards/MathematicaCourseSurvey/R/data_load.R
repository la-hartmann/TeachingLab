#### Course Survey Dashboard ####

library(magrittr)

course_survey <- readr::read_rds("data/merged/course_surveymonkey.rds") |> 
  dplyr::filter(date_created >= as.Date("2021-07-01")) |>
  dplyr::mutate(grades_merge = stringr::str_remove_all(paste0("_", `What grade(s) do you teach, support, and/or lead? You can select more than one. - K`, "_",
                                      `What grade(s) do you teach, support, and/or lead? You can select more than one. - 1`, "_",
                                      `What grade(s) do you teach, support, and/or lead? You can select more than one. - 2`, "_",
                                      `What grade(s) do you teach, support, and/or lead? You can select more than one. - 3`, "_",
                                      `What grade(s) do you teach, support, and/or lead? You can select more than one. - 4`, "_",
                                      `What grade(s) do you teach, support, and/or lead? You can select more than one. - 5`, "_",
                                      `What grade(s) do you teach, support, and/or lead? You can select more than one. - 6`, "_",
                                      `What grade(s) do you teach, support, and/or lead? You can select more than one. - 7`, "_",
                                      `What grade(s) do you teach, support, and/or lead? You can select more than one. - 8`, "_",
                                      `What grade(s) do you teach, support, and/or lead? You can select more than one. - 9`, "_",
                                      `What grade(s) do you teach, support, and/or lead? You can select more than one. - 10`, "_",
                                      `What grade(s) do you teach, support, and/or lead? You can select more than one. - 11`, "_",
                                      `What grade(s) do you teach, support, and/or lead? You can select more than one. - 12`), "NA")) |> 
  dplyr::ungroup()

# NAs dataframe
na_df <- TeachingLab::na_df

recent_choices <- course_survey |> 
  dplyr::filter(date_created > Sys.Date() - 100 & !is.na(`Select your course.`)) |> # CURRENTLY SET TO LAST TWO WEEKS
  dplyr::group_by(`Select your site (district, parish, network, or school).`, `Select your course.`) |>
  dplyr::summarise() |>
  tidyr::drop_na() |>
  dplyr::ungroup() |>
  dplyr::mutate(id = dplyr::row_number())

recent_choices_final <- tibble::tibble(choice = paste(recent_choices$`Select your site (district, parish, network, or school).` %>% as.character() %>% stringr::str_replace_all(., ", ", " "),
                                                      recent_choices$`Select your course.`, sep = ", ")) |>
  dplyr::mutate(id = dplyr::row_number())
