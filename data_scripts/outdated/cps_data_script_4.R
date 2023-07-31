library(googlesheets4)
library(tidyverse)

cps_data <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1QqsTVWRSr5PUnA4WKfZoqhQZkCLUwVk2mi3-9JtkbHU/edit#gid=1659857917",
                                      sheet = 1) |>
  mutate(`Please select the content area for today’s Co-Lab session.` = coalesce(`Please select the content area for today’s Co-Lab session.`,
                                                                                 `Please select the content area for today’s Co-Lab Skyline session.`))

pos_responses <- function(x) {
  
  x <- x[!is.na(x)]
  x <- x[!is.null(x)]
  x <- x[!str_detect(x, "Not Observed|Not observed|NULL")]
  
  x <- (sum(stringr::str_detect(x, "4|5"), na.rm = T)) /
    (sum(!stringr::str_detect(x, "4|5"), na.rm = T) + sum(str_detect(x, "4|5"), na.rm = T))
  
}

cps_data_summarise <- function(content_area, filter_universal = F, filter_skyline = F) {
  
  if (filter_universal == T) {
    cps_data <- cps_data |>
      dplyr::filter(`What type of Co-Lab did you participate in today?` == "Universal")
  }
  
  if (filter_skyline == T) {
    cps_data <- cps_data |>
      dplyr::filter(`What type of Co-Lab did you participate in today?` == "Skyline")
  }
  
  cps_data |>
    dplyr::filter(`Please select the content area for today’s Co-Lab session.` == content_area) |>
    select(`How much do you agree with the following statements about this session? [I looked forward to attending this Co-Lab session.]...7`,
           `How much do you agree with the following statements about this session? [I am satisfied with the overall quality of this Co-Lab session.]...8`,
           `How much do you agree with the following statements about this session? [I was fully present/”minds-on” during this Co-Lab session.]...9`,
           `How much do you agree with the following statements about this session? [The activities of the Co-Lab session were well-designed to help me meet the learning targets.]...10`,
           `How much do you agree with the following statements about this session? [I felt a sense of community with the other participants in this Co-Lab session.]...11`,
           `How much do you agree with the following statements about this session? [I will apply what I have learned in this Co-Lab session to my practice within the next 4-6 weeks.]...12`,
           `How much do you agree with the following statements about this session? [The strategies I’ve learned in this Co-Lab session will improve my instruction.]...13`,
           `How much do you agree with the following statements about this session? [This Co-Lab session provided me with the opportunity to actively explore the Skyline curriculum.]`,
           `How much do you agree with the following statements about this session? [This Co-Lab session helped me identify next steps to strengthen my use of curriculum to meet my students' needs.]`,
           `How much do you agree with the following statements about this session? [This Co-Lab session effectively built on what we learned from the Unit Launch and Practice session.]`,
           `How much do you agree with the following statements about this session? [This Co-Lab session reinforced or deepened my understanding of the Key Practices.]`,
           `How much do you agree with the following statements about this session? [This Co-Lab session provided me with the opportunity to actively explore the Creative Curriculum.]`,
           `How much do you agree with the following statements about this session? [This Co-Lab session helped me identify next steps to strengthen my use of Creative Curriculum to meet my students' needs.]`,
           `How much do you agree with the following statements about this session? [This Co-Lab session has supported me in being responsive to students' backgrounds, cultures, and points of view.]`,
           `How much do you agree with the following statements about this session? [This Co-Lab session provided strategies and reflection on building healthy and productive relationships in my classroom for all students.]`,
           `How much do you agree with the following statements about this session? [I would recommend this Co-Lab professional learning to another teacher.]`,
           `How much do you agree with the following statements about this session? [This Co-Lab session was a productive use of my time.]`,
           `How much do you agree with the following statements about this session? [There was sufficient time to cover the content of the Co-Lab session.]`,
           `Which Co-Lab did you participate in today?`) |>
    dplyr::group_by(`Which Co-Lab did you participate in today?`) |>
    dplyr::summarise(across(everything(), ~ pos_responses(.x))) |>
    dplyr::mutate(`Which Co-Lab did you participate in today?` = factor(`Which Co-Lab did you participate in today?`,
                                                                        levels = c(map_chr(1:16, ~ paste0("Co-Lab ", .x))))) |>
    dplyr::arrange(`Which Co-Lab did you participate in today?`) |>
    tidyr::pivot_longer(!`Which Co-Lab did you participate in today?`, names_to = "Question", values_to = "% Positive Responses") |>
    dplyr::mutate(Question = stringr::str_remove_all(Question, "\\.\\.\\.[0-9]"),
                  Question = stringr::str_remove_all(Question, "How much do you agree with the following statements about this session\\? \\[|[0-9]|\\]")) |>
    pivot_wider(names_from = `Which Co-Lab did you participate in today?`, values_from = `% Positive Responses`)
  
}

#### Early Childhood Section ####
#### Overall #####
early_childhood_data <- cps_data_summarise(content_area = "Early Childhood") 
childhood_range <- paste0(LETTERS[ncol(early_childhood_data)+1], nrow(early_childhood_data) + 3)

early_childhood_data |>
  range_write(ss = "1QqsTVWRSr5PUnA4WKfZoqhQZkCLUwVk2mi3-9JtkbHU",
              sheet = "Early Childhood",
              range = glue::glue("B3:{childhood_range}"),
              col_names = TRUE,
              reformat = FALSE)

#### Universal #####
early_childhood_data_universal <- cps_data_summarise(content_area = "Early Childhood",
                                                     filter_universal = TRUE)
childhood_range_universal <- paste0(LETTERS[ncol(early_childhood_data_universal)+1], 
                                    nrow(early_childhood_data_universal) + 26)

early_childhood_data_universal |>
  range_write(ss = "1QqsTVWRSr5PUnA4WKfZoqhQZkCLUwVk2mi3-9JtkbHU",
              sheet = "Early Childhood",
              range = glue::glue("B26:{childhood_range_universal}"),
              col_names = TRUE,
              reformat = FALSE)

#### Skyline #####
early_childhood_data_skyline <- cps_data_summarise(content_area = "Early Childhood",
                                                     filter_skyline = TRUE)
childhood_range_skyline <- paste0(LETTERS[ncol(early_childhood_data_skyline)+1], 
                                    nrow(early_childhood_data_skyline) + 50)

early_childhood_data_skyline |>
  range_write(ss = "1QqsTVWRSr5PUnA4WKfZoqhQZkCLUwVk2mi3-9JtkbHU",
              sheet = "Early Childhood",
              range = glue::glue("B50:{childhood_range_skyline}"),
              col_names = TRUE,
              reformat = FALSE)


#### End of Early Childhood Section ####


#### ELA Section ####

#### Overall #####
ela_data <- cps_data_summarise(content_area = "ELA") 
childhood_range <- paste0(LETTERS[ncol(ela_data)+1], nrow(ela_data) + 3)

ela_data |>
  range_write(ss = "1QqsTVWRSr5PUnA4WKfZoqhQZkCLUwVk2mi3-9JtkbHU",
              sheet = "ELA",
              range = glue::glue("B3:{childhood_range}"),
              col_names = TRUE,
              reformat = FALSE)

#### Universal #####
ela_data_universal <- cps_data_summarise(content_area = "ELA",
                                                     filter_universal = TRUE)
childhood_range_universal <- paste0(LETTERS[ncol(ela_data_universal)+1], 
                                    nrow(ela_data_universal) + 26)

ela_data_universal |>
  range_write(ss = "1QqsTVWRSr5PUnA4WKfZoqhQZkCLUwVk2mi3-9JtkbHU",
              sheet = "ELA",
              range = glue::glue("B26:{childhood_range_universal}"),
              col_names = TRUE,
              reformat = FALSE)

#### Skyline #####
ela_data_skyline <- cps_data_summarise(content_area = "ELA",
                                                   filter_skyline = TRUE)
childhood_range_skyline <- paste0(LETTERS[ncol(ela_data_skyline)+1], 
                                  nrow(ela_data_skyline) + 50)

ela_data_skyline |>
  range_write(ss = "1QqsTVWRSr5PUnA4WKfZoqhQZkCLUwVk2mi3-9JtkbHU",
              sheet = "ELA",
              range = glue::glue("B50:{childhood_range_skyline}"),
              col_names = TRUE,
              reformat = FALSE)

#### End of ELA Section ####

#### Health, Arts, & Physical Education ####

health_physical_etc_data <- cps_data_summarise(content_area = "Health, Arts, & Physical Education") 
childhood_range <- paste0(LETTERS[ncol(health_physical_etc_data)+1], nrow(health_physical_etc_data) + 3)

health_physical_etc_data |>
  range_write(ss = "1QqsTVWRSr5PUnA4WKfZoqhQZkCLUwVk2mi3-9JtkbHU",
              sheet = "Health, Arts, & Physical Education",
              range = glue::glue("B3:{childhood_range}"),
              col_names = TRUE,
              reformat = FALSE)

#### Universal #####
health_physical_etc_data_universal <- cps_data_summarise(content_area = "Health, Arts, & Physical Education",
                                         filter_universal = TRUE)
childhood_range_universal <- paste0(LETTERS[ncol(health_physical_etc_data_universal)+1], 
                                    nrow(health_physical_etc_data_universal) + 26)

health_physical_etc_data_universal |>
  range_write(ss = "1QqsTVWRSr5PUnA4WKfZoqhQZkCLUwVk2mi3-9JtkbHU",
              sheet = "Health, Arts, & Physical Education",
              range = glue::glue("B26:{childhood_range_universal}"),
              col_names = TRUE,
              reformat = FALSE)

#### Skyline #####
health_physical_etc_data_skyline <- cps_data_summarise(content_area = "Health, Arts, & Physical Education",
                                       filter_skyline = TRUE)
childhood_range_skyline <- paste0(LETTERS[ncol(health_physical_etc_data_skyline)+1], 
                                  nrow(health_physical_etc_data_skyline) + 50)

health_physical_etc_data_skyline |>
  range_write(ss = "1QqsTVWRSr5PUnA4WKfZoqhQZkCLUwVk2mi3-9JtkbHU",
              sheet = "Health, Arts, & Physical Education",
              range = glue::glue("B50:{childhood_range_skyline}"),
              col_names = TRUE,
              reformat = FALSE)

#### End of Health, Arts, & Physical Education ####


#### Math Section ####

math_data <- cps_data_summarise(content_area = "Math") 
childhood_range <- paste0(LETTERS[ncol(math_data)+1], nrow(math_data) + 3)

math_data |>
  range_write(ss = "1QqsTVWRSr5PUnA4WKfZoqhQZkCLUwVk2mi3-9JtkbHU",
              sheet = "Math",
              range = glue::glue("B3:{childhood_range}"),
              col_names = TRUE,
              reformat = FALSE)

#### Universal #####
math_data_universal <- cps_data_summarise(content_area = "Math",
                                                         filter_universal = TRUE)
childhood_range_universal <- paste0(LETTERS[ncol(math_data_universal)+1], 
                                    nrow(math_data_universal) + 26)

math_data_universal |>
  range_write(ss = "1QqsTVWRSr5PUnA4WKfZoqhQZkCLUwVk2mi3-9JtkbHU",
              sheet = "Math",
              range = glue::glue("B26:{childhood_range_universal}"),
              col_names = TRUE,
              reformat = FALSE)

#### Skyline #####
math_data_skyline <- cps_data_summarise(content_area = "Math",
                                                       filter_skyline = TRUE)
childhood_range_skyline <- paste0(LETTERS[ncol(math_data_skyline)+1], 
                                  nrow(math_data_skyline) + 50)

math_data_skyline |>
  range_write(ss = "1QqsTVWRSr5PUnA4WKfZoqhQZkCLUwVk2mi3-9JtkbHU",
              sheet = "Math",
              range = glue::glue("B50:{childhood_range_skyline}"),
              col_names = TRUE,
              reformat = FALSE)

#### End of Math Section ####

#### Other Section ####

other_data <- cps_data_summarise(content_area = "Other") 
childhood_range <- paste0(LETTERS[ncol(other_data)+1], nrow(other_data) + 3)

other_data |>
  range_write(ss = "1QqsTVWRSr5PUnA4WKfZoqhQZkCLUwVk2mi3-9JtkbHU",
              sheet = "Other",
              range = glue::glue("B3:{childhood_range}"),
              col_names = TRUE,
              reformat = FALSE)

#### Universal #####
other_data_universal <- cps_data_summarise(content_area = "Other",
                                          filter_universal = TRUE)
childhood_range_universal <- paste0(LETTERS[ncol(other_data_universal)+1], 
                                    nrow(other_data_universal) + 26)

other_data_universal |>
  range_write(ss = "1QqsTVWRSr5PUnA4WKfZoqhQZkCLUwVk2mi3-9JtkbHU",
              sheet = "Other",
              range = glue::glue("B26:{childhood_range_universal}"),
              col_names = TRUE,
              reformat = FALSE)

#### Skyline #####
other_data_skyline <- cps_data_summarise(content_area = "Other",
                                        filter_skyline = TRUE)
childhood_range_skyline <- paste0(LETTERS[ncol(other_data_skyline)+1], 
                                  nrow(other_data_skyline) + 50)

other_data_skyline |>
  range_write(ss = "1QqsTVWRSr5PUnA4WKfZoqhQZkCLUwVk2mi3-9JtkbHU",
              sheet = "Other",
              range = glue::glue("B50:{childhood_range_skyline}"),
              col_names = TRUE,
              reformat = FALSE)

#### End of Other Section ####


#### Science Section ####

science_data <- cps_data_summarise(content_area = "Science") 
childhood_range <- paste0(LETTERS[ncol(science_data)+1], nrow(science_data) + 3)

science_data |>
  range_write(ss = "1QqsTVWRSr5PUnA4WKfZoqhQZkCLUwVk2mi3-9JtkbHU",
              sheet = "Science",
              range = glue::glue("B3:{childhood_range}"),
              col_names = TRUE,
              reformat = FALSE)

#### Universal #####
science_data_universal <- cps_data_summarise(content_area = "Science",
                                           filter_universal = TRUE)
childhood_range_universal <- paste0(LETTERS[ncol(science_data_universal)+1], 
                                    nrow(science_data_universal) + 26)

science_data_universal |>
  range_write(ss = "1QqsTVWRSr5PUnA4WKfZoqhQZkCLUwVk2mi3-9JtkbHU",
              sheet = "Science",
              range = glue::glue("B26:{childhood_range_universal}"),
              col_names = TRUE,
              reformat = FALSE)

#### Skyline #####
science_data_skyline <- cps_data_summarise(content_area = "Science",
                                         filter_skyline = TRUE)
childhood_range_skyline <- paste0(LETTERS[ncol(science_data_skyline)+1], 
                                  nrow(science_data_skyline) + 50)

science_data_skyline |>
  range_write(ss = "1QqsTVWRSr5PUnA4WKfZoqhQZkCLUwVk2mi3-9JtkbHU",
              sheet = "Science",
              range = glue::glue("B50:{childhood_range_skyline}"),
              col_names = TRUE,
              reformat = FALSE)

#### End of Science Section ####


#### Social Studies Section ####

social_studies_data <- cps_data_summarise(content_area = "Social Studies") 
childhood_range <- paste0(LETTERS[ncol(social_studies_data)+1], nrow(social_studies_data) + 3)

social_studies_data |>
  range_write(ss = "1QqsTVWRSr5PUnA4WKfZoqhQZkCLUwVk2mi3-9JtkbHU",
              sheet = "Social Studies",
              range = glue::glue("B3:{childhood_range}"),
              col_names = TRUE,
              reformat = FALSE)

#### Universal #####
social_studies_data_universal <- cps_data_summarise(content_area = "Social Studies",
                                             filter_universal = TRUE)
childhood_range_universal <- paste0(LETTERS[ncol(social_studies_data_universal)+1], 
                                    nrow(social_studies_data_universal) + 26)

social_studies_data_universal |>
  range_write(ss = "1QqsTVWRSr5PUnA4WKfZoqhQZkCLUwVk2mi3-9JtkbHU",
              sheet = "Social Studies",
              range = glue::glue("B26:{childhood_range_universal}"),
              col_names = TRUE,
              reformat = FALSE)

#### Skyline #####
social_studies_data_skyline <- cps_data_summarise(content_area = "Social Studies",
                                           filter_skyline = TRUE)
childhood_range_skyline <- paste0(LETTERS[ncol(social_studies_data_skyline)+1], 
                                  nrow(social_studies_data_skyline) + 50)

social_studies_data_skyline |>
  range_write(ss = "1QqsTVWRSr5PUnA4WKfZoqhQZkCLUwVk2mi3-9JtkbHU",
              sheet = "Social Studies",
              range = glue::glue("B50:{childhood_range_skyline}"),
              col_names = TRUE,
              reformat = FALSE)

#### End of Social Studies Section ####