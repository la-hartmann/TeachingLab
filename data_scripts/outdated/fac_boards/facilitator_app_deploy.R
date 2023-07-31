facilitator_dashboards <- list.files(here::here("dashboards/IndividualFacilitatorBoards"),
                                     full.names = T)

purrr::map(facilitator_dashboards, 
           ~ rsconnect::deployApp(appDir = .x,
                                  account = "teachinglabhq",
                                  server = "shinyapps.io",
                                  appName = stringr::str_remove(facilitator_dashboards, "/Users/dunk/Teaching Lab/Coding/TeachingLab/dashboards/IndividualFacilitatorBoards/")))
