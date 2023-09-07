library(googlesheets4)
library(TeachingLab)
library(tidyverse)


all_sites <- c(#"All Sites",
               "AR_Arkansas DOE"#,
               # "CA_San Diego",
               # "CA_Silver Giving",
               # "CA_West Contra Costa_Michelle Obama ES",
               # "CA_West Contra Costa_Murphyc ES",
               # "CA_West Contra Costa_WCCUSD",
               # "IL_Chicago Public Schools_Aggregate",
               # "DE_DE Department of Education",
               # "IL_Chicago Public Schools_Network 12",
               # "IL_Chicago Public Schools_Network 4",
               # "IL_Chicago Public Schools_Network 7",
               # "IL_Kankakee District 111",
               # "KY_Louisville School District_Jacob Elem",
               # "LA_Calcasieu Parish",
               # "LA_Jefferson Davis Parish",
               # "LA_Pointe Coupee Parish",
               # "LA_St. Landry Parish",
               # "NM_NM Public Education Department",
               # "NY_Amistad Dual Language",
               # "NY_Channel View School for Research",
               # "NY_D10_10X386",
               # "NY_D11",
               # "NY_D9",
               # "NY_ESMT_IS190",
               # "NY_Fannie Lou Hamer",
               # "NY_Rochester City Schools",
               # "OH_Cleveland Metro School District",
               # "RI_RI Department of Education",
               # "TN_McNairy County Schools",
               # "TX_RAISE Rice University",
               # "US_City Year",
               # "US_Digital Nest",
               # "US_Nellie Mae",
               # "US_Open Enrollment",
               # "VT_Danville School",
               # "VT_Orleans Central Supervisory Union",
               #"WI_WI DPI"
               )

### Generate Reports ###
purrr::walk(all_sites,
           ~ TeachingLab:::partner_file_remove(partner = .x,
                                               input = here::here("analysis/sy22_23/mid_year_report/mid_year_report.Rmd"),
                                               output_dir = here::here("analysis/sy22_23/mid_year_report/partner_reports")))
### Password Protect ###

# reports <- list.files(here::here("analysis/sy22_23/mid_year_report/partner_reports"), full.names = TRUE)
# # passwords <- unlist(purrr::rerun(.n = length(all_sites), TeachingLab::password_generator(length = 8)))
# passwords <- googlesheets4::read_sheet(ss = "1p2v3f0DKqOoxLA5P6odgZWfPfJcjpFsM0FFKKkM8_fg",
#                                        sheet = "Mid-Year Reports",
#                                        range = "B:B") |>
#   dplyr::pull(Password)


# purrr::map2(.x = reports,
#             .y = passwords,
#             ~ pagecryptr::pagecryptr(file = .x,
#                                      password = .y,
#                                      out_file = .x))


# here::here("analysis/sy22_23/mid_year_report/mid_year_report.html")
# fidelius::charm(input = here::here("test.html"),
#                 password = "test",
#                 hint = "Check the google sheet!",
#                 # output = here::here("analysis/sy22_23/mid_year_report/mid_year_report_password.html"),
#                 style = fidelius::stylize(font_family = "Calibri",
#                                           title_color = "#04abeb", btn_bg_color = "#04abeb", modal_title_color = "#04abeb",
#                                           btn_hover_color = "#43464d"),
#                 bundle = TRUE,
#                 minified = TRUE)

### Copy to Website ###
# file.copy(from = list.files(here::here("analysis/sy22_23/mid_year_report/partner_reports"), full.names = TRUE),
#           to = "~/Teaching Lab/Coding/teachinglab.github.io/Reports/2023Reports/MidYear",
#           overwrite = TRUE)
