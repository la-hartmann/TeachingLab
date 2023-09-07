library(googlesheets4)
library(tidyverse)

attendance <- googlesheets4::read_sheet(ss = "15mSsAWPq-d0s-syNHKhgUPf4iHYr-zP0uslxKcRheIc",
                                        sheet = 1)

course_columns <- attendance |>
  select(contains("course did")) |>
  colnames()

coalesce_plus <- function(data, vars) {
  x <- as.list(select(data, vars))
  data.frame(data, coalesced_col = coalesce(!!!x))
}

attendance_organized <- attendance |>
  dplyr::mutate(
    Name = paste(`Enter your first name`, `Enter your last name`),
    School = as.character(`What is your school? (If you do not work at a school site, please put "n/a".)`),
    Email = `What is your email?(Please double check the accuracy of  your email address.)`,
    Site = `Please select your site (district, parish, network, or school)`
  ) |>
  coalesce_plus(vars = course_columns) |>
  dplyr::select(
    Date = Timestamp,
    Site,
    School,
    Email,
    Name,
    Role = Roles,
    Course = coalesced_col
  )


attendance_organized |>
  filter(Site == "NY_D9" & Date >= as.Date("2022-10-01")& str_detect(School, "35|42|294|274|163")) |>
  view()


attendance_organized |>
  filter(Site == "LA_Pointe Coupee Parish" & 
           Date >= as.Date("2023-01-01") & Date < as.Date("2023-02-01") &
           Course == "Lab Leaders 1") |>
  write_sheet()
