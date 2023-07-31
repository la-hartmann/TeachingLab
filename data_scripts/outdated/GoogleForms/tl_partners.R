library(googlesheets4)

#### Stored in this google sheet ####
official_partner_list <- read_sheet("https://docs.google.com/spreadsheets/d/11jlo9UeWxZGwunhDb24hZBwAKc5b8ZKM9AYNWZaUyZY/edit#gid=0",
  sheet = "Automation",
  range = "C:C",
  skip = 1
)

#### Write to data folder ####
official_partner_list %>%
  readr::write_rds(here::here("data/sy21_22/tl_partners.rds"))
#### Write to site completion tracking data folder ####
official_partner_list_reduced <- official_partner_list %>%
  dplyr::mutate(
    `SITES (Educator, End of Session, End of Course, Knowledge Assessments, Attendance sheet)` = TeachingLab::string_replace(
      `SITES (Educator, End of Session, End of Course, Knowledge Assessments, Attendance sheet)`,
      "th and|Andover",
      "North Andover Public Schools, MA"
    ),
    `SITES (Educator, End of Session, End of Course, Knowledge Assessments, Attendance sheet)` = TeachingLab::string_replace(
      `SITES (Educator, End of Session, End of Course, Knowledge Assessments, Attendance sheet)`,
      "bronx",
      "North Bronx School of Empowerment, NY"
    ),
    `SITES (Educator, End of Session, End of Course, Knowledge Assessments, Attendance sheet)` = TeachingLab::string_replace(
      `SITES (Educator, End of Session, End of Course, Knowledge Assessments, Attendance sheet)`,
      "District 11",
      "NYC District 11 - District-wide, NY"
    ),
    `SITES (Educator, End of Session, End of Course, Knowledge Assessments, Attendance sheet)` = TeachingLab::string_replace(
      `SITES (Educator, End of Session, End of Course, Knowledge Assessments, Attendance sheet)`,
      "District 9",
      "NYC District 9 - District-wide, NY"
    ),
    `SITES (Educator, End of Session, End of Course, Knowledge Assessments, Attendance sheet)` = TeachingLab::string_replace(
      `SITES (Educator, End of Session, End of Course, Knowledge Assessments, Attendance sheet)`,
      "EMST",
      "NYC District 12 - EMST-IS 190, NY"
    ),
    `SITES (Educator, End of Session, End of Course, Knowledge Assessments, Attendance sheet)` = TeachingLab::string_replace(
      `SITES (Educator, End of Session, End of Course, Knowledge Assessments, Attendance sheet)`,
      "Coupee",
      "Pointe Coupee Parish, LA"
    ),
    `SITES (Educator, End of Session, End of Course, Knowledge Assessments, Attendance sheet)` = TeachingLab::string_replace(
      `SITES (Educator, End of Session, End of Course, Knowledge Assessments, Attendance sheet)`,
      "Rochester",
      "Rochester City School District - District-wide"
    ),
    `SITES (Educator, End of Session, End of Course, Knowledge Assessments, Attendance sheet)` = TeachingLab::string_replace(
      `SITES (Educator, End of Session, End of Course, Knowledge Assessments, Attendance sheet)`,
      "San Diego",
      "San Diego Unified School District, CA"
    ),
    `SITES (Educator, End of Session, End of Course, Knowledge Assessments, Attendance sheet)` = TeachingLab::string_replace(
      `SITES (Educator, End of Session, End of Course, Knowledge Assessments, Attendance sheet)`,
      "West Contra",
      "West Contra Costa USD, CA"
    ),
    `SITES (Educator, End of Session, End of Course, Knowledge Assessments, Attendance sheet)` = TeachingLab::string_replace(
      `SITES (Educator, End of Session, End of Course, Knowledge Assessments, Attendance sheet)`,
      "Wisconsin Department",
      "Wisconsin Department of Education, WI"
    )
  )

official_partner_list_reduced %>%
  readr::write_rds(here::here("dashboards/SiteCollectionProgress/data/tl_partners.rds"))
