student_work_grades <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/15ixca0QKloZtYLcmj_9Uc20zdQ5FE6pSVj3EBamLoiI/edit?pli=1#gid=0",
                                                 sheet = "Student Work Scores"
) |>
  dplyr::mutate(`Grade Band` = stringr::str_replace_all(`Grade Band`, "Other", "K-2")) |>
  dplyr::filter(!is.na(`Submitted By`)) |>
  dplyr::filter(Site != "MA_DESE")

student_work_grades |>
  filter(str_detect(`Student Work File`, "pdf") & Site != "NM NM_PED" & !is.na(`Subject Area`)) |>
  group_by(`Subject Area`) |>
  slice_sample(n = 126) |>
  write_sheet("1OIhgxzHS9vo2eFBH286Yjlnh8JmscGIBRMoSqgSZVb0",
              sheet = "Samples Scored")

student_work <- TeachingLab::get_student_work(year = "22_23")

student_work_edlight <- read_sheet("1OIhgxzHS9vo2eFBH286Yjlnh8JmscGIBRMoSqgSZVb0",
                                   sheet = "Samples Scored")


### List of all API Request URLs (includes additional file submissions since the google sheet it is pulling from already has submissions) ###
url <- purrr::map2_chr(
  student_work_edlight$`Student Work Survey ID`, student_work_edlight$`Student Work File ID`,
  ~ glue::glue("https://iad1.qualtrics.com/API/v3/surveys/SV_6nwa9Yb4OyXLji6/responses/{.x}/uploaded-files/{.y}")
)

### Download all submissions with file names as file ids ###
needed_submissions <- purrr::walk2(
  url,
  student_work_edlight$`Student Work File ID`,
  ~ httr::GET(
    url = .x,
    httr::add_headers(`X-API-TOKEN` = "r1vgrzHjb3AQrBQEKgLXd8khdF5R7FFjP5lp7bzT"),
    httr::write_disk(paste0("~/Teaching Lab/Coding/TeachingLab/data/edlight/", .y, ".pdf"), overwrite = T),
    httr::progress()
  )
)
