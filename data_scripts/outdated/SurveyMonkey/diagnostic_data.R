##### Get Diagnostic Educator Survey from Survey Monkey #####
##### Writes to both diagnostic survey dashboard as well as data folder #####
options(sm_oauth_token = Sys.getenv("diagnostic_token"))

diagnostic_survey <- TeachingLab::get_diagnostic_survey(update = TRUE)

## Write Misssissippi Data to reports folder ##
knowledge_assessments_mississippi <- readr::read_rds(here::here("data/sy21_22/diagnostic.rds")) |>
  dplyr::filter(stringr::str_detect(your_site_district_parish_network_or_school_br_br, ", MS")) |>
  dplyr::mutate(
    code = toupper(paste0(
      substr(
        please_write_in_your_3_initials_if_you_do_not_have_a_middle_initial_please_write_x_br_this_is_used_to_link_the_diagnostic_and_follow_up_surveys_but_is_kept_confidential_br_br,
        1,
        1
      ),
      substr(
        please_write_in_your_3_initials_if_you_do_not_have_a_middle_initial_please_write_x_br_this_is_used_to_link_the_diagnostic_and_follow_up_surveys_but_is_kept_confidential_br_br,
        3,
        3
      )
    )),
    code = dplyr::na_if(code, "NANA")
  )

readr::write_rds(knowledge_assessments_mississippi, here::here("data/sy21_22/mississippi_reports/knowledge_assessments.rds"))
