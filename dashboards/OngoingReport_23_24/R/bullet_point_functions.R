data_setup <- function(data) {
  data |>
    dplyr::mutate(
      race = dplyr::case_when(
        ethnicity == "Hispanic or Latino" ~ "Hispanic/Latino",
        (!is.na(race_1) & !is.na(race_2)) |
          (!is.na(race_1) & !is.na(race_3)) |
          (!is.na(race_1) & !is.na(race_4)) |
          (!is.na(race_1) & !is.na(race_5)) |
          (!is.na(race_2) & !is.na(race_3)) |
          (!is.na(race_2) & !is.na(race_4)) |
          (!is.na(race_2) & !is.na(race_5)) |
          (!is.na(race_3) & !is.na(race_4)) |
          (!is.na(race_3) & !is.na(race_5)) |
          (!is.na(race_4) & !is.na(race_5)) ~ "More than one race",
        race_1 == "Asian" ~ "Asian",
        race_2 == "Black or African American" ~ "Black or African American",
        race_3 == "Native American or Indian" ~ "Native American or Indian",
        race_5 == "White" ~ "White",
        race_6 == "Prefer not to say" ~ "Prefer not to say",
        race_7 == "I prefer to self describe" ~ "I prefer to self describe"
      ),
      content_area = dplyr::if_else(!content_area %in% c("Math", "ELA"), "Other", content_area)
    )
}

session_summary_bullets <- function(data) {
  
  ### Get second facilitator responses ###
  second_fac <- data |>
    dplyr::select(
      `They demonstrated deep knowledge of the content they facilitated` = coach_ongoing_feed_2_1,
      `They facilitated the content clearly` = coach_ongoing_feed_2_2,
      `They effectively built a safe learning environment` = coach_ongoing_feed_2_3,
      `They seemed fully prepared for the session` = coach_ongoing_feed_2_4,
      `They made adjustments based on participant needs` = coach_ongoing_feed_2_5
    )
  
  
  ### Get first facilitator combined with second agree per question for end of session survey ###
  plot_agree <- data |>
    dplyr::select(
      `They demonstrated deep knowledge of the content they facilitated` = coach_ongoing_feed_1,
      `They facilitated the content clearly` = coach_ongoing_feed_2,
      `They effectively built a safe learning environment` = coach_ongoing_feed_3,
      `They seemed fully prepared for the session` = coach_ongoing_feed_4,
      `They made adjustments based on participant needs` = coach_ongoing_feed_5
    ) |>
    dplyr::bind_rows(second_fac) |>
    tidyr::pivot_longer(tidyr::everything(), names_to = "Question", values_to = "Response") |>
    tidyr::drop_na() |>
    dplyr::group_by(Question, Response) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::group_by(Question) |>
    dplyr::reframe(
      Response = Response,
      Percent = n / sum(n) * 100
    ) |>
    dplyr::filter(Response %in% c("4 - Agree", "5 - Strongly agree")) |>
    dplyr::group_by(Question) |>
    dplyr::reframe(Percent = round(sum(Percent))) |>
    dplyr::mutate(Question = stringr::str_remove_all(Question, "They"),
                  Question = factor(Question, levels = c(" demonstrated deep knowledge of the content they facilitated",
                                                         " facilitated the content clearly",
                                                         " seemed fully prepared for the session",
                                                         " effectively built a safe learning environment",
                                                         " made adjustments based on participant needs"))) |>
    dplyr::arrange(Question)
  
  HTML(paste0("• ", plot_agree$Percent, "% strongly agree or agree that the facilitators ", plot_agree$Question, collapse = "</br>"))
  
}

course_summary_bullets <- function(data) {
  
  ### Get first facilitator combined with second agree per question for end of session survey ###
  plot_agree <- data |>
    dplyr::select(
      `I looked forward to attending this PL` = coach_end_feed_1,
      `I was fully present/"minds-on" during these PL sessions` = coach_end_feed_2,
      `The activities were well-designed to help me meet the learning targets` = coach_end_feed_3,
      `I am satisfied with how the sessions were facilitated` = coach_end_feed_4,
      `This PL was a good use of my time` = coach_end_feed_5,
      `I talk to other teachers about the things I learned in this PL` = coach_end_feed_6,
      `I felt a sense of community with the other participants in this course` = coach_end_feed_7,
      `The PL was relevant to my instructional practices` = coach_end_feed_8,
      `The strategies I’ve learned will improve my instruction` = coach_end_feed_9,
      `The strategies I’ve learned will improve my coaching or supervision of teachers` = coach_end_feed_10,
      `I have applied or will apply what I have learned to my practice` = coach_end_feed_11,
      `The PL has supported me in being responsive to students' backgrounds, cultures, and points of view.` = coach_end_feed_12,
      `I am satisfied with the overall quality of this PL` = coach_end_feed_13
    ) |>
    tidyr::pivot_longer(tidyr::everything(), names_to = "Question", values_to = "Response") |>
    tidyr::drop_na() |>
    dplyr::group_by(Question, Response) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::group_by(Question) |>
    dplyr::reframe(
      Response = Response,
      Percent = n / sum(n) * 100
    ) |>
    dplyr::filter(Response %in% c("4 - Agree", "5 - Strongly agree")) |>
    dplyr::group_by(Question) |>
    dplyr::reframe(Percent = round(sum(Percent))) |>
    dplyr::mutate(Question = stringr::str_replace_all(Question, c("I was" = "they were",
                                                                  "I looked" = "they looked",
                                                                  "I am" = "they were",
                                                                  "I " = "they ",
                                                                  "I’ve" = "they’ve",
                                                                  "my " = "their ",
                                                                  "me " = "them ",
                                                                  "This PL" = "this PL",
                                                                  "The PL" = "the PL",
                                                                  "The strategies" = "the strategies",
                                                                  "The activities" = "the activities")),
                  Question = factor(Question, levels = c("they looked forward to attending this PL",
                                                         "they were fully present/\"minds-on\" during these PL sessions",
                                                         "the activities were well-designed to help them meet the learning targets",
                                                         "they were satisfied with how the sessions were facilitated",
                                                         "this PL was a good use of their time",
                                                         "they talk to other teachers about the things they learned in this PL",
                                                         "they felt a sense of community with the other participants in this course",
                                                         "the PL was relevant to their instructional practices",
                                                         "the strategies they’ve learned will improve their instruction",
                                                         "the strategies they’ve learned will improve their coaching or supervision of teachers",
                                                         "they have applied or will apply what they have learned to their practice",
                                                         "the PL has supported them in being responsive to students' backgrounds, cultures, and points of view.",
                                                         "they were satisfied with the overall quality of this PL"))) |>
    dplyr::arrange(Question)
  
  HTML(paste0("• ", plot_agree$Percent, "% strongly agree or agree that ", plot_agree$Question, collapse = "</br>"))
  
}