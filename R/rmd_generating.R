#' @title Remove Image Files Render R Markdown
#' @param partner The partner to render a report for
#' @param input the rmd to use to parametrically generate reports
#' @param output_dir the output directory for the files
#' @param content_area content area to filter by default to NULL and ignored
#' @param ... arguments passed to `rmarkdown::render`
#' @description remove files from images/report_images and images/report_summary_images and render specified rmd
partner_file_remove <- function(partner, content_area = NULL, input, output_dir, ...) {
  
  print("Rendering new rmd...")
  
  output_file <- paste0("final_report_", stringr::str_replace_all(tolower(partner), c(" " = "_",
                                                                "-" = "_",
                                                                "," = "_")))
  
  rmarkdown::render(
    input = input,
    output_file = output_file,
    output_dir = output_dir,
    params = purrr::keep(list(partner = partner,
                              content_area = content_area),
                         ~ !is.null(.x)),
    ...
  )
  
  do.call(file.remove, list(list.files("images/report_images", full.names = TRUE)))
  do.call(file.remove, list(list.files("images/report_summary_images", full.names = TRUE)))
  
  print("Removing files...\n")
  
}

#' @title End of session feedback graph dependent on race and content area
#' @description Returns a barchart for selections in the relevant questions of the end of coaching survey
#' @param race_filter the filter to apply to the race column, one of "All", "White", "Black or African American", "Asian", "Hispanic/Latino", "More than one race", or "Other"
#' @param content_area_filter the content area to filter for, one otf "All", "ELA", "Math", or "Other"
#' @return prints a ggplot object
#' @export
session_feedback_graph <- function(race_filter = "All Races", content_area_filter = "All") {
  if (content_area_filter == "All") {
    session_survey_2 <- session_survey
  } else {
    session_survey_2 <- session_survey |>
      dplyr::filter(content_area == content_area_filter)
  }
  
  if (race_filter == "All Races") {
    session_survey_3 <- session_survey_2
  } else {
    session_survey_3 <- session_survey_2 |>
      dplyr::filter(race == race_filter)
  }
  
  ### Get second facilitator responses ###
  second_fac <- session_survey_3 |>
    dplyr::select(
      `They demonstrated deep knowledge of the content they facilitated` = fac_feedback_2_1,
      `They facilitated the content clearly` = fac_feedback_2_2,
      `They effectively built a safe learning community` = fac_feedback_2_3,
      `They were fully prepared for the session` = fac_feedback_2_4,
      `They responded to the group's needs` = fac_feedback_2_5
    )
  ### Get first facilitator combined with second agree per question for end of session survey ###
  plot_agree <- session_survey_3 |>
    dplyr::select(
      `They demonstrated deep knowledge of the content they facilitated` = fac_feedback_1,
      `They facilitated the content clearly` = fac_feedback_2,
      `They effectively built a safe learning community` = fac_feedback_3,
      `They were fully prepared for the session` = fac_feedback_4,
      `They responded to the group's needs` = fac_feedback_5
    ) |>
    dplyr::bind_rows(second_fac) |>
    tidyr::pivot_longer(tidyr::everything(), names_to = "Question", values_to = "Response") |>
    tidyr::drop_na() |>
    dplyr::group_by(Question, Response) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::group_by(Question) |>
    dplyr::mutate(Question = str_wrap(Question, width = 20)) |>
    dplyr::reframe(
      n = n,
      Response = Response,
      Percent = n / sum(n) * 100
    )
  
  ### Calculate agree/strongly agree per each question ###
  prepared_agree <- plot_agree |>
    TeachingLab::agree_strongly_agree(question = "They were fully\nprepared for the\nsession")
  responded_agree <- plot_agree |>
    TeachingLab::agree_strongly_agree(question = "They responded to\nthe group's needs")
  facilitated_agree <- plot_agree |>
    TeachingLab::agree_strongly_agree(question = "They facilitated the\ncontent clearly")
  effectively_agree <- plot_agree |>
    TeachingLab::agree_strongly_agree(question = "They effectively\nbuilt a safe\nlearning community")
  demonstrated_agree <- plot_agree |>
    TeachingLab::agree_strongly_agree(question = "They demonstrated\ndeep knowledge of\nthe content they\nfacilitated")
  
  ### Calculate n size ###
  n_size_agree <- session_survey_3 |>
    dplyr::count(sort = T)
  
  ### Make ggplot of session survey agree percent ###
  p <- plot_agree |>
    dplyr::group_by(Question, Response) |>
    dplyr::summarise(Percent = weighted.mean(Percent, n)) |>
    dplyr::mutate(Question = factor(Question)) |>
    ggplot2::ggplot(ggplot2::aes(x = Question, y = Percent, fill = factor(Response))) +
    ggplot2::geom_col(color = NA, width = 0.95, position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = dplyr::if_else(Percent >= 10, paste0(round(Percent), "%"), ""),
        color = Response
      ),
      size = 6.5,
      position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE),
      fontface = "bold"
    ) +
    ggplot2::scale_fill_manual(values = c(
      "(1) Strongly disagree" = "#040404", "(2) Disagree" = "#032E3F",
      "(3) Neither agree nor disagree" = "#02587A", "(4) Agree" = "#0182B4", "(5) Strongly agree" = "#00ACF0"
    )) +
    ggplot2::scale_color_manual(values = c(
      "(1) Strongly disagree" = "white", "(2) Disagree" = "white",
      "(3) Neither agree nor disagree" = "black", "(4) Agree" = "black", "(5) Strongly agree" = "black"
    )) +
    ggplot2::labs(
      fill = "", title = glue::glue("Participant Perceptions of Course Facilitation (n = {format(sum(n_size_agree$n), big.mark = ',')})"),
      x = "", y = ""
    ) +
    ggplot2::coord_flip() +
    ggplot2::guides(
      fill = ggplot2::guide_legend(),
      color = "none"
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    TeachingLab::theme_final_report() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 17),
      plot.title = ggplot2::element_text(size = 25),
      legend.text = ggplot2::element_text(size = 15)
    )
  
  cat("#### ", race_filter, "\n")
  cat("\n")
  
  print(p)
  
  cat("\n")
  cat("In summary, we see the following % agree or strongly agree with the above statements:")
  cat("\n")
  cat("\n")
  
  cat(paste0("- ", prepared_agree, " strongly agree or agree that the facilitators were fully prepared for the session."))
  cat("\n")
  cat("\n")
  cat(paste0("- ", responded_agree, " strongly agree or agree that the facilitators were fully prepared for the session."))
  cat("\n")
  cat(paste0("- ", facilitated_agree, " strongly agree or agree that the facilitators were fully prepared for the session."))
  cat("\n")
  cat(paste0("- ", effectively_agree, " strongly agree or agree that the facilitators were fully prepared for the session."))
  cat("\n")
  cat(paste0("- ", demonstrated_agree, " strongly agree or agree that the facilitators were fully prepared for the session."))
  
  cat("\n\n")
}

#' @title Course survey feedback graph dependent on race and content area
#' @description Returns a barchart for selections in the relevant questions of the end of coaching survey
#' @param race_filter the filter to apply to the race column, one of "All", "White", "Black or African American", "Asian", "Hispanic/Latino", "More than one race", or "Other"
#' @param content_area_filter the content area to filter for, one otf "All", "ELA", "Math", or "Other"
#' @return prints a ggplot object
#' @export
course_feedback_graph <- function(race_filter = "All", content_area_filter = "All") {
  if (content_area_filter == "All") {
    course_survey_2 <- course_survey
  } else {
    course_survey_2 <- course_survey |>
      dplyr::filter(content_area == content_area_filter)
  }
  if (race_filter == "All Races") {
    course_survey_3 <- course_survey_2
  } else {
    course_survey_3 <- course_survey_2 |>
      dplyr::filter(race == race_filter)
  }
  
  ### Get first facilitator combined with second agree per question for end of course survey ###
  plot_agree <- course_survey_3 |>
    dplyr::select(
      `I looked forward to attending this PL` = course_feedback_1,
      `I was fully present/"minds-on" during these PL sessions` = course_feedback_2,
      `The activities were well-designed to help me meet the learning targets` = course_feedback_3,
      `I am satisfied with how the course was facilitated` = course_feedback_4,
      `This PL was a good use of my time` = course_feedback_5,
      `I talk to other teachers about the things I learned in this PL` = course_feedback_6,
      `I felt a sense of community with the other participants in this course` = course_feedback_7,
      `This course was relevant to my instructional practices` = course_feedback_8,
      `The strategies I've learned in this course will improve my instruction` = course_feedback_9,
      `The strategies I've learned in this course will improve my coaching or supervision of teachers` = course_feedback_10,
      `The strategies I've learned in the course are easy to implement` = course_feedback_11,
      `I have applied or will apply what I have learned in this course to my practice` = course_feedback_12,
      `This course has supported me in being responsive to students' backgrounds, cultures, and points of view` = course_feedback_13,
      `I am satisfied with the overall quality of this course` = course_feedback_14
    ) |>
    tidyr::pivot_longer(tidyr::everything(), names_to = "Question", values_to = "Response") |>
    tidyr::drop_na() |>
    dplyr::group_by(Question, Response) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::group_by(Question) |>
    dplyr::mutate(Question = str_wrap(Question, width = 40)) |>
    dplyr::reframe(
      n = n,
      Response = Response,
      Percent = n / sum(n) * 100
    )
  
  ### Calculate agree/strongly agree per each question ###
  facilitated_agree <- plot_agree |>
    TeachingLab::agree_strongly_agree(question = plot_agree$Question |> keep(~ str_detect(.x, "facilitated")) |> magrittr::extract(1))
  quality_agree <- plot_agree |>
    TeachingLab::agree_strongly_agree(question = plot_agree$Question |> keep(~ str_detect(.x, "quality")) |> magrittr::extract(1))
  community_agree <- plot_agree |>
    TeachingLab::agree_strongly_agree(question = plot_agree$Question |> keep(~ str_detect(.x, "community")) |> magrittr::extract(1))
  applied_agree <- plot_agree |>
    TeachingLab::agree_strongly_agree(question = plot_agree$Question |> keep(~ str_detect(.x, "applied")) |> magrittr::extract(1))
  looked_agree <- plot_agree |>
    TeachingLab::agree_strongly_agree(question = plot_agree$Question |> keep(~ str_detect(.x, "looked")) |> magrittr::extract(1))
  talk_agree <- plot_agree |>
    TeachingLab::agree_strongly_agree(question = plot_agree$Question |> keep(~ str_detect(.x, "talk")) |> magrittr::extract(1))
  present_agree <- plot_agree |>
    TeachingLab::agree_strongly_agree(question = plot_agree$Question |> keep(~ str_detect(.x, "minds")) |> magrittr::extract(1))
  activities_agree <- plot_agree |>
    TeachingLab::agree_strongly_agree(question = plot_agree$Question |> keep(~ str_detect(.x, "designed")) |> magrittr::extract(1))
  improve_supervision_agree <- plot_agree |>
    TeachingLab::agree_strongly_agree(question = plot_agree$Question |> keep(~ str_detect(.x, "supervision")) |> magrittr::extract(1))
  improve_instruction_agree <- plot_agree |>
    TeachingLab::agree_strongly_agree(question = plot_agree$Question |> keep(~ str_detect(.x, "facilitated")) |> keep(~ !str_detect(.x, "supervison")) |> magrittr::extract(1))
  supported_agree <- plot_agree |>
    TeachingLab::agree_strongly_agree(question = plot_agree$Question |> keep(~ str_detect(.x, "responsive")) |> magrittr::extract(1))
  relevant_agree <- plot_agree |>
    TeachingLab::agree_strongly_agree(question = plot_agree$Question |> keep(~ str_detect(.x, "relevant")) |> magrittr::extract(1))
  time_agree <- plot_agree |>
    TeachingLab::agree_strongly_agree(question = plot_agree$Question |> keep(~ str_detect(.x, "time")) |> magrittr::extract(1))
  
  ### Calculate n size ###
  n_size_agree <- course_survey_3 |>
    dplyr::count(sort = T)
  
  ### Make ggplot of course survey agree percent ###
  p <- plot_agree |>
    dplyr::group_by(Question, Response) |>
    dplyr::summarise(Percent = weighted.mean(Percent, n)) |>
    dplyr::mutate(Question = factor(Question)) |>
    ggplot2::ggplot(ggplot2::aes(x = Question, y = Percent, fill = factor(Response))) +
    ggplot2::geom_col(color = NA, width = 0.95, position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = dplyr::if_else(Percent >= 10, paste0(round(Percent), "%"), ""),
        color = Response
      ),
      size = 6.5,
      position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE),
      fontface = "bold"
    ) +
    ggplot2::scale_fill_manual(values = c(
      "(1) Strongly disagree" = "#040404", "(2) Disagree" = "#032E3F",
      "(3) Neither agree nor disagree" = "#02587A", "(4) Agree" = "#0182B4", "(5) Strongly agree" = "#00ACF0"
    )) +
    ggplot2::scale_color_manual(values = c(
      "(1) Strongly disagree" = "white", "(2) Disagree" = "white",
      "(3) Neither agree nor disagree" = "black", "(4) Agree" = "black", "(5) Strongly agree" = "black"
    )) +
    ggplot2::labs(
      fill = "", title = glue::glue("Participant Perceptions of Course (n = {format(sum(n_size_agree$n), big.mark = ',')})"),
      x = "", y = ""
    ) +
    ggplot2::coord_flip() +
    ggplot2::guides(
      fill = ggplot2::guide_legend(),
      color = "none"
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    TeachingLab::theme_final_report() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 13, lineheight = 0.8),
      plot.title = ggplot2::element_text(size = 25),
      legend.margin = ggplot2::margin(-30, 0, 0, -190),
      legend.text = ggplot2::element_text(size = 12)
    )
  
  cat("#### ", race_filter, "\n")
  cat("\n")
  
  print(p)
  
  cat("\n")
  cat("In summary, we see the following % agree or strongly agree with the above statements:")
  cat("\n")
  cat("\n")
  
  cat(paste0("- ", facilitated_agree, " strongly agree or agree that they were satisfied with how the course was facilitated."))
  cat("\n")
  cat("\n")
  cat(paste0("- ", quality_agree, " strongly agree or agree that they were satisfied with the overall quality of the course."))
  cat("\n")
  cat(paste0("- ", community_agree, " strongly agree or agree that they felt a sense of community with the other participants in the course."))
  cat("\n")
  cat(paste0("- ", applied_agree, " strongly agree or agree that they will apply what they have learned in the course to their practice."))
  cat("\n")
  cat(paste0("- ", looked_agree, " strongly agree or agree that they looked forward to attending this PL."))
  cat("\n")
  cat(paste0("- ", talk_agree, " strongly agree or agree that they talk to other teachers about the things they learned in this PL."))
  cat(paste0("- ", present_agree, ' strongly agree or agree that they were fully present "minds-on" during these PL sessions.'))
  cat("\n")
  cat(paste0("- ", activities_agree, " strongly agree or agree that the activities were well-designed to help them meet the learning targets."))
  cat("\n")
  cat(paste0("- ", improve_supervision_agree, " strongly agree or agree that what they've learned in this course will improve my supervision of teachers."))
  cat("\n")
  cat(paste0("- ", improve_instruction_agree, " strongly agree or agree that what they've learned in this course will improve my instruction."))
  cat(paste0("- ", supported_agree, " strongly agree or agree that the course has supported me in being responsive to students' backgrounds, cultures, and points of view."))
  cat("\n")
  cat(paste0("- ", relevant_agree, " strongly agree or agree that the course was relevant to their instructional practices."))
  cat("\n")
  cat(paste0("- ", time_agree, " strongly agree or agree that the PL was a good use of their time."))
  
  cat("\n\n")
}

#' @title Ongoing Coaching feedback graph dependent on race and content area
#' @description Returns a barchart for selections in the relevant questions of the end of coaching survey
#' @param race_filter the filter to apply to the race column, one of "All", "White", "Black or African American", "Asian", "Hispanic/Latino", "More than one race", or "Other"
#' @param content_area_filter the content area to filter for, one otf "All", "ELA", "Math", or "Other"
#' @return prints a ggplot object
#' @export
ongoing_coaching_feedback_graph <- function(race_filter = "All", content_area_filter = "All") {
  if (content_area_filter == "All") {
    ongoing_coaching_2 <- ongoing_coaching
  } else {
    ongoing_coaching_2 <- ongoing_coaching |>
      dplyr::filter(content_area == content_area_filter)
  }
  if (race_filter == "All Races") {
    ongoing_coaching_3 <- ongoing_coaching_2
  } else {
    ongoing_coaching_3 <- ongoing_coaching_2 |>
      dplyr::filter(race == race_filter)
  }
  
  coaching_plot_agree <- ongoing_coaching_3 |>
    dplyr::select(
      `They demonstrated deep knowledge of the content they coach` = coach_ongoing_feed_1,
      `Their coaching is clear` = coach_ongoing_feed_2,
      `They seem fully prepared for the coaching sessions` = coach_ongoing_feed_3,
      `They effectively build a safe learning environment` = coach_ongoing_feed_4,
      `They make necessary adjustments based on my needs` = coach_ongoing_feed_5
    ) |>
    tidyr::pivot_longer(tidyr::everything(), names_to = "Question", values_to = "Response") |>
    tidyr::drop_na(Response) |>
    dplyr::group_by(Question, Response) |>
    dplyr::count(sort = T) |>
    dplyr::ungroup() |>
    dplyr::group_by(Question) |>
    dplyr::mutate(
      Percent = round(100 * n / sum(n), 2),
      Response = factor(Response, levels = c(
        "1 - Strongly disagree",
        "2 - Disagree",
        "3 - Neither agree nor disagree",
        "4 - Agree",
        "5 - Strongly agree"
      )),
      Question = TeachingLab::html_wrap(Question, n = 30)
    )
  
  n_size_agree <- ongoing_coaching_3 |>
    dplyr::count(sort = T)
  
  ### Percent Agree Strongly Agrees Aggregate ###
  coach_prepared_agree <- coaching_plot_agree |>
    TeachingLab::agree_strongly_agree(question = "They seem fully prepared for<br>the coaching sessions")
  coach_adjustments_agree <- coaching_plot_agree |>
    TeachingLab::agree_strongly_agree(question = "They make necessary<br>adjustments based on my needs")
  coach_effectively_agree <- coaching_plot_agree |>
    TeachingLab::agree_strongly_agree(question = "They effectively build a safe<br>learning environment")
  coach_knowledge_agree <- coaching_plot_agree |>
    TeachingLab::agree_strongly_agree(question = "They demonstrated deep<br>knowledge of the content they<br>coach")
  coach_clear_coaching_agree <- coaching_plot_agree |>
    TeachingLab::agree_strongly_agree(question = "Their coaching is clear")
  
  p <- coaching_plot_agree |>
    dplyr::mutate(
      Percent = round(100 * n / sum(n), 2),
      Question = factor(Question)
    ) |>
    ggplot2::ggplot(ggplot2::aes(
      x = forcats::fct_reorder(Question, Percent, .desc = T),
      y = Percent,
      color = Response,
      fill = Response
    )) +
    ggplot2::geom_col(color = NA, position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = dplyr::if_else(Percent >= 10, paste0(round(Percent), "%"), ""),
        color = Response
      ),
      position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE),
      fontface = "bold",
      size = 6.5
    ) +
    ggplot2::labs(
      x = "", y = "",
      title = glue::glue("Ongoing Coaching Participant Feedback (n = {sum(n_size_agree$n, na.rm = T)})"),
      fill = ""
    ) +
    ggplot2::scale_fill_manual(values = c(
      "1 - Strongly disagree" = "#040404",
      "2 - Disagree" = "#032E3F",
      "3 - Neither agree nor disagree" = "#02587A",
      "4 - Agree" = "#0182B4",
      "5 - Strongly agree" = "#00ACF0"
    )) +
    ggplot2::scale_color_manual(values = c(
      "1 - Strongly disagree" = "white",
      "2 - Disagree" = "black",
      "3 - Neither agree nor disagree" = "black",
      "4 - Agree" = "black",
      "5 - Strongly agree" = "black"
    )) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(),
      color = "none"
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(scale = 1),
      expand = c(0.14, 0)
    ) +
    ggplot2::coord_flip() +
    TeachingLab::theme_final_report() +
    ggplot2::theme(
      axis.text.y = ggtext::element_markdown(
        size = 17,
        margin = ggplot2::margin(r = -55)
      ),
      plot.title = ggplot2::element_text(size = 25),
      legend.margin = ggplot2::margin(-30, 0, 0, -140),
      legend.text = ggplot2::element_text(size = 15)
    )
  
  cat("#### ", race_filter, "\n")
  cat("\n")
  
  print(p)
  
  cat("\n")
  cat("In summary, we see the following % agree or strongly agree with the above statements:")
  cat("\n")
  cat("\n")
  
  cat(paste0("- ", coach_prepared_agree, " strongly agree or agree that the coach seemed fully prepared for the coaching sessions."))
  cat("\n")
  cat("\n")
  cat(paste0("- ", coach_adjustments_agree, " strongly agree or agree that the coaches made necessary adjustments based on their needs."))
  cat("\n")
  cat(paste0("- ", coach_effectively_agree, " strongly agree or agree that the coaches effectively built a safe learning environment."))
  cat("\n")
  cat(paste0("- ", coach_knowledge_agree, " strongly agree or agree that the coaches demonstrated deep knowledge of the content they coach."))
  cat("\n")
  cat(paste0("- ", coach_clear_coaching_agree, " strongly agree or agree that the coaching was clear."))
  
  
  cat("\n\n")
}

#' @title End of Coaching feedback graph dependent on race and content area
#' @description Returns a barchart for selections in the relevant questions of the end of coaching survey
#' @param race_filter the filter to apply to the race column, one of "All", "White", "Black or African American", "Asian", "Hispanic/Latino", "More than one race", or "Other"
#' @param content_area_filter the content area to filter for, one otf "All", "ELA", "Math", or "Other"
#' @return prints a ggplot object
#' @export
end_coaching_feedback_graph <- function(race_filter = "All", content_area_filter = "All") {
  if (content_area_filter == "All") {
    end_coaching_2 <- end_coaching
  } else {
    end_coaching_2 <- end_coaching |>
      dplyr::filter(content_area == content_area_filter)
  }
  if (race_filter == "All Races") {
    end_coaching_3 <- end_coaching_2
  } else {
    end_coaching_3 <- end_coaching_2 |>
      dplyr::filter(race == race_filter)
  }
  
  coaching_plot_agree <- end_coaching_3 |>
    dplyr::select(
      `I looked forward to attending the coaching sessions` = coach_end_feed_1,
      `I was fully present/"minds-on" during the coaching sessions` = coach_end_feed_2,
      `I am satisfied with the coaching I received` = coach_end_feed_3,
      `The coaching was a good use of my time` = coach_end_feed_4,
      `I talk to other teachers about the things I learned from coaching` = coach_end_feed_5,
      `The coaching support was relevant to my instructional practices` = coach_end_feed_6,
      `The strategies I've learned through coaching will improve my instruction` = coach_end_feed_7,
      `The strategies I've learned through coaching will improve my coaching or supervision of teachers` = coach_end_feed_8,
      `I have applied or will apply what I have learned through coaching to my practice` = coach_end_feed_9,
      `The coaching supported me in being responsive to students' backgrounds, cultures, and points of view` = coach_end_feed_10,
      `I am satisfied with the overall quality of the coaching I received` = coach_end_feed_11
    ) |>
    tidyr::pivot_longer(tidyr::everything(), names_to = "Question", values_to = "Response") |>
    tidyr::drop_na(Response) |>
    dplyr::group_by(Question, Response) |>
    dplyr::count(sort = T) |>
    dplyr::ungroup() |>
    dplyr::group_by(Question) |>
    dplyr::mutate(
      Percent = round(100 * n / sum(n), 2),
      Response = factor(Response, levels = c(
        "1 - Strongly disagree",
        "2 - Disagree",
        "3 - Neither agree nor disagree",
        "4 - Agree",
        "5 - Strongly agree"
      )),
      Question = TeachingLab::html_wrap(Question, n = 40)
    )
  
  n_size_agree <- end_coaching_3 |>
    dplyr::count(sort = T)
  
  ### Percent Agree Strongly Agrees ###
  coach_applied_agree <- coaching_plot_agree |>
    TeachingLab::agree_strongly_agree(question = "I have applied or will apply what I have<br>learned through coaching to my practice")
  coach_quality_agree <- coaching_plot_agree |>
    TeachingLab::agree_strongly_agree(question = "I am satisfied with the overall quality<br>of the coaching I received")
  coach_supported_agree <- coaching_plot_agree |>
    TeachingLab::agree_strongly_agree(question = "The coaching supported me in being<br>responsive to students' backgrounds,<br>cultures, and points of view")
  coach_looked_agree <- coaching_plot_agree |>
    TeachingLab::agree_strongly_agree(question = "I looked forward to attending the<br>coaching sessions")
  coach_talk_agree <- coaching_plot_agree |>
    TeachingLab::agree_strongly_agree(question = "I talk to other teachers about the<br>things I learned from coaching")
  coach_present_agree <- coaching_plot_agree |>
    TeachingLab::agree_strongly_agree(question = "I was fully present/\"minds-on\" during<br>the coaching sessions")
  coach_satisfied_agree <- coaching_plot_agree |>
    TeachingLab::agree_strongly_agree(question = "I am satisfied with the coaching I<br>received")
  coach_relevant_agree <- coaching_plot_agree |>
    TeachingLab::agree_strongly_agree(question = "The coaching support was relevant to my<br>instructional practices")
  coach_improve_agree <- coaching_plot_agree |>
    TeachingLab::agree_strongly_agree(question = "The strategies I've learned through<br>coaching will improve my instruction")
  coach_time_agree <- coaching_plot_agree |>
    TeachingLab::agree_strongly_agree(question = "The coaching was a good use of my time")
  coach_supervision_agree <- coaching_plot_agree |>
    TeachingLab::agree_strongly_agree(question = "The strategies I've learned through<br>coaching will improve my coaching or<br>supervision of teachers")
  
  p <- coaching_plot_agree |>
    dplyr::mutate(
      Percent = round(100 * n / sum(n), 2),
      Question = factor(Question)
    ) |>
    ggplot2::ggplot(ggplot2::aes(
      x = fct_reorder(Question, Percent, .desc = T),
      y = Percent,
      color = Response,
      fill = Response
    )) +
    ggplot2::geom_col(color = NA, position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = dplyr::if_else(Percent >= 10, paste0(round(Percent), "%"), ""),
        color = Response
      ),
      position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE),
      fontface = "bold",
      size = 6.5
    ) +
    ggplot2::labs(
      x = "", y = "",
      title = glue::glue("End of Coaching Participant Feedback (n = {sum(n_size_agree$n, na.rm = T)})"),
      fill = ""
    ) +
    ggplot2::scale_fill_manual(values = c(
      "1 - Strongly disagree" = "#040404",
      "2 - Disagree" = "#032E3F",
      "3 - Neither agree nor disagree" = "#02587A",
      "4 - Agree" = "#0182B4",
      "5 - Strongly agree" = "#00ACF0"
    )) +
    ggplot2::scale_color_manual(values = c(
      "1 - Strongly disagree" = "white",
      "2 - Disagree" = "black",
      "3 - Neither agree nor disagree" = "black",
      "4 - Agree" = "black",
      "5 - Strongly agree" = "black"
    )) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(),
      color = "none"
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(scale = 1),
      expand = c(0.14, 0)
    ) +
    ggplot2::coord_flip() +
    TeachingLab::theme_final_report() +
    ggplot2::theme(
      axis.text.y = element_markdown(
        size = 13, lineheight = 1,
        margin = ggplot2::margin(r = -60)
      ),
      plot.title = ggplot2::element_text(size = 25),
      legend.margin = ggplot2::margin(-30, 0, 0, -120),
      legend.text = ggplot2::element_text(size = 13)
    )
  
  cat("#### ", race_filter, "\n")
  cat("\n")
  
  print(p)
  
  cat("\n")
  cat("In summary, we see the following % agree or strongly agree with the above statements:")
  cat("\n")
  cat("\n")
  
  cat(paste0("- ", coach_applied_agree, " strongly agree or agree that they have applied or will apply what they have learned through coaching to their practice."))
  cat("\n")
  cat("\n")
  cat(paste0("- ", coach_quality_agree, " strongly agree or agree that they were satisfied with the overall quality of the coaching they received."))
  cat("\n")
  cat(paste0("- ", coach_supported_agree, " strongly agree or agree that the coaching supported me in being responsive to students' backgrounds, cultures, and points of view."))
  cat("\n")
  cat(paste0("- ", coach_looked_agree, " strongly agree or agree that they looked forward to attending the coaching sessions."))
  cat("\n")
  cat(paste0("- ", coach_talk_agree, " strongly agree or agree that they talk to other teachers about the things they learned from coaching."))
  cat("\n")
  cat(paste0("- ", coach_present_agree, ' strongly agree or agree that they were fully present/"minds-on" during the coaching sessions.'))
  cat(paste0("- ", coach_satisfied_agree, " strongly agree or agree that they were satisfied with the coaching they received."))
  cat("\n")
  cat(paste0("- ", coach_relevant_agree, " strongly agree or agree that the coaching was relevant to their instructional practices."))
  cat("\n")
  cat(paste0("- ", coach_improve_agree, " strongly agree or agree that the strategies they've learned through coaching will improve their instruction."))
  cat("\n")
  cat(paste0("- ", coach_time_agree, " strongly agree or agree that the coaching was a good use of their time."))
  cat(paste0("- ", coach_supervision_agree, " strongly agree or agree that the strategies they've learned through coaching will improve their coaching or supervision or teachers."))
  
  cat("\n\n")
}

#' @title Useful text selector
#' @description Gets all text over a specified nps and tibble
#' @param data the data to filter the text for
#' @param text_col the text column to select
#' @param name the new name for the text column
#' @param n 15 observations to return by default
#' @return returns a single tibble column
#' @export
select_useful_text <- function(data, text_col, name, n = 15) {
  
  data |>
    dplyr::arrange(dplyr::desc(nps), dplyr::desc(stringr::str_length( {{text_col}} ))) |>
    dplyr::select({{ name }} := {{ text_col }}) |>
    tidyr::drop_na() |>
    dplyr::filter(!!rlang::sym(name) %!in% na_df) |>
    dplyr::filter(stringr::str_length(!!rlang::sym(name)) > 10) |>
    dplyr::slice_sample(n = 15) |>
    dplyr::distinct(!!rlang::sym(name))
}