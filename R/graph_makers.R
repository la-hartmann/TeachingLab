#' @title Graph maker for score comparisons
#' @description Makes a ggplot comparison table
#'
#' @param data the data to use
#' @param question the question column to use for comparison
#' @param order the order to keep the questions in
#' @param prepost The column to compare before and after
#' @param score the scores to compare
#' @param split_variable the text detected in the variable to create segments with (from prepost)
#' @param title the title of the gt
#' @return a colored gt table
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' score_compare_plot(data, question, prepost, score)
#' }
#' @export

score_compare_plot <- function(data, question, order, prepost, score, split_variable, title) {
  data_wrapped <- data %>%
    dplyr::mutate(Question = factor(.data[[question]], levels = c(order))) %>%
    dplyr::mutate(Question = TeachingLab::html_wrap(Question, 25))

  graph_segments <- tibble::tibble(
    x = data_wrapped %>% dplyr::filter(stringr::str_detect(prepost, split_variable[1])) %>% dplyr::pull(score),
    xend = data_wrapped %>% dplyr::filter(stringr::str_detect(prepost, split_variable[2])) %>% dplyr::pull(score),
    y = data_wrapped %>% dplyr::filter(stringr::str_detect(prepost, split_variable[1])) %>% dplyr::pull(Question),
    yend = data_wrapped %>% dplyr::filter(stringr::str_detect(prepost, split_variable[2])) %>% dplyr::pull(Question),
    fall_text = data_wrapped %>% dplyr::filter(stringr::str_detect(prepost, split_variable[1])) %>% dplyr::pull(score),
    spring_text = data_wrapped %>% dplyr::filter(stringr::str_detect(prepost, split_variable[2])) %>% dplyr::pull(score)
  )

  ggplot2::ggplot() +
    ggplot2::geom_point(data = data_wrapped, mapping = ggplot2::aes(color = prepost, x = score, y = Question, size = score)) +
    ggplot2::geom_segment(data = graph_segments, mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = yend, alpha = 0.7), color = "black", size = 0.3, arrow = grid::arrow(length = grid::unit(0.1, "inches"))) +
    ggtext::geom_richtext(
      data = graph_segments, fill = NA, label.color = NA,
      ggplot2::aes(x = fall_text, y = y, label = paste0(x, "%")),
      color = "#ff7b43", vjust = -0.5, size = 4.85
    ) +
    ggtext::geom_richtext(
      data = graph_segments, fill = NA, label.color = NA,
      ggplot2::aes(x = spring_text, y = y, label = paste0(xend, "%")),
      color = "#00acf0", vjust = -0.5, size = 4.85
    ) +
    ggplot2::scale_color_manual(values = c("#ff7b43", "#00acf0")) +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(scale = 1), breaks = scales::pretty_breaks(n = 5),
      limits = c(0, 100)
    ) +
    ggplot2::labs(
      x = NULL, y = NULL,
      title = glue::glue("{title}")
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.border = ggplot2::element_blank(),
      legend.position = "none",
      plot.title = ggtext::element_markdown(hjust = 0.5, family = "Calibri", size = 20, lineheight = 1.15),
      text = ggplot2::element_text(family = "Calibri"),
      axis.text.y = ggtext::element_markdown(hjust = 0.5, lineheight = 1.1, size = 14),
      axis.text.x = ggtext::element_markdown(size = 14)
    )
}


#' @title Knowledge Assessment Graph Summary
#' @description Creates a graph specifically for Knowledge Assessments in mid year reports
#' @param data the data
#' @param know_assess the knowledge assessment to make plot for
#' @param summary_path optional path to save plot to a file, if NULL does not save anywhere
#' @return a ggplot
#' @export
know_assess_summary <- function(data, know_assess, summary_path = "report_summary_images") {
  plot_data <- data |>
    dplyr::filter(know_assess == !!rlang::enquo(know_assess)) |>
    dplyr::select(-site) |> # Get rid of site for when there is more than one
    dplyr::group_by(prepost) |>
    dplyr::summarise(percent = mean(percent, na.rm = T)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      name = ifelse(prepost == "pre",
        "Before",
        "After"
      ),
      value = percent,
      name = factor(name, levels = c("Before", "After"))
    ) |>
    dplyr::select(name, value)

  ### Make sure no over 100's ###
  plot_data$value <- ifelse(plot_data$value >= 100, 100, plot_data$value)

  title <- stringr::str_to_title(stringr::str_replace_all(know_assess, "_", " ")) |>
    stringr::str_replace_all("Ela", "ELA") |>
    stringr::str_replace_all("Eic", "EIC") # Correct title casing

  n1 <- data |>
    dplyr::filter(know_assess == !!rlang::enquo(know_assess) & prepost == "pre") |>
    # dplyr::pull(id) |>
    nrow()

  n2 <- data |>
    dplyr::filter(know_assess == !!rlang::enquo(know_assess) & prepost == "post") |>
    # dplyr::pull(id) |>
    nrow()

  if (length(n1) == 0) {
    n1 <- 0
  }

  if (length(n2) == 0) {
    n2 <- 0
  }

  p <- plot_data |>
    dplyr::mutate(value = 100 * value) |>
    ggplot2::ggplot(ggplot2::aes(x = name, y = value, fill = name)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(value), "%")),
      vjust = -1,
      fontface = "bold",
      family = "Calibri"
    ) +
    ggplot2::scale_fill_manual(values = c("Before" = "#D17DF7", "After" = "#55BBC7")) +
    # ggtext::geom_richtext(data = data.frame(name = "Before", value = 100,
    #                                         label = "% Correct <b style='color:#d17df7'>before</b> and <b style='color:#55bbc7'>after</b>."),
    #                       aes(x = name, y = value, label = label)) +
    ggplot2::labs(
      x = "", y = "",
      # title = paste0(title, "\n% Correct before and after")#,
      title = paste0(title, "<br>% Correct <b style='color:#d17df7'>before (n = ", n1, ")</b> and <b style='color:#55bbc7'>after (n = ", n2, ")</b>")
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(scale = 1), expand = c(0.1, 0),
      limits = c(0, 100)
    ) +
    # TeachingLab::theme_tl(markdown = F) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      # plot.title = ggplot2::element_text(),
      plot.title = ggtext::element_markdown(lineheight = 1.1, hjust = 0.5),
      legend.position = "none",
      axis.text.x = ggplot2::element_text(face = "bold")
    )

  if (!is.null(summary_path)) {
    ggplot2::ggsave(
      plot = p,
      filename = glue::glue("{know_assess}.png"),
      path = glue::glue("images/{summary_path}"),
      bg = "white",
      device = "png",
      height = 5, width = 5
    )
  } else {
    return(p)
  }
}

#' @title IPG Forms Grapher
#' @description Creates a graph specifically for IPG Forms data
#' @param data the data
#' @param name the column name for the data frame to focus on
#' @param height height
#' @param width width
#' @param save_name the name to save
#' @param wrap passes to str_wrap for the title
#' @param sizing the base text size multiplier
#' @param dpi the dpi to save with
#' @param numeric if it is numeric reorder the factors
#' @param split if it is sequenced by commas split it
#' @param save FALSE
#' @param factor_level option for factor levels
#' @return a ggplot
#' @export
ipg_plot <- function(data, name, save_name, height = 5, width = 8.5, wrap = 60, sizing = 1, dpi = 300,
                     split = F, numeric = F, factor_level = NULL, save = FALSE) {
  n <- data |>
    dplyr::filter(name == {{ name }}) |>
    dplyr::ungroup() |>
    dplyr::summarise(n = sum(n)) |>
    dplyr::select(n) |>
    as.vector()

  plot_data <- data %>%
    dplyr::filter(name == {{ name }}) %>%
    dplyr::ungroup() %>%
    {
      if (split == T) {
        dplyr::mutate(., value = stringr::str_split(value, ", ")) %>%
          tidyr::unnest(value) %>%
          dplyr::mutate(value = stringr::str_remove_all(value, ",")) %>%
          dplyr::group_by(value) %>%
          dplyr::summarise(
            name = name,
            n = sum(n),
            percent = sum(percent)
          ) %>%
          dplyr::mutate(percent = replace(percent, percent == 99, 100)) %>%
          dplyr::distinct(value, .keep_all = T)
      } else {
        .
      }
    } %>%
    dplyr::mutate(value = stringr::str_wrap(value, 30)) %>%
    {
      if (numeric == T) {
        dplyr::mutate(.,
          number = readr::parse_number(value),
          value = factor(value),
          value = forcats::fct_reorder(value, number)
        )
      } else {
        .
      }
    } %>%
    {
      if (numeric == F) {
        dplyr::mutate(.,
          value = factor(value),
          value = forcats::fct_reorder(value, percent)
        )
      } else {
        .
      }
    } %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      color = ifelse(percent < mean(percent, na.rm = T), "white", "black"),
      color = ifelse(percent < 10, "black", color)
    )

  if (missing(factor_level)) {
    plot_data
  } else if (factor_level == "ac1") {
    plot_data$value <- factor(plot_data$value,
      levels = c(
        "1- Rarely/Never",
        "2- Sometimes",
        "3- Often",
        "4- Always"
      )
    )
  } else if (factor_level == "ca2a") {
    plot_data$value <- factor(plot_data$value,
      levels = stringr::str_wrap(c(
        "1- Instruction is not focused on the mathematics of the lesson.",
        "2- Instruction is limited to showing students how to get the answer.",
        "3- Examples are used to make the mathematics of the lesson clear.",
        "4- A variety of instructional techniques and examples are used to make the mathematics of the lesson clear."
      ), 30)
    )
  } else if (factor_level == "ca2b") {
    plot_data$value <- factor(plot_data$value,
      levels = stringr::str_wrap(c(
        "1- Student solution methods are not shared.",
        "2- Student solution methods are shared, but few connections are made to strengthen student understanding.",
        "3- Student solution methods are shared, and some mathematical connections are made between them.",
        "4- Student solution methods are shared, and connections to the mathematics are explicit and purposeful. If applicable, connections between the methods are examined."
      ), 30)
    )
  } else if (factor_level == "ca3a") {
    plot_data$value <- factor(plot_data$value,
      levels = stringr::str_wrap(c(
        "1- Teacher provides few or no opportunities, or few or very few students take the opportunities provided.",
        "2- Teacher provides some opportunities, and some students take them.",
        "3- Teacher provides many opportunities, and some students take them; or teacher provides some opportunities and most students take them.",
        "4- Teacher provides many opportunities, and most students take them."
      ), 30)
    )
  } else if (factor_level == "td") {
    plot_data <- plot_data |>
      dplyr::mutate(value = stringr::str_replace_all(value, c(
        "2- Often" = "3- Often",
        "3- Sometimes" = "2- Sometimes"
      )))

    plot_data$value <- factor(plot_data$value,
      levels = c(
        "1- Rarely/Never",
        "2- Sometimes",
        "3- Often",
        "4- Always"
      )
    )

    plot_data <- plot_data |>
      dplyr::group_by(value) |>
      dplyr::summarise(
        n = sum(n),
        percent = sum(percent)
      ) |>
      dplyr::mutate(
        name = plot_data$name[1],
        color = ifelse(percent < mean(percent, na.rm = T), "white", "black")
      )
  } else if (factor_level == "sp") {
    plot_data$value <- factor(plot_data$value,
      levels = c(
        "1- Few/No",
        "2- Some",
        "3- Most",
        "4- All"
      )
    )
  } else if (factor_level == "ca1a") {
    plot_data$value <- factor(plot_data$value,
      levels = c(
        "No- The enacted lesson focuses on mathematics outside the grade-level/ course-level standards.",
        "Yes- The enacted lesson focuses only on mathematics within the grade-level/ course-level standards."
      )
    )
  } else if (factor_level == "ca1b") {
    plot_data$value <- factor(plot_data$value,
      levels = c(
        "No- The enacted lesson does not connect or has weak connections to students' prior skills and understandings.",
        "Yes- The enacted lesson builds on students' prior skills and understandings."
      )
    )
  } else if (factor_level == "ca1c") {
    plot_data$value <- factor(plot_data$value,
      levels = c(
        "No- The enacted lesson targets aspects of Rigor that are not appropriate for the standard(s) being addressed.",
        "Yes- The enacted lesson explicitly targets the aspect(s) of Rigor called for by the standard(s) being addressed."
      )
    )
  }

  p <- plot_data |>
    ggplot2::ggplot(ggplot2::aes(
      x = value,
      y = percent,
      fill = percent
    )) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = paste0(percent, "%"), color = color),
      hjust = ifelse(plot_data$percent > 5, 1.25, -0.2), size = 5 * sizing,
      fontface = "bold"
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(scale = 1), expand = c(0.1, 0),
      limits = c(0, 100), breaks = scales::breaks_pretty(n = 4)
    ) +
    ggplot2::scale_x_discrete(drop = F, limits = levels(plot_data$value)) +
    ggplot2::labs(y = "", x = "", title = stringr::str_wrap(name, wrap), caption = paste0("n = ", n)) +
    ggplot2::scale_color_manual(values = c("black", "white")) +
    TeachingLab::theme_tl() +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(size = 10 * sizing),
      axis.text.x = ggplot2::element_text(size = 15 * sizing),
      axis.text.y = ggplot2::element_text(size = 15 * sizing),
      plot.title = ggplot2::element_text(hjust = 0, face = "bold", size = 18 * sizing)
    )

  if (save == T) {
    ggplot2::ggsave(glue::glue("images/sy21_22/ipg_forms/{save_name}.png"), width = width, height = height, bg = "white", dpi = dpi)
  }

  return(p)
}

#' @title IPG Forms Grapher
#' @description Creates a graph specifically for IPG Forms data
#' @param data the data
#' @param name the column name for the data frame to focus on
#' @param wrap passes to str_wrap for the title
#' @param sizing the base text size multiplier
#' @param numeric if it is numeric reorder the factors
#' @param split if it is sequenced by commas split it
#' @param factor_level option for factor levels
#' @return a ggplot object
#' @export
dashboard_ipg_plot <- function(data, name, wrap = 60, sizing = 1,
                               split = F, numeric = F, factor_level = NULL) {
  n <- data %>%
    dplyr::filter(name == {{ name }}) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(n = sum(n)) %>%
    dplyr::select(n) %>%
    as.vector()

  plot_data <- data %>%
    dplyr::filter(name == {{ name }}) %>%
    dplyr::ungroup() %>%
    {
      if (split == T) {
        dplyr::mutate(., value = stringr::str_split(value, ", ")) %>%
          tidyr::unnest(value) %>%
          dplyr::mutate(value = stringr::str_remove_all(value, ",")) %>%
          dplyr::group_by(value) %>%
          dplyr::summarise(
            name = name,
            n = sum(n),
            percent = sum(percent)
          ) %>%
          dplyr::mutate(percent = replace(percent, percent == 99, 100)) %>%
          dplyr::distinct(value, .keep_all = T)
      } else {
        .
      }
    } %>%
    dplyr::mutate(value = stringr::str_wrap(value, 30)) %>%
    {
      if (numeric == T) {
        dplyr::mutate(.,
          number = readr::parse_number(value),
          value = factor(value),
          value = forcats::fct_reorder(value, number)
        )
      } else {
        .
      }
    } %>%
    {
      if (numeric == F) {
        dplyr::mutate(.,
          value = factor(value),
          value = forcats::fct_reorder(value, percent)
        )
      } else {
        .
      }
    } %>%
    dplyr::ungroup() %>%
    dplyr::mutate(color = ifelse(percent < mean(percent, na.rm = T), "white", "black")) %>%
    print()

  if (missing(factor_level)) {
    plot_data
  } else if (factor_level == "ac1") {
    plot_data$value <- factor(plot_data$value,
      levels = c(
        "1- Rarely/Never",
        "2- Sometimes",
        "3- Often",
        "4- Always"
      )
    )
  } else if (factor_level == "ca2a") {
    plot_data$value <- factor(plot_data$value,
      levels = stringr::str_wrap(c(
        "1- Instruction is not focused on the mathematics of the lesson.",
        "2- Instruction is limited to showing students how to get the answer.",
        "3- Examples are used to make the mathematics of the lesson clear.",
        "4- A variety of instructional techniques and examples are used to make the mathematics of the lesson clear."
      ), 30)
    )
  } else if (factor_level == "ca2b") {
    plot_data$value <- factor(plot_data$value,
      levels = stringr::str_wrap(c(
        "1- Student solution methods are not shared.",
        "2- Student solution methods are shared, but few connections are made to strengthen student understanding.",
        "3- Student solution methods are shared, and some mathematical connections are made between them.",
        "4- Student solution methods are shared, and connections to the mathematics are explicit and purposeful. If applicable, connections between the methods are examined."
      ), 30)
    )
  } else if (factor_level == "ca3a") {
    plot_data$value <- factor(plot_data$value,
      levels = stringr::str_wrap(c(
        "1- Teacher provides few or no opportunities, or few or very few students take the opportunities provided.",
        "2- Teacher provides some opportunities, and some students take them.",
        "3- Teacher provides many opportunities, and some students take them; or teacher provides some opportunities and most students take them.",
        "4- Teacher provides many opportunities, and most students take them."
      ), 30)
    )
  }

  p <- plot_data |>
    ggplot2::ggplot(ggplot2::aes(
      x = value,
      y = percent,
      fill = percent
    )) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = paste0(percent, "%"), color = color),
      hjust = 1.25, size = 5 * sizing,
      fontface = "bold"
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1), expand = c(0.1, 0)) +
    ggplot2::scale_x_discrete(drop = F, limits = levels(plot_data$value)) +
    ggplot2::labs(y = "", x = "", title = stringr::str_wrap(name, wrap), caption = paste0("n = ", n)) +
    ggplot2::scale_color_manual(values = c("black", "white")) +
    TeachingLab::theme_tl() +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(size = 10 * sizing),
      axis.text.x = ggplot2::element_text(size = 15 * sizing),
      axis.text.y = ggplot2::element_text(size = 15 * sizing),
      plot.title = ggplot2::element_text(hjust = 0, face = "bold", size = 18 * sizing)
    )


  return(p)
}

#' @title IPG Forms Grapher
#' @description Creates a graph specifically for IPG Forms data
#' @param data the data
#' @param name the column name for the data frame to focus on
#' @param wrap passes to str_wrap for the title
#' @param sizing the base text size multiplier
#' @param numeric if it is numeric reorder the factors
#' @param split if it is sequenced by commas split it
#' @param factor_level option for factor levels
#' @return a ggplot object
#' @export
dashboard_ipg_plot_ts <- function(data, name, wrap = 60, sizing = 1,
                                  split = F, numeric = F, factor_level = NULL) {
  n <- data %>%
    dplyr::filter(name == {{ name }}) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(`Timeline of Obs`) %>%
    dplyr::summarise(n = sum(n)) %>%
    dplyr::select(n) %>%
    as.vector()

  plot_data <- data %>%
    dplyr::filter(name == {{ name }}) %>%
    dplyr::ungroup() %>%
    {
      if (split == T) {
        dplyr::mutate(., value = stringr::str_split(value, ", ")) %>%
          tidyr::unnest(value) %>%
          dplyr::mutate(value = stringr::str_remove_all(value, ",")) %>%
          dplyr::group_by(value) %>%
          dplyr::summarise(
            name = name,
            n = sum(n),
            percent = sum(percent)
          ) %>%
          dplyr::mutate(percent = replace(percent, percent == 99, 100)) %>%
          dplyr::distinct(value, .keep_all = T)
      } else {
        .
      }
    } %>%
    dplyr::mutate(value = stringr::str_wrap(value, 30)) %>%
    {
      if (numeric == T) {
        dplyr::mutate(.,
          number = readr::parse_number(value),
          value = factor(value),
          value = forcats::fct_reorder(value, number)
        )
      } else {
        .
      }
    } %>%
    {
      if (numeric == F) {
        dplyr::mutate(.,
          value = factor(value),
          value = forcats::fct_reorder(value, percent)
        )
      } else {
        .
      }
    } %>%
    dplyr::ungroup() %>%
    dplyr::mutate(color = ifelse(percent < mean(percent, na.rm = T), "white", "black")) %>%
    print()

  if (missing(factor_level)) {
    plot_data
  } else if (factor_level == "ac1") {
    plot_data$value <- factor(plot_data$value,
      levels = c(
        "1- Rarely/Never",
        "2- Sometimes",
        "3- Often",
        "4- Always"
      )
    )
  } else if (factor_level == "ca2a") {
    plot_data$value <- factor(plot_data$value,
      levels = stringr::str_wrap(c(
        "1- Instruction is not focused on the mathematics of the lesson.",
        "2- Instruction is limited to showing students how to get the answer.",
        "3- Examples are used to make the mathematics of the lesson clear.",
        "4- A variety of instructional techniques and examples are used to make the mathematics of the lesson clear."
      ), 30)
    )
  } else if (factor_level == "ca2b") {
    plot_data$value <- factor(plot_data$value,
      levels = stringr::str_wrap(c(
        "1- Student solution methods are not shared.",
        "2- Student solution methods are shared, but few connections are made to strengthen student understanding.",
        "3- Student solution methods are shared, and some mathematical connections are made between them.",
        "4- Student solution methods are shared, and connections to the mathematics are explicit and purposeful. If applicable, connections between the methods are examined."
      ), 30)
    )
  } else if (factor_level == "ca3a") {
    plot_data$value <- factor(plot_data$value,
      levels = stringr::str_wrap(c(
        "1- Teacher provides few or no opportunities, or few or very few students take the opportunities provided.",
        "2- Teacher provides some opportunities, and some students take them.",
        "3- Teacher provides many opportunities, and some students take them; or teacher provides some opportunities and most students take them.",
        "4- Teacher provides many opportunities, and most students take them."
      ), 30)
    )
  }

  ts <- plot_data %>%
    dplyr::group_by(
      `Timeline of Obs`,
      value
    ) %>%
    dplyr::summarise(
      value = value,
      percent = sum(percent)
    )
  p <- ggplot2::ggplot(ggplot2::aes(
    x = `Timeline of Obs`,
    y = percent,
    fill = value
  )) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = paste0(percent, "%"), color = color), hjust = 1.25, size = 5 * sizing) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1), expand = c(0.1, 0)) +
    ggplot2::scale_x_discrete(drop = F, limits = levels(plot_data$value)) +
    ggplot2::labs(y = "", x = "", title = stringr::str_wrap(name, wrap), caption = paste0("n = ", n)) +
    ggplot2::scale_color_manual(values = c("black", "white")) +
    TeachingLab::theme_tl() +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(size = 10 * sizing),
      axis.text.x = ggplot2::element_text(size = 15 * sizing),
      axis.text.y = ggplot2::element_text(size = 15 * sizing),
      plot.title = ggplot2::element_text(hjust = 0, face = "bold", size = 18 * sizing)
    )


  return(p)
}


#' @title GT or ggplot maker
#' @description makes a gt table with percent and n colored
#' @param df the data frame
#' @param column the column to get count and percent from
#' @param custom_title the title for the table
#' @param no_title make the table have no title
#' @param base_font overall table font size
#' @param heading_font title font size
#' @param custom_column_name a custom name for the column
#' @param viz_type gt by default, also has ggplot options like pie chart, waffle, or treemap
#' @return a gt table
#' @export
gt_percent_n <- function(df, column, custom_title, no_title = T, base_font = 10,
                         heading_font = 14, custom_column_name = "", viz_type = "gt") {
  column <- rlang::sym(column)

  if (viz_type == "gt") {
    df |>
      dplyr::group_by(!!column) |>
      dplyr::count(sort = T) |>
      tidyr::drop_na(!!column) |>
      dplyr::ungroup() |>
      dplyr::mutate(Percent = round(100 * n / sum(n), 2)) |>
      dplyr::rename({{ custom_column_name }} := {{ column }}) |>
      gt::gt() |>
      gt::cols_label({{ custom_column_name }} := gt::html(custom_column_name)) %>%
      {
        if (no_title == F) gt::tab_header(title = gt::md(glue::glue("*{custom_title}*"))) else .
      } %>%
      gt::data_color(
        columns = n,
        colors = scales::col_numeric(
          palette = TeachingLab::tl_palette(color = "blue", n = 10),
          domain = NULL
        )
      ) |>
      gt::fmt_percent(
        columns = Percent,
        decimals = 2,
        scale_values = F
      ) |>
      gt::grand_summary_rows(
        columns = c(n),
        fns = list(
          Total = ~ sum(.)
        ),
        formatter = gt::fmt_number,
        decimals = 0
      ) |>
      gt::grand_summary_rows(
        columns = c(Percent),
        fns = list(
          Total = ~ sum(.)
        ),
        formatter = gt::fmt_percent,
        scale_values = F,
        decimals = 0
      ) %>%
      TeachingLab::gt_theme_tl(base_font = base_font, heading_font = heading_font)
  } else if (viz_type == "pie") {
    ggplot_data <- df |>
      dplyr::group_by(!!column) |>
      dplyr::count(sort = T) |>
      tidyr::drop_na(!!column) |>
      dplyr::ungroup() |>
      dplyr::mutate(!!(column) := stringr::str_wrap(!!rlang::sym(column), width = 10),
        Percent = round(100 * n / sum(n), 2)
      ) |>
      dplyr::rename({{ custom_column_name }} := {{ column }}) |>
      dplyr::mutate(
        prop = 100 * (Percent / sum(Percent)),
        ypos = cumsum(prop) - 0.5 * prop,
        {{ custom_column_name }} := forcats::fct_reorder(!!rlang::ensym(custom_column_name), Percent)
      )
    ggplot_data |>
      ggplot2::ggplot(ggplot2::aes(
        x = "", y = Percent,
        fill = !!rlang::ensym(custom_column_name)
      )) +
      ggplot2::geom_col(key_glyph = draw_key_point) +
      ggplot2::geom_text(
        ggplot2::aes(
          label = paste0(Percent, "%"),
          y = ypos
        ),
        family = "Calibri",
        fontface = "bold",
        color = ifelse(min(ggplot_data$Percent) == ggplot_data$Percent | ggplot_data$Percent < 10,
          "white",
          "black"
        ),
        size = ifelse(min(ggplot_data$Percent) == ggplot_data$Percent | ggplot_data$Percent < 10,
          4,
          6
        ),
        vjust = ifelse(min(ggplot_data$Percent) == ggplot_data$Percent, -1.5, 0.5)
      ) +
      ggplot2::coord_polar("y", start = 0) +
      ggplot2::labs(title = paste0(custom_column_name, " (n = ", format(sum(ggplot_data$n, na.rm = T), big.mark = ","), ")")) +
      ggplot2::scale_fill_manual(values = TeachingLab::tl_palette(
        color = "blue",
        n = length(unique(ggplot_data[[custom_column_name]]))
      )) +
      ggplot2::theme_void(base_family = "Calibri") +
      ggplot2::theme(
        legend.position = "bottom",
        legend.title = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
      ) +
      ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(shape = 21, size = 10), reverse = T))
  } else if (viz_type == "waffle") {
    ggplot_data <- df |>
      dplyr::group_by(!!column) |>
      dplyr::count(sort = T) |>
      tidyr::drop_na(!!column) |>
      dplyr::ungroup() |>
      dplyr::mutate(Percent = round(100 * n / sum(n), 2)) |>
      dplyr::rename({{ custom_column_name }} := {{ column }}) |>
      dplyr::mutate(
        prop = 100 * (Percent / sum(Percent)),
        ypos = cumsum(prop) - 0.75 * prop,
        {{ custom_column_name }} := forcats::fct_reorder(!!rlang::ensym(custom_column_name), Percent)
      )

    subtitle <- ggplot_data |>
      dplyr::select({{ custom_column_name }}, Percent, n) |>
      dplyr::arrange(dplyr::desc(Percent)) |>
      dplyr::mutate(color = rev(TeachingLab::tl_palette(n = length(n), color = "blue"))) |>
      dplyr::summarise(text = stringr::str_c("<b style='color:", color, "'>", !!rlang::ensym(custom_column_name), ": ", Percent, "%</b>", collapse = "<br>")) |>
      dplyr::pull(text)

    ggplot_data |>
      ggplot2::ggplot(ggplot2::aes(
        fill = !!rlang::ensym(custom_column_name),
        values = n
      )) +
      ### ISSUE: some kind of weird error here internal to the library itself I think ###
      waffle::geom_waffle(
        n_rows = 10,
        size = 1, colour = "white",
        make_proportional = TRUE,
        na.rm = TRUE,
        radius = grid::unit(2, "pt"),
        height = 0.9,
        width = 0.9
      ) +
      ggplot2::labs(
        title = paste0(custom_column_name, " (n = ", format(sum(ggplot_data$n, na.rm = TRUE), big.mark = ","), ")"),
        subtitle = subtitle
      ) +
      ggplot2::scale_fill_manual(values = TeachingLab::tl_palette(
        color = "blue",
        n = length(unique(ggplot_data[[custom_column_name]]))
      )) +
      ggplot2::theme_void(base_family = "Calibri") +
      ggplot2::theme(
        legend.position = "none",
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = ggtext::element_markdown(
          hjust = 0.5, face = "italic",
          lineheight = 1.15
        )
      )
  } else if (viz_type == "treemap") {
    ggplot_data <- df |>
      dplyr::group_by(!!column) |>
      dplyr::count(sort = T) |>
      tidyr::drop_na(!!column) |>
      dplyr::ungroup() |>
      dplyr::mutate(Percent = round(100 * n / sum(n), 2)) |>
      dplyr::rename({{ custom_column_name }} := {{ column }}) |>
      dplyr::mutate(
        prop = 100 * (Percent / sum(Percent)),
        ypos = cumsum(prop) - 0.5 * prop,
        {{ custom_column_name }} := forcats::fct_reorder(!!rlang::ensym(custom_column_name), Percent)
      )
    ggplot_data |>
      ggplot2::ggplot(ggplot2::aes(
        area = Percent,
        fill = !!rlang::ensym(custom_column_name)
      )) +
      treemapify::geom_treemap(key_glyph = draw_key_point) +
      treemapify::geom_treemap_text(ggplot2::aes(label = paste0(!!rlang::ensym(custom_column_name), ": ", Percent, "%")),
        family = "Calibri",
        fontface = "bold",
        # grow = T,
        reflow = T,
        color = ifelse(min(ggplot_data$Percent) == ggplot_data$Percent | ggplot_data$Percent < 10,
          "white",
          "black"
        ),
        place = "center"
      ) +
      ggplot2::labs(title = paste0(custom_column_name, " (n = ", format(sum(ggplot_data$n, na.rm = T), big.mark = ","), ")\n")) +
      ggplot2::scale_fill_manual(values = TeachingLab::tl_palette(
        color = "blue",
        n = length(unique(ggplot_data[[custom_column_name]]))
      )) +
      ggplot2::theme_void(base_family = "Calibri") +
      ggplot2::theme(
        legend.position = "none",
        legend.title = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
      )
  }
}


#' @title Fake p1, p2, n1, n2 data
#' @description Creates fake data for knowledge assessments in SXSW report
#' @param p1_range the range of values to sample for p1
#' @param p2_range the range of values to sample for p2
#' @param n1_range the range of values to sample for n1
#' @param n2_range the range of values to sample for n2
#' @return a randomised dataset
#' @export
p_and_n <- function(p1_range = c(40:80),
                    p2_range = c(70:100),
                    n1_range = stats::rnorm(1, mean = 80, sd = 10),
                    n2_range = stats::rnorm(1, mean = 50, sd = 10)) {
  df <- tibble::tibble(
    p1 = sample(p1_range, size = 1),
    p2 = sample(p2_range, size = 1),
    n1 = sample(n1_range, size = 1),
    n2 = sample(n2_range, size = 1)
  )

  while (df$n2 > df$n1) {
    df$n2 <- sample(n2_range, size = 1)
  }

  while (df$n1 < 10) {
    df$n1 <- sample(n1_range, size = 1)
  }

  while (df$n2 < 10) {
    df$n2 <- sample(n2_range, size = 1)
  }

  df
}

#' @title Fake p1_1, p1_2, p2_1, p2_2, n1, n2 data
#' @description Creates fake data for knowledge assessments in SXSW report
#' @param p1_1_range the lower range of values to sample for p1
#' @param p1_2_range the upper range of values to sample for p1
#' @param p2_1_range the lower range of values to sample for p2
#' @param p2_2_range the upper range of values to sample for p2
#' @param n1_range the range of values to sample for n1
#' @param n2_range the range of values to sample for n2
#' @return a randomised dataset
#' @export
p_and_n_split <- function(p1_1_range = c(30:70),
                          p1_2_range = c(70:100),
                          p2_1_range = c(80:95),
                          p2_2_range = c(85:100),
                          n1_range = stats::rnorm(1, mean = 80, sd = 10),
                          n2_range = stats::rnorm(1, mean = 50, sd = 10)) {
  df <- tibble::tibble(
    p1_1 = sample(p1_1_range, size = 1),
    p1_2 = sample(p1_2_range, size = 1),
    p2_1 = sample(p2_1_range, size = 1),
    p2_2 = sample(p2_2_range, size = 1),
    n1 = sample(n1_range, size = 1),
    n2 = sample(n2_range, size = 1)
  )

  while (df$n2 > df$n1) {
    df$n2 <- sample(n2_range, size = 1)
  }

  while (df$n1 < 10) {
    df$n1 <- sample(n1_range, size = 1)
  }

  while (df$n2 < 10) {
    df$n2 <- sample(n2_range, size = 1)
  }

  df
}


#' @title FAKE Knowledge Assessment Graph Summary
#' @description Creates a bar graph specifically for Knowledge Assessments for SXSW Report
#' @param title the title for the knowledge assessment to make plot for
#' @param fake_data_fun p_and_n, p_and_n_split
#' @param custom_x_axis_labels custom x-axis labels
#' @param custom_n custom n, one number
#' @param custom_n_range custom n range the number to add/subtract for random number generation
#' @param custom_p_1 custom percentage 1, vector with 1: initial percentage, and 2: amount to set maximum increase
#' @param custom_p_2 custom percentage 2, vector with 1: initial percentage, and 2: amount to set maximum increase
#' @param randomness multiplier for amount of randomness in custom percentages, default 0
#' @param multiple_labels a special label maker to add group labels in bar charts
#' @param know_graph if it is a knowledge assessments graph then add % correct to title
#' @return a ggplot object
#' @export
fake_bar_graph_create <- function(title,
                                  fake_data_fun = "p_and_n",
                                  custom_n_range = 0,
                                  custom_n = NULL,
                                  custom_x_axis_labels = NULL,
                                  custom_p_1 = NULL,
                                  custom_p_2 = NULL,
                                  randomness = 0,
                                  multiple_labels = NULL,
                                  know_graph = F) {
  if (fake_data_fun == "p_and_n") {
    if (!is.null(custom_p_1)) {
      p1 <- TeachingLab::runif_round(custom_p_1[1] * (1 - randomness), custom_p_1 * (1 + randomness))
      p2 <- TeachingLab::runif_round((custom_p_1[1] + custom_p_1[2]) * (1 - randomness), (custom_p_1[1] + custom_p_1[2]) * (1 + randomness))
    } else {
      p1 <- TeachingLab::runif_round(40, 80)
      p2 <- TeachingLab::runif_round(70, 100)
    }

    if (!is.null(custom_n)) {
      n1 <- TeachingLab::runif_round(custom_n - custom_n_range, custom_n + custom_n_range)
      n2 <- round(n1 * 0.87)
    } else {
      n1 <- TeachingLab::runif_round(60, 95)
      n2 <- TeachingLab::runif_round(35, 65)
    }

    if (stringr::str_detect(title, "_")) {
      title <- stringr::str_replace_all(
        stringr::str_to_title(
          stringr::str_replace_all(title, "_", " ")
        ),
        "Ela",
        "ELA"
      )
    }

    plot_data <- tibble::tibble(
      name = c("Before", "After"),
      value = c(p2, p1),
      fill = name
    )

    p <- plot_data %>%
      ggplot2::ggplot(ggplot2::aes(x = name, y = value, fill = name)) +
      ggplot2::geom_col() +
      ggplot2::geom_text(ggplot2::aes(label = paste0(round(value), "%")),
        vjust = -1,
        fontface = "bold",
        family = "Calibri"
      ) +
      ggplot2::scale_fill_manual(values = c("Before" = "#D17DF7", "After" = "#55BBC7")) +
      ggplot2::labs(
        x = "", y = "",
        title = paste0(title, "<br>", ifelse(know_graph == T, "% Correct ", ""), "<b style='color:#55bbc7'>before (n = ", n1, ")</b> and <b style='color:#d17df7'>after (n = ", n2, ")</b>")
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::percent_format(scale = 1), expand = c(0.1, 0),
        limits = c(0, 100)
      ) +
      ggplot2::scale_x_discrete(labels = c("Before", "After")) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggtext::element_markdown(lineheight = 1.1, hjust = 0.5),
        legend.position = "none",
        axis.text.x = ggplot2::element_text(face = "bold")
      )

    return(p)
  } else if (fake_data_fun == "p_and_n_split") {
    if (!is.null(custom_p_1)) {
      p1_1 <- TeachingLab::runif_round(custom_p_1[1] * (1 - randomness), custom_p_1[1] * (1 + randomness))
      p1_2 <- TeachingLab::runif_round(custom_p_1[1] + custom_p_1[2] * (1 - randomness), custom_p_1[1] + custom_p_1[2])
      p2_1 <- TeachingLab::runif_round(custom_p_2[1] * (1 - randomness), custom_p_2[1] * (1 + randomness))
      p2_2 <- TeachingLab::runif_round(custom_p_2[1] + custom_p_2[2] * (1 - randomness), custom_p_2[1] + custom_p_2[2])
    } else {
      p1_1 <- TeachingLab::runif_round(30, 70)
      p1_2 <- TeachingLab::runif_round(70, 100)
      p2_1 <- TeachingLab::runif_round(80, 95)
      p2_2 <- TeachingLab::runif_round(85, 100)
    }

    if (!is.null(custom_n)) {
      n1 <- TeachingLab::runif_round(custom_n - custom_n_range, custom_n + custom_n_range)
      n2 <- round(n1 * 0.87)
    } else {
      n1 <- TeachingLab::runif_round(60, 95)
      n2 <- TeachingLab::runif_round(35, 65)
    }


    if (stringr::str_detect(title, "_")) {
      title <- stringr::str_replace_all(
        stringr::str_to_title(
          stringr::str_replace_all(title, "_", " ")
        ),
        "Ela",
        "ELA"
      )
    }

    if (is.null(custom_x_axis_labels)) {
      custom_x_axis_labels <- c("First Year Participants", "Returning Participants")
    }

    plot_data <- tibble::tibble(
      name = factor(rep(custom_x_axis_labels, 2)),
      value = c(p1_2, p2_2, p1_1, p2_1),
      fill = factor(c("Before", "Before", "After", "After"))
    )

    if (!is.null(multiple_labels)) {
      label_df <- tibble::tibble(
        x = c(0.75, 1.25, 1.75, 2.25),
        y = c(15, 15, 15, 15), # ORDER IS 4, 1, 3, 2 FOR SOME REASON
        label = rep(multiple_labels, 2) %>% sort()
      ) %>%
        dplyr::mutate(label = stringr::str_wrap(label, 15))
    } else {
      label_df <- tibble::tibble(x = 1, y = 1, label = "")
    }

    p <- plot_data %>%
      ggplot2::ggplot(ggplot2::aes(x = name, y = value, fill = fill)) +
      ggplot2::geom_col(position = ggplot2::position_dodge()) +
      ggplot2::geom_text(ggplot2::aes(label = paste0(round(value), "%")),
        vjust = -1,
        fontface = "bold",
        family = "Calibri",
        position = ggplot2::position_dodge(width = 0.9)
      ) +
      ggplot2::geom_text(
        data = label_df,
        ggplot2::aes(label = label, x = x, y = y, fill = NULL),
        size = 4,
        color = "black",
        fontface = "bold",
        family = "Calibri"
      ) +
      ggplot2::scale_fill_manual(values = c("Before" = "#D17DF7", "After" = "#55BBC7")) +
      # ggtext::geom_richtext(data = data.frame(name = "Before", value = 100,
      #                                         label = "% Correct <b style='color:#d17df7'>before</b> and <b style='color:#55bbc7'>after</b>."),
      #                       aes(x = name, y = value, label = label)) +
      ggplot2::labs(
        x = "", y = "",
        # title = paste0(title, "\n% Correct before and after")#,
        title = paste0(title, "<br>", ifelse(know_graph == T, "% Correct ", ""), "<b style='color:#55bbc7'>before (n = ", n1, ")</b> and <b style='color:#d17df7'>after (n = ", n2, ")</b>")
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::percent_format(scale = 1), expand = c(0.1, 0),
        limits = c(0, 100)
      ) +
      # TeachingLab::theme_tl(markdown = F) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        # plot.title = ggplot2::element_text(),
        plot.title = ggtext::element_markdown(lineheight = 1.1, hjust = 0.5),
        legend.position = "none",
        axis.text.x = ggplot2::element_text(face = "bold"),
        axis.text.y.left = ggplot2::element_text(face = "bold")
      )

    print(p)
  }
}


#' @title FAKE Knowledge Assessment Line Graph Summary
#' @description Creates a line graph specifically for Knowledge assessments for SXSW Report
#' @param title the title for the plot
#' @param fake_data_fun time_data
#' @param x_axis the labels for the x-axis: yearly or pre-pl-post-pl
#' @param y_axis_label A custom y axis label
#' @param labels The labels for the lines
#' @param lines number of lines to create, either 1 or 2
#' @param custom_n custom n, one number
#' @param custom_n_range custom n range the number to add/subtract for random number generation
#' @return a ggplot object
#' @export

fake_line_graph_create <- function(title, fake_data_fun = "time_data",
                                   labels = c("Test 1", "Test 2"),
                                   x_axis = "yearly",
                                   y_axis_label = NULL,
                                   lines = 2,
                                   custom_n = NULL,
                                   custom_n_range = 0) {
  point1_1 <- TeachingLab::runif_round(0, 35)
  point2_1 <- TeachingLab::runif_round(20, 50)
  point3_1 <- TeachingLab::runif_round(35, 70)
  point4_1 <- TeachingLab::runif_round(55, 85)
  point1_2 <- TeachingLab::runif_round(10, 20)
  point2_2 <- TeachingLab::runif_round(15, 35)
  point3_2 <- TeachingLab::runif_round(30, 50)
  point4_2 <- TeachingLab::runif_round(35, 50)

  if (!is.null(custom_n)) {
    n1 <- TeachingLab::runif_round(custom_n - custom_n_range, custom_n + custom_n_range)
    n2 <- TeachingLab::runif_round(custom_n - custom_n_range, custom_n + custom_n_range)
  } else {
    n1 <- TeachingLab::runif_round(50, 70)
    n2 <- TeachingLab::runif_round(50, 70)
  }

  if (x_axis == "yearly") {
    x_axis_labels <- factor(c("Pre Partnership", "Year 1", "Year 2", "Year 3"),
      levels = c("Pre Partnership", "Year 1", "Year 2", "Year 3")
    )
  } else if (x_axis == "pl") {
    x_axis_labels <- factor(c("Pre PL", "Cycle 1", "Cycle 2", "Post PL"),
      levels = c("Pre PL", "Cycle 1", "Cycle 2", "Post PL")
    )
  }

  plot_data <- tibble::tibble(
    name = rep(x_axis_labels, 2),
    value = c(
      point1_1, point2_1, point3_1, point4_1,
      point1_2, point2_2, point3_2, point4_2
    ),
    color = c(
      "line1", "line1", "line1", "line1",
      "line2", "line2", "line2", "line2"
    )
  )

  label_df <- tibble::tibble(
    x = c(4, 4),
    y = c(
      plot_data %>% dplyr::filter((name == "Year 3" | name == "Post PL") & color == "line1") %>% dplyr::select(value) %>% purrr::as_vector(),
      plot_data %>% dplyr::filter((name == "Year 3" | name == "Post PL") & color == "line2") %>% dplyr::select(value) %>% purrr::as_vector()
    ),
    label = stringr::str_wrap(
      labels,
      15
    )
  )

  if (lines == 1) {
    plot_data <- plot_data %>%
      dplyr::filter(color == "line1")

    label_df$label <- ""
  }

  y_axis_label <- if (is.null(y_axis_label)) {
    "Percent Positive Indicators on IPG"
  } else {
    y_axis_label
  }

  p <- plot_data %>%
    ggplot2::ggplot(ggplot2::aes(x = name, y = value, color = color)) +
    ggplot2::geom_line(ggplot2::aes(group = color)) +
    ggplot2::geom_point() +
    # ggrepel::geom_text_repel(
    #   data = label_df,
    #   ggplot2::aes(label = label, x = x, y = y),
    #   nudge_x = 1,
    #   nudge_y = 5,
    #   arrow = grid::arrow(length = grid::unit(0.02, "npc")),
    #   # vjust = -0.25,
    #   color = "black",
    #   fontface = "bold",
    #   family = "Calibri",
    #   hjust = 0
    # ) +
    ggplot2::geom_text(
      data = label_df,
      ggplot2::aes(label = label, x = x, y = y),
      arrow = grid::arrow(length = grid::unit(0.02, "npc")),
      # vjust = -0.25,
      color = "black",
      fontface = "bold",
      family = "Calibri",
      hjust = 0
    ) +
    # ggplot2::scale_fill_manual(values = c("Before" = "#D17DF7", "After" = "#55BBC7")) +
    ggplot2::labs(
      x = "", y = y_axis_label,
      title = paste0(title, " (N size ranges from <b>", ifelse(n2 > n1, n1, n2), "</b> to <b>", ifelse(n2 < n1, n1, n2), "</b>)")
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(scale = 1), expand = c(0.1, 0),
      limits = c(0, 100)
    ) +
    ggplot2::scale_color_manual(values = c("line1" = "#314482", "line2" = "#d1c926")) +
    ggplot2::expand_limits(x = c(0, length(unique(plot_data$name)) + 1)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(lineheight = 1.1, hjust = 0.5),
      legend.position = "none",
      axis.text.x = ggplot2::element_text(face = "bold"),
      axis.text.y.left = ggplot2::element_text(face = "bold")
    )

  return(p)
}


#' @title TL Themed Stacked Bar Chart
#' @description Automatically scaled stacked bar chart with TL theming
#' @param data the data for the plotter to use, should include all columns of interest
#' @param title the title for the plot
#' @param string_remove NULL by default, provides an optional string removal
#' @param string_wrap 80 by default, the amount by which to wrap y axis text
#' @return a ggplot object
#' @export

tl_likert <- function(data, title = "% Selected", string_remove = NULL, string_wrap = 80) {
  n <- nrow(data)

  if (!is.null(string_remove)) {
    data <- dplyr::rename_with(data, ~ stringr::str_remove_all(.x, string_remove))
  }

  data_final <- data |>
    tidyr::pivot_longer(tidyr::everything()) |>
    tidyr::drop_na(value) |>
    dplyr::group_by(name, value) |>
    dplyr::summarise(n = n()) |>
    dplyr::ungroup() |>
    dplyr::group_by(name) |>
    dplyr::mutate(
      percent = 100 * (n / sum(n)),
      name = stringr::str_wrap(name, string_wrap),
      text_color = dplyr::if_else(readr::parse_number(value) < 4, "white", "black"),
      value = factor(value, levels = c(
        "1 - Strongly Disagree",
        "2 - Disagree",
        "3 - Mostly Disagree",
        "4 - Mostly Agree",
        "5 - Agree",
        "6 - Strongly Agree"
      ))
    )

  ggplot2::ggplot(data_final, aes(
    x = name, y = percent
  )) +
    ggplot2::geom_col(
      color = NA,
      position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE),
      ggplot2::aes(fill = value)
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = dplyr::if_else(percent >= 10,
          paste0(round(percent), "%"),
          ""
        ),
        color = text_color
      ),
      position = ggplot2::position_stack(vjust = 0.5),
      family = "Calibri Bold",
      fontface = "bold"
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = TeachingLab::tl_palette(
      color = "blue",
      n = 6
    )) +
    ggplot2::scale_color_identity() +
    # ggplot2::scale_fill_manual(values = c(
    #   "1 - Strongly Disagree" = "#040404",
    #   "2 - Disagree" = "#032533",
    #   "3 - Mostly Disagree" = "#024762",
    #   "4 - Mostly Agree" = "#016891",
    #   "5 - Agree" = "#008AC0",
    #   "6 - Strongly Agree" = "#00ACF0"
    # )) +
    ggplot2::guides(fill = guide_legend(reverse = FALSE)) +
    ggplot2::labs(
      fill = "", title = glue::glue("{title}"),
      x = "", y = "",
      subtitle = glue::glue("n = {n}")
    ) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    TeachingLab::theme_tl() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(lineheight = 1.1, size = 14),
      legend.position = "bottom",
      plot.title = ggplot2::element_text(lineheight = 1.1, size = 20, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 14, face = "bold"),
      legend.key.size = grid::unit(1.25, "cm"),
      legend.text = ggplot2::element_text(size = 12)
    )
}


#' @title Student percent agree/strongly agree
#' @description Automatically dodged bar chart for student data
#' @param data the data for the plotter to use, should include all columns of interest
#' @param title the title for the plot
#' @param string_remove NULL by default, provides an optional string removal
#' @param col_select the columns to select with `tidyselect::contains`
#' @param agree_select the type of agree/strongly agree to select, for example also often/always
#' @param legend_position c(0.8, 0.25) by default, adjustable
#' @param race_filter a parameter to filter for a specific race from the survey
#' @return a ggplot object
#' @export

student_bar_chart <- function(data,
                              col_select,
                              agree_select,
                              string_remove,
                              title,
                              legend_position = c(0.8, 0.25),
                              race_filter = "All") {
  
  if (race_filter != "All") {
    data <- data |>
      dplyr::filter(race == race_filter)
  }
  
  n_size_1 <- format(sum(!is.na(data |> dplyr::filter(prepost == "Pre") |> dplyr::select(tidyselect::contains(col_select)) |> dplyr::pull(1))), big.mark = ",")
  n_size_2 <- format(sum(!is.na(data |> dplyr::filter(prepost == "Post") |> dplyr::select(tidyselect::contains(col_select)) |> dplyr::pull(1))), big.mark = ",")
  
  ### Makes race column, selects relevant columns and gets percent
  ### that selected relevant levels of agreeness
  student_data_summarised <- data |>
    dplyr::select(tidyselect::contains(col_select), prepost) |>
    TeachingLab::relabel_qualtrics_df() |>
    dplyr::group_by(prepost) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ TeachingLab::tl_select_percent(.x, agree_select))) |>
    tidyr::drop_na(prepost) |>
    (\(.) dplyr::mutate(., Overall = rowMeans(select(., starts_with("Please")))))()

  ### Reformat dataframe and prep for ggplot2
  student_data_percent <- student_data_summarised |>
    tidyr::pivot_longer(!prepost, names_to = "question", values_to = "percent") |>
    tidyr::drop_na(percent) |>
    dplyr::mutate(
      question = stringr::str_remove_all(question, string_remove),
      question = stringr::str_wrap(question, 25),
      percent = percent * 100
    )

  subtitle <- if (length(agree_select) > 1) {
    glue::glue('The following percentages show the % that selected "{agree_select[1]}" or "{agree_select[2]}"')
  } else {
    glue::glue('The following percentages show the % that selected "{agree_select[1]}"')
  }

  p <- ggplot2::ggplot(student_data_percent, aes(x = forcats::fct_relevel(question, "Overall", after = length(question)), y = percent, fill = prepost)) +
    ggplot2::geom_col(position = position_dodge()) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = paste0(round(percent), "%")
      ),
      position = ggplot2::position_dodge2(width = 1, preserve = "total"),
      hjust = -0.25,
      family = "Calibri"
    ) +
    ggplot2::labs(
      x = "", y = "",
      title = glue::glue("{title} (n = {n_size_1}) & <span style = 'color:#04abeb;'>(n = {n_size_2})</span>"),
      subtitle = subtitle,
      fill = "Race"
    ) +
    ggplot2::scale_fill_manual(values = c("Post" = "#040404", "Pre" = "#04ABEB")) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(reverse = TRUE),
      color = "none"
    ) +
    ggplot2::coord_flip() +
    TeachingLab::theme_tl(legend = F) +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(family = "Calibri Bold", face = "bold"),
      plot.subtitle = ggtext::element_markdown(family = "Calibri", hjust = 1),
      legend.key.size = grid::unit(1.2, "cm"),
      axis.text.y = ggplot2::element_text(size = 14)
    )
  
  cat("#### ", race_filter, "\n")
  cat("\n")
  
  print(p)
  
  cat("\n\n")
  
}

