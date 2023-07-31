#' @title General Table Maker
#' @description Makes a gt table for mindsets question set
#'
#' @param data the dataframe to be analyzed
#' @param column_names the names of the columns
#' @param title The tables title
#' @param spanner The gt spanner
#' @param n1 the n for fall
#' @param n2 the n for spring
#' @param rows_positive the positive rows
#' @param rows_negative the negative_rows
#' @param improve_col name for the improve/sustain column
#' @param bottom_row the bottom row which has "n = " instead of data
#'
#' @importFrom magrittr %>%
#'
#' @return Returns an unsaved gt table
#' @export

table_maker <- function(data, column_names, title, spanner, n1, n2, rows_positive, rows_negative, improve_col, bottom_row) {
  colnames(data)[2:5] <- c("name_1", "name_2", "name_3", "name_4")

  gt_table <- data %>%
    gt::gt(rowname_col = "rowname") %>%
    # Add spanner
    gt::tab_spanner(
      label = spanner,
      columns = c(2:4)
    ) %>%
    # Add title
    gt::tab_header(title = gt::md(glue::glue("**{title}, by Survey Administration**"))) %>%
    # Make labels colorful
    gt::cols_label(
      name_1 = gt::html("<strong><center><span style = 'color:#00acf0;'>Diagnostic Survey</span></center></strong>"),
      name_2 = gt::html("<strong><center><span style = 'color:#00acf0;'>Follow-up Survey</span></center></strong>"),
      name_3 = gt::html("<strong><center><span style = 'color:#00acf0;'>Percentage Point Change</span></center></strong>"),
      name_4 = gt::html(glue::glue("<strong><center><span style = 'color:#43c6b9;'>{improve_col}</span></center></strong>"))
    ) %>%
    # Column widths
    gt::cols_width(
      1 ~ gt::px(200),
      2 ~ gt::px(125),
      3 ~ gt::px(125),
      4 ~ gt::px(125),
      5 ~ gt::px(200)
    ) %>%
    # Percent format the data
    gt::fmt_percent(
      columns = c(2, 3, 5),
      scale_values = F,
      decimals = 0
    ) %>%
    # Add + symbol where needed
    # fmt_percent(
    #   columns = c(4),
    #   scale_values = F,
    #   decimals = 0,
    #   rows = rows_positive,
    #   pattern = "+{x}"
    # ) %>%
    # For - make percent as well
    # fmt_percent(
    #   columns = c(4),
    #   scale_values = F,
    #   decimals = 0,
    #   rows = rows_negative
    # ) %>%
    # Color by gradation, < 40 is light, 40-80 is medium, > 80 is dark
    # Blue
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#89d7f7")
      ),
      locations = gt::cells_body(
        columns = c(2),
        rows = `name_1` < 40
      )
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#52c6f4")
      ),
      locations = gt::cells_body(
        columns = c(2),
        rows = `name_1` >= 40
      )
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#00ACF0")
      ),
      locations = gt::cells_body(
        columns = c(2),
        rows = `name_1` > 80
      )
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#89d7f7")
      ),
      locations = gt::cells_body(
        columns = c(3),
        rows = `name_2` < 40
      )
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#52c6f4")
      ),
      locations = gt::cells_body(
        columns = c(3),
        rows = `name_2` >= 40
      )
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#00ACF0")
      ),
      locations = gt::cells_body(
        columns = c(3),
        rows = `name_2` > 80
      )
    ) %>%
    # Green
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#A7E3DE")
      ),
      locations = gt::cells_body(
        columns = c(5),
        rows = `name_4` < 40
      )
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#7FD7CF")
      ),
      locations = gt::cells_body(
        columns = c(5),
        rows = `name_4` >= 40
      )
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#43C6B9")
      ),
      locations = gt::cells_body(
        columns = c(5),
        rows = `name_4` > 80
      )
    ) %>%
    # Add black line next to row group
    gt::tab_style(
      style = list(
        gt::cell_borders(
          sides = c("right"),
          color = "black",
          weight = gt::px(3)
        )
      ),
      locations = gt::cells_stub()
    ) %>%
    # Add black line next to tab spanner
    gt::tab_style(
      style = list(
        gt::cell_borders(
          sides = c("right"),
          color = "black",
          weight = gt::px(3)
        )
      ),
      locations = gt::cells_body(
        columns = c(4)
      )
    ) %>%
    # Add thin black line next to improvement column
    gt::tab_style(
      style = list(
        gt::cell_borders(
          sides = c("left"),
          color = "black",
          weight = gt::px(1.5)
        )
      ),
      locations = gt::cells_body(
        columns = c(4)
      )
    ) %>%
    # Make last row n row
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#FFFFFF")
      ),
      locations = gt::cells_body(
        rows = bottom_row
      )
    ) %>%
    gt::fmt_number(
      columns = c(2, 3, 5),
      rows = c(bottom_row),
      pattern = "n = {x}",
      decimals = 0
    ) %>%
    gt::sub_missing(
      columns = c(4),
      rows = c(bottom_row),
      missing_text = ""
    ) %>%
    # Footnotes
    # tab_footnote(
    #   footnote = gt::md(glue::glue("Note: The number of observations varies between items from {n2[1]} to {n2[2]}")),
    #   locations = cells_column_labels(
    #     columns = c(1:4)
    #   )
    # ) %>%
    # tab_footnote(
    #   footnote = gt::md(glue::glue("n = {n1}")),
    #   locations = cells_column_labels(
    #     columns = c(5)
    #   )
    # ) %>%
    # Final theming
    TeachingLab::gt_theme_tl() %>%
    gt::tab_options(
      column_labels.border.lr.style = "solid",
      column_labels.vlines.style = "solid",
      heading.border.lr.style = "solid",
      heading.border.bottom.width = gt::px(3),
      heading.border.bottom.color = "black",
      heading.border.lr.width = gt::px(3),
      heading.border.lr.color = "black"
    )

  gt_table
}


#' @title General Table Maker
#' @description Makes a gt table with teaching lab color style
#'
#' @param gt_table the gt table to color
#' @param color the color style to apply
#' @param column The column to apply the color to
#' @param scale the scale to apply to
#' @return a colored gt table
#'
#' @importFrom magrittr %>%
#'
#' @export

gt_tl_color <- function(gt_table, color, column, scale = 1) {
  column_quo <- rlang::quo_name(rlang::enquo(column))

  value_one <- 40 * scale
  value_two <- 80 * scale
  cat(paste(value_one, value_two, "\n"))

  if (color == "blue") {
    gt_table %>%
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#89d7f7")
        ),
        locations = gt::cells_body(
          columns = c(`column_quo`),
          rows = `column_quo` < value_one
        )
      ) %>%
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#52c6f4")
        ),
        locations = gt::cells_body(
          columns = c(`column_quo`),
          rows = `column_quo` >= value_one
        )
      ) %>%
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#00ACF0")
        ),
        locations = gt::cells_body(
          columns = c(`column_quo`),
          rows = `column_quo` > value_two
        )
      )
  } else if (color == "green") {
    gt_table %>%
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#A7E3DE")
        ),
        locations = gt::cells_body(
          columns = c(`column_quo`),
          rows = `column_quo` > value_one
        )
      ) %>%
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#7FD7CF")
        ),
        locations = gt::cells_body(
          columns = c(`column_quo`),
          rows = `column_quo` > value_one
        )
      ) %>%
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#43C6B9")
        ),
        locations = gt::cells_body(
          columns = c(`column_quo`),
          rows = `column_quo` > value_two
        )
      )
  }
}


#' @title Arrow maker for gt table
#' @description Makes an html column for a gt table
#'
#' @param data the gt table to make arrows for
#' @param colors the color style to apply
#' @param column_one The first column to compare
#' @param column_two The second column to compare
#' @return a colored gt table
#'
#' @importFrom magrittr %>%
#'
#' @examples gt_arrow(data = mtcars, colors = c("red", "blue"), column_one = cyl, column_two = disp)
#' @export

gt_arrow <- function(data, colors = c("#800000", "#98AFC7"), column_one, column_two) {
  rank_chg <- function(change_dir) {
    if (change_dir == "increase") {
      logo_out <- fontawesome::fa("arrow-up", fill = "#98AFC7")
    } else if (change_dir == "decrease") {
      logo_out <- fontawesome::fa("arrow-down", fill = "#800000")
    } else if (change_dir == "equal") {
      logo_out <- "<strong>="
    }

    logo_out %>%
      as.character() %>%
      gt::html()
  }

  score_1 <- rlang::enquo(column_one)
  score_2 <- rlang::enquo(column_two)
  score_pre <- rlang::quo_name(score_1)
  score_post <- rlang::quo_name(score_2)

  data %>%
    dplyr::mutate(rank_change = dplyr::case_when(
      .data[[score_pre]] < .data[[score_post]] ~ "increase",
      .data[[score_post]] < .data[[score_pre]] ~ "decrease",
      abs(.data[[score_pre]] - .data[[score_post]]) < 3 ~ "equal"
    )) %>%
    dplyr::mutate(rank_change = purrr::map(rank_change, ~ rank_chg(change_dir = .x))) %>%
    dplyr::rename(Improvement = rank_change)
}

#' @title Word highlighting
#' @description Finds most common words in string
#' @param string the string to evaluate
#' @param n the number of words to find
#' @param print whether or not to print the highlighted words
#' @return a vector of strings
#'
#' @export
find_highlight <- function(string, n = 3, print = F) {
  data(na_df)
  stop_words <- tidytext::stop_words |>
    dplyr::bind_rows(tibble::tibble(word = na_df,
                                    lexicon = "TL_NA"))

  highlight <- string |>
    na.omit() |>
    tibble::as_tibble_col(column_name = "txt") |>
    tidytext::unnest_tokens(word, txt) |>
    # This makes sure to get rid of numbers in consideration for highlighting
    # By making sure as.numeric returns NA on words
    dplyr::filter(is.na(as.numeric(word))) |>
    # Get a count of words and sort
    dplyr::count(word, sort = T) |>
    # Get rid of generic (stop) words
    dplyr::anti_join(stop_words) |>
    # Get a user-specified number of words, or the default 3
    utils::head(n) |>
    # Make this a vector
    dplyr::pull(word) |>
    # Suppress warnings from the as.numeric call
    suppressWarnings() |>
    suppressMessages()

  if (print == T) {
    print(highlight)
  }

  return(highlight)
}

#' @title Word highlighting
#' @description Provides html formatted highlighting
#' @param data the data to highlight
#' @param highlight the words to highlight
#' @return a vector of strings
#'
#' @export
highlight_fun <- function(data, highlight = TeachingLab::find_highlight(data)) {
  
  # If the word is not plural then add highlighting for the plural version of the same word, 
  plural_highlights <- unlist(
    purrr::map(highlight, ~ if (stringr::str_sub(.x, -1, -1)[1] != "s") {
      paste0(.x, "s")
    })
  )
  
  # Also do the inverse
  not_plural_highlights <- unlist(
    purrr::map(highlight, ~ if (stringr::str_sub(.x, -1, -1)[1] == "s") {
      stringr::str_remove(.x, "s$")
    })
  )
  # If the word is not capitalized then add highlighting for the capitalized version of the same word, 
  capital_highlights <- unlist(
    purrr::map(highlight, ~ if (stringr::str_detect(stringr::str_sub(.x, 1, 1)[1], "[:upper:]")) {
      stringr::str_to_lower(.x)
    })
  )
  # Also do the inverse
  not_capital_highlights <- unlist(
    purrr::map(highlight, ~ if (stringr::str_detect(stringr::str_sub(.x, 1, 1)[1], "[:lower:]")) {
      stringr::str_to_title(.x)
    })
  )
  # Add plurals, capitalization to list and ensure uniqueness
  highlight <- append(highlight,
                      c(plural_highlights,
                        not_plural_highlights,
                        capital_highlights,
                        not_capital_highlights)) |>
    unique()

  # Create a vector for replacement with format <html>new_name</html> = old_name
  replacement_vector <- stats::setNames(
    paste0(
      "<span style='color:#04abeb; font-weight:bold;'>",
      highlight,
      "</span>"
    ),
    highlight
  )

  # Use replace_all on the original `string with the replacement vector created above
  highlighted_string <- stringr::str_replace_all(
    data,
    replacement_vector
  )

  return(highlighted_string)
}



#' @title Quote Visualization
#' @description takes a dataframe and makes a gt table or ggplot that shows a quote
#' @param data the dataframe
#' @param text_col columns to create table for
#' @param viz_type ggplot or gt visualization
#' @param title the title of the ggplot or gt
#' @param custom_highlight a vector, optional custom highlighting
#' @param n integer, number of words to auto-highlight
#' @param print T, whether or not to print the highlighted words to console
#' @param width The width of the table generated
#' @param suppress_warnings T/F suppression of warnings
#' @param align the table alignment: "left", "center", "right"
#' @param save if TRUE actually make the gt
#' @param ... Arguments passed onto the gt table
#' @return a ggplot/gt that visualizes text
#'
#' @examples
#' \dontrun{
#' df <- TeachingLab::survey_monkey
#' colnames(df)[1] <- "What learning are you excited to try?"
#' quote_viz(
#'   data = df,
#'   text_col = "What learning are you excited to try?",
#'   viz_type = "gt",
#'   title = "Responses from Survey Monkey"
#' )
#' }
#' @export

quote_viz <- function(data,
                      text_col = colnames(data)[1],
                      viz_type = "gt",
                      custom_highlight = NULL,
                      n = 3,
                      print = T,
                      width = 60,
                      title = NULL,
                      suppress_warnings = T,
                      align = "center",
                      save = T,
                      ...) {
  selecting_cols <- text_col
  text_col <- rlang::enquo(text_col)

  if (viz_type == "ggplot") {
    data |>
      dplyr::mutate(text = stringr::str_replace_all(stringr::str_wrap(.data[[text_col]], width = 60), "\n", "<br>")) %>%
      dplyr::mutate(text = paste0("\"<i>", text, "\"")) %>%
      dplyr::mutate(
        x = 0,
        y = dplyr::row_number()
      ) |>
      ggplot2::ggplot() +
      ggtext::geom_richtext(
        fill = NA, label.color = NA, family = "Calibri",
        ggplot2::aes(label = text, x = x, y = y)
      ) +
      ggplot2::scale_y_discrete(expand = c(0, 0.3)) +
      ggplot2::theme_void() +
      ggplot2::theme(text = ggplot2::element_text(family = "Calibri"))
  } else if (viz_type == "gt") {
    if (!tibble::is_tibble(data)) {
      data <- tibble::as_tibble(data)
      selecting_cols <- "value"
    }

    # If not custom highlighting, find a specified number of words to highlight, n = 3 by default
    if (is.null(custom_highlight)) {
      highlight <- purrr::map_dfc(selecting_cols, ~ TeachingLab::find_highlight(string = data %>% 
                                                                                  dplyr::pull(.x), n = n)) %>%
        suppressMessages()
      highlight <- purrr::map_chr(1:length(highlight), ~ paste0("highlight", .x)) %>%
        stats::setNames(highlight, nm = .)
    } else if (is.character(custom_highlight)) {
      highlight <- custom_highlight # Custom highlighting
    }

    # Print out highlights for reference
    if (print == T) {
      print(highlight)
    }

    # Select just the relevant columns for highlighting
    data_text <- data |>
      dplyr::select(!!text_col)

    # Get all highlights as just one vector
    all_highlights <- highlight |>
      tidyr::pivot_longer(dplyr::everything(), values_to = "highlight") |>
      dplyr::pull(highlight)

    # First map function applies the highlighting column by column
    # Second map function sorts the data so that the highlighted words are on top
    # Issue: this allows for overlay of html tags since it doesn't all occur at once,
    # NEED TO PREVENT <br> and <span> from getting highlighted
    data_final <- purrr::map2_dfc(data_text[selecting_cols], 1:length(colnames(highlight)), ~
    TeachingLab::highlight_fun(
      TeachingLab::html_wrap(.x, n = width),
      highlight |> dplyr::pull(.y)
    )) %>% purrr::map_df(., ~ append(
      sort(as.character(factor(.x, levels = unique(.x[stringr::str_detect(.x, "<span")])))),
      .x[!stringr::str_detect(.x, "<span")]
    ))

    if (save == F) {
      data_final
    } else {
      data(na_df)
      # Make gt table with all HTML Formatting
      data_final |>
        janitor::remove_empty("rows") |>
        dplyr::filter(dplyr::if_any(dplyr::everything(), ~ .x %!in% na_df)) |>
        gt::gt() %>%
        {
          if (!is.null(title)) gt::tab_header(data = ., title = gt::md(paste0("**", title, "**"))) else .
        } %>%
        gt::fmt_markdown(columns = gt::everything()) |>
        gt::sub_missing(columns = everything(),
                        missing_text = " ") |>
        gt::cols_align(align = align) |>
        gt::tab_style(
          style = list(
            gt::cell_text(
              size = "medium"
            )
          ),
          locations = gt::cells_body(
            columns = gt::everything(),
            rows = gt::everything()
          )
        ) |>
        gt::opt_row_striping(row_striping = TRUE) |>
        TeachingLab::gt_theme_tl(align = align, ...)
    }
    
  }
}



#' @title Knowledge Assessment Graphs
#' @description Creates a graph specifically for Knowledge Assessments in mid year reports
#' @param data the data
#' @param know_assess the knowledge assessment to make a table for
#' @return a gt table
#' @export
gt_know_assess <- function(data, know_assess) {

  # Format to wide pre and post
  plot_data <- data %>%
    dplyr::filter(know_assess == !!rlang::enquo(know_assess)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-site) %>% # Get rid of site for when there is more than one
    dplyr::group_by(question, prepost) %>%
    dplyr::summarise(percent = mean(percent, na.rm = T)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = "prepost", values_from = "percent") %>%
    dplyr::select(percent_pre = pre,
                  percent_post = post,
                  question)

  title <- stringr::str_to_title(stringr::str_replace_all(know_assess, "_", " ")) %>%
    stringr::str_replace_all(., "Ela", "ELA") %>%
    stringr::str_replace_all(., "Eic", "EIC") # Correct title casing
  
  questions_and_answers <- readr::read_rds("data/sy21_22/all_knowledge_questions_and_answers.rds")
  
  ## Overall averages ##
  if (sum(data$prepost == "pre") >= 1) {
    pre_percent_correct <- data %>%
      dplyr::filter(prepost == "pre") %>%
      dplyr::summarise(percent = mean(percent, na.rm = T)) %>%
      dplyr::pull(percent)
  } else {
    pre_percent_correct <- NA
    plot_data <- plot_data %>%
      dplyr::mutate(percent_pre = NA)
  }
  if (sum(data$prepost == "post") >= 1) {
    post_percent_correct <- data %>%
      dplyr::filter(prepost == "post") %>%
      dplyr::summarise(percent = mean(percent, na.rm = T)) %>%
      dplyr::pull(percent)
  } else {
    post_percent_correct <- NA
    plot_data <- plot_data %>%
      dplyr::mutate(percent_post = NA)
  }


  n1 <- data %>%
    dplyr::filter(know_assess == !!rlang::enquo(know_assess) & prepost == "pre") %>%
    dplyr::pull(id) %>%
    unique() %>%
    length()
  
  n2 <- data %>%
    dplyr::filter(know_assess == !!rlang::enquo(know_assess) & prepost == "post") %>%
    dplyr::pull(id) %>%
    unique() %>%
    length()

  if (length(n1) == 0) {
    n1 <- 0
  }

  if (length(n2) == 0) {
    n2 <- 0
  }

  gt_table <- plot_data %>%
    dplyr::left_join(questions_and_answers, by = "question") %>%
    dplyr::mutate(highlight = T) %>%
    dplyr::group_by(question) %>%
    gt::gt() %>%
    gt::cols_hide(highlight) %>%
    gt::cols_move_to_end(percent_post) %>%
    gt::tab_header(
      title = gt::html(glue::glue("<b>{title} Knowledge Assessments Scoring</b>")),
      subtitle = gt::html("<i style='color:#04abeb'>Correct Answers are Highlighted in Blue</i>")
    ) %>%
    gt::cols_label(
      answer = "Answer",
      question = "Question",
      percent_post = gt::html(glue::glue("Post<br>(n = {n2})")),
      percent_pre = gt::html(glue::glue("Pre<br>(n = {n1})"))
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_text(
          color = "#04abeb",
          weight = "bolder"
        )
      ),
      locations = gt::cells_body(
        columns = c(percent_pre, percent_post),
        rows = highlight == T
      )
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_text(weight = "bolder")
      ),
      locations = gt::cells_row_groups()
    ) %>%
    gt::fmt_markdown(columns = gt::everything()) %>%
    gt::fmt_percent(
      columns = tidyselect::where(is.numeric),
      scale_values = F,
      decimals = 0
    ) %>%
    gt::sub_missing(
      columns = tidyselect::where(is.logical),
      missing_text = "no data"
    ) %>%
    # Impute in total averages
    {
      if (is.numeric(plot_data$percent_pre) == T) {
        gt::summary_rows(.,
          fns = list(`Average % Correct` = ~ return(pre_percent_correct)),
          columns = c(percent_pre),
          formatter = gt::fmt_percent,
          scale_values = F,
          decimals = 0
        )
      } else {
        gt::summary_rows(.,
          fns = list(`Average % Correct` = ~ return(pre_percent_correct)),
          columns = c(percent_pre),
          formatter = gt::sub_missing,
          missing_text = "no data"
        )
      }
    } %>%
    {
      if (is.numeric(plot_data$percent_post) == T) {
        gt::summary_rows(.,
          fns = list(`Average % Correct` = ~ return(post_percent_correct)),
          columns = c(percent_post),
          formatter = gt::fmt_percent,
          scale_values = F,
          decimals = 0
        )
      } else {
        gt::summary_rows(.,
          fns = list(`Average % Correct` = ~ return(post_percent_correct)),
          columns = c(percent_post),
          formatter = gt::sub_missing
        )
      }
    } %>%
    TeachingLab::gt_theme_tl()

  gt_table
}


#' @title Dashboard End of Session Quotes
#' @description Creates a gt table for qualitative responses to the end of session survey
#' @param data the data to be input
#' @param n The number of words to highlight
#' @param size the number of rows of quotes to return
#' @param all Whether or not to return ALL data
#' @param include_columns A vector of additional columns to include in table
#' @param save NULL by default, a save path to make inside current package directory
#' @return Returns a gt, unless save in which case it will return a saved file with gtsave
#' @export
session_quotes <- function(data = TeachingLab::get_session_survey(),
                           size = 10,
                           n = 3,
                           all = F,
                           save = NULL,
                           include_columns = NULL) {
  
  data(na_df)
  
  quotes_gt <- data %>%
    dplyr::select(c("Facilitation_Feedback",
                    "What went well in today's session?",
                    "What could have been better about today's session?"),
                  include_columns) %>%
    dplyr::mutate(dplyr::across(c(1:3), ~ replace(.x, .x %in% na_df, NA))) %>%
    janitor::remove_empty("rows") %>%
    dplyr::mutate(dplyr::across(c(1:3), ~ tidyr::replace_na(.x, "No Response"))) %>%
    {
      if (all == F) purrr::map(., ~ sample(.x, size = size)) else .
    } %>%
    tibble::as_tibble() %>%
    stats::setNames(c("General Facilitation Feedback",
               "What went well in today's session?",
               "What could have been better about today's session?",
               include_columns)) %>%
    TeachingLab::quote_viz(text_col = colnames(.), n = n)
  if (is.null(save)) {
    quotes_gt
  } else {
    gt::gtsave(data = quotes_gt,
               filename = paste0(save))
  }
}

#' @title Dashboard End of Course Quotes
#' @description Creates a gt table for qualitative responses of the end of course survey
#' @param data the data to be input
#' @param n The number of words to highlight
#' @param size the number of rows of quotes to return
#' @param all F by default, whether or not to return ALL data
#' @param include_columns A vector of additional columns to include in table
#' @param save NULL by default, a save path to make inside current package directory
#' @return Returns a gt, unless save in which case it will return a saved file with gtsave
#' @export
course_quotes <- function(data = TeachingLab::get_course_survey(), 
                          size = 10,
                          n = 3,
                          all = F,
                          save = NULL,
                          include_columns = NULL) {
  
  data(na_df)
  
  quotes_gt <- data %>%
    dplyr::select(c("Overall, what went well in this course?", # Qualitative feedback
                    "Overall, what could have been better in this course?", # Qualitative feedback
                    "What is the learning from this course that you are most excited about trying out?", # Qualitative feedback
                    "Which activities best supported your learning in this course?", # Qualitative feedback
                    "Feel free to leave us any additional comments, concerns, or questions."),
                  include_columns) %>%
    dplyr::mutate(dplyr::across(c(1:5), ~ replace(.x, .x %in% na_df, NA))) %>%
    janitor::remove_empty("rows") %>%
    dplyr::mutate(dplyr::across(c(1:5), ~ tidyr::replace_na(.x, "No Response"))) %>%
    {
      if (all == F) purrr::map(., ~ sample(.x, size = size)) else .
    } %>%
    tibble::as_tibble() %>%
    stats::setNames(c("Overall, what went well in this course?", # Qualitative feedback
               "Overall, what could have been better in this course?", # Qualitative feedback
               "What is the learning from this course that you are most excited about trying out?", # Qualitative feedback
               "Which activities best supported your learning in this course?", # Qualitative feedback
               "Feel free to leave us any additional comments, concerns, or questions.")) %>%
    TeachingLab::quote_viz(text_col = colnames(.), n = n)
  if (is.null(save)) {
    quotes_gt
  } else {
    gt::gtsave(data = quotes_gt,
               filename = paste0(save))
  }
  
}



#' @title Teaching Lab Summary Tables
#' @description Creates tables for responses to the Educator Diagnostic Survey
#' @param data the data to be input
#' @param grouping includes "equitable", "high_expectations", and "crse"
#' @param summarise TRUE by default will summarise all data selected
#' @param n_size NULL an optional n_size to include
#' @param n_size_single NULL an optional single value for n_size
#' @param explain add explanation for different percentage levels to the table
#' @param save F by default, a save path inside current working directory
#' @param prepost F by default for making a prepost dataframe
#' @param rename F by default for making prepost dataframe
#' @param admin F by default for prepost admin dataframe
#' @return Returns a gt table, unless save in which case it will return a saved file with gtsave
#' @export
tl_summary_table <- function(data, 
                             save = F, 
                             summarise = T,
                             grouping = NULL,
                             n_size = NULL,
                             n_size_single = NULL,
                             explain = F,
                             prepost = T,
                             admin = F,
                             rename = F) {
  
  
  ## Function to nicely format for table presenting
  reformat_cols <- function(x) {
    stringr::str_replace_all(
      TeachingLab::html_wrap(
        TeachingLab::first_up(
          stringr::str_replace_all(
            stringr::str_remove_all(x, 
                                    "to_what_extent_do_you_agree_or_disagree_with_the_following_statements_|please_rate_your_confidence_on_the_following_items_br_i_am_able_to_|Please rate your confidence on the following items. \\<br\\>I am able to\\.\\.\\. - "), 
            "_", 
            " ")
          )
        ), 
        c("don t" = "don't",
          " i " = " I ",
          " e g " = " e.g. ")
      )
  }
  
  ## Function to calculate scoring
  diagnostic_score <- function(x, reverse) {
    round(100 * (mean(x, na.rm = T)/5))
  }
  
  ## Conditional for item
  if (!is.null(grouping)) {
    if (grouping %in% c("equitable", "high_expectations", "summarise", "crse")) {
      
      if (prepost == T & rename == T & admin == F) {
        equitable_questions <- tibble::tibble(
          question = c("To what extent do you agree or disagree with the following statements? - I am color blind when it comes to my teaching - I don't think of my students in terms of their race or ethnicity.",
                       "To what extent do you agree or disagree with the following statements? - The gap in the achievement among students of different races is about poverty, not race.",
                       "To what extent do you agree or disagree with the following statements? - I think about my own background and experiences and how those affect my instruction."),
          reverse = c(T, T, F)
        )
        
        high_expectations_questions <- tibble::tibble(
          question = c("To what extent do you agree or disagree with the following statements? - I try to keep in mind the limits of my students' ability and give them assignments that I know they can do so that they do not become discouraged.",
                       "To what extent do you agree or disagree with the following statements? - Before students are asked to engage in complex learning tasks, they need to have a solid grasp of basic skills.",
                       "To what extent do you agree or disagree with the following statements? - It is not fair to ask students who are struggling with English to take on challenging academic assignments.",
                       "To what extent do you agree or disagree with the following statements? - Teachers should provide all students the opportunity to work with grade-level texts and tasks."),
          reverse = c(T, T, T, F)
        )
        
        crse_questions <- c("Please rate your confidence on the following items. <br>I am able to... - Adapt instruction to meet the needs of my students",
                            "Please rate your confidence on the following items. <br>I am able to... - Identify ways that the school culture (e.g., values, norms, and practices) is different from my students' home culture",
                            "Please rate your confidence on the following items. <br>I am able to... - Use my students' prior knowledge to help them make sense of new information",
                            "Please rate your confidence on the following items. <br>I am able to... - Revise instructional material to include a better representation of cultural groups",
                            "Please rate your confidence on the following items. <br>I am able to... - Teach the curriculum to students with unfinished learning",
                            "Please rate your confidence on the following items. <br>I am able to... - Teach the curriculum to students who are from historically marginalized groups")
        
        
        if (grouping != "crse") {
          selected <- c(equitable_questions |> pull(question),
                        high_expectations_questions |> pull(question),
                        crse_questions)
          ## Only reversed columns for later calculations
          reversed <- c(equitable_questions %>% dplyr::filter(reverse == T) |> dplyr::pull(question),
                        high_expectations_questions %>% dplyr::filter(reverse == T) |> dplyr::pull(question))
        } else {
          selected <- crse_questions
          reversed <- NULL
          equitable_questions$question <- NULL 
          high_expectations_questions$question <- NULL
        }
        
        
        ## Reformatted column names for later
        equitable_reformat <- purrr::map_chr(equitable_questions$question, reformat_cols)
        high_expectations_reformat <- purrr::map_chr(high_expectations_questions$question, reformat_cols)
        crse_reformat <- purrr::map_chr(crse_questions, reformat_cols)
      } else if (admin == T & prepost == T & rename == F) {
        equitable_questions <- tibble::tibble(
          question = c("to_what_extent_do_you_agree_or_disagree_with_the_following_statements_teachers_should_be_color_blind_when_it_comes_to_their_teaching_they_shouldnt_think_of_students_in_terms_of_their_race_or_ethnicity",
                       "to_what_extent_do_you_agree_or_disagree_with_the_following_statements_the_gap_in_the_achievement_among_students_of_different_races_is_about_poverty_not_race_2",
                       "to_what_extent_do_you_agree_or_disagree_with_the_following_statements_i_think_about_my_own_background_and_experiences_and_how_those_affect_my_instructional_leadership"),
          reverse = c(T, T, F)
        )
        
        high_expectations_questions <- tibble::tibble(
          question = c("to_what_extent_do_you_agree_or_disagree_with_the_following_statements_teachers_should_try_to_keep_in_mind_the_limits_of_students_ability_and_give_them_assignments_that_they_know_they_can_do_so_that_they_do_not_become_discouraged",
                       "to_what_extent_do_you_agree_or_disagree_with_the_following_statements_before_students_are_asked_to_engage_in_complex_learning_tasks_they_need_to_have_a_solid_grasp_of_basic_skills_2",
                       "to_what_extent_do_you_agree_or_disagree_with_the_following_statements_it_is_not_fair_to_ask_students_who_are_struggling_with_english_to_take_on_challenging_academic_assignments_2",
                       "to_what_extent_do_you_agree_or_disagree_with_the_following_statements_teachers_should_provide_all_students_the_opportunity_to_work_with_grade_level_texts_and_tasks_2"),
          reverse = c(T, T, T, F)
        )
        
        crse_questions <- c("please_rate_the_extent_to_which_you_believe_you_can_support_teachers_in_the_following_areas_br_i_believe_i_can_directly_or_indirectly_support_teachers_to_adapt_instruction_to_meet_the_needs_of_their_students",
                            "please_rate_the_extent_to_which_you_believe_you_can_support_teachers_in_the_following_areas_br_i_believe_i_can_directly_or_indirectly_support_teachers_to_identify_ways_that_the_school_culture_e_g_values_norms_and_practices_is_different_from_their_students_home_culture",
                            "please_rate_the_extent_to_which_you_believe_you_can_support_teachers_in_the_following_areas_br_i_believe_i_can_directly_or_indirectly_support_teachers_to_use_their_students_prior_knowledge_to_help_them_make_sense_of_new_information",
                            "please_rate_the_extent_to_which_you_believe_you_can_support_teachers_in_the_following_areas_br_i_believe_i_can_directly_or_indirectly_support_teachers_to_revise_instructional_material_to_include_a_better_representation_of_cultural_groups",
                            "please_rate_the_extent_to_which_you_believe_you_can_support_teachers_in_the_following_areas_br_i_believe_i_can_directly_or_indirectly_support_teachers_to_teach_the_curriculum_to_students_with_unfinished_learning",
                            "please_rate_the_extent_to_which_you_believe_you_can_support_teachers_in_the_following_areas_br_i_believe_i_can_directly_or_indirectly_support_teachers_to_teach_the_curriculum_to_students_who_are_from_historically_marginalized_groups")
        
        ## Only reversed columns for later calculations
        reversed <- c(equitable_questions %>% dplyr::filter(reverse == T) |> dplyr::pull(question),
                      high_expectations_questions %>% dplyr::filter(reverse == T) |> dplyr::pull(question))
        
        selected <- c(equitable_questions |> dplyr::pull(question),
                      high_expectations_questions |> dplyr::pull(question),
                      crse_questions)
        
        ## Reformatted column names for later
        equitable_reformat <- purrr::map_chr(equitable_questions$question, reformat_cols)
        high_expectations_reformat <- purrr::map_chr(high_expectations_questions$question, reformat_cols)
        crse_reformat <- purrr::map_chr(crse_questions, reformat_cols)
      } else if (admin == T & prepost == T & rename == T) {
        equitable_questions <- tibble::tibble(
          question = c("To what extent do you agree or disagree with the following statements? - Teachers should be color blind when it comes to their teaching - they shouldn't think of students in terms of their race or ethnicity.",
                       "To what extent do you agree or disagree with the following statements? - The gap in the achievement among students of different races is about poverty, not race._2",
                       "To what extent do you agree or disagree with the following statements? - I think about my own background and experiences and how those affect my instructional leadership."),
          reverse = c(T, T, F)
        )
        
        high_expectations_questions <- tibble::tibble(
          question = c("To what extent do you agree or disagree with the following statements? - Teachers should try to keep in mind the limits of students' ability and give them assignments that they know they can do so that they do not become discouraged.",
                       "To what extent do you agree or disagree with the following statements? - Before students are asked to engage in complex learning tasks, they need to have a solid grasp of basic skills._2",
                       "To what extent do you agree or disagree with the following statements? - It is not fair to ask students who are struggling with English to take on challenging academic assignments._2",
                       "To what extent do you agree or disagree with the following statements? - Teachers should provide all students the opportunity to work with grade-level texts and tasks._2"),
          reverse = c(T, T, T, F)
        )
        
        crse_questions <- c("Please rate the extent to which you believe you can support teachers in the following areas.<br>I believe I can directly or indirectly support teachers to... - Adapt instruction to meet the needs of their students",
                            "Please rate the extent to which you believe you can support teachers in the following areas.<br>I believe I can directly or indirectly support teachers to... - Identify ways that the school culture (e.g., values, norms, and practices) is different from their students' home culture",
                            "Please rate the extent to which you believe you can support teachers in the following areas.<br>I believe I can directly or indirectly support teachers to... - Use their students' prior knowledge to help them make sense of new information",
                            "Please rate the extent to which you believe you can support teachers in the following areas.<br>I believe I can directly or indirectly support teachers to... - Revise instructional material to include a better representation of cultural groups",
                            "Please rate the extent to which you believe you can support teachers in the following areas.<br>I believe I can directly or indirectly support teachers to... - Teach the curriculum to students with unfinished learning",
                            "Please rate the extent to which you believe you can support teachers in the following areas.<br>I believe I can directly or indirectly support teachers to... - Teach the curriculum to students who are from historically marginalized groups")
        
        
        if (grouping != "crse") {
          selected <- c(equitable_questions |> dplyr::pull(question),
                        high_expectations_questions |> dplyr::pull(question),
                        crse_questions)
          ## Only reversed columns for later calculations
          reversed <- c(equitable_questions %>% dplyr::filter(reverse == T) |> dplyr::pull(question),
                        high_expectations_questions %>% dplyr::filter(reverse == T) |> dplyr::pull(question))
        } else {
          selected <- crse_questions
          reversed <- NULL
          equitable_questions$question <- NULL 
          high_expectations_questions$question <- NULL
        }
        
        
        ## Reformatted column names for later
        equitable_reformat <- purrr::map_chr(equitable_questions$question, reformat_cols)
        high_expectations_reformat <- purrr::map_chr(high_expectations_questions$question, reformat_cols)
        crse_reformat <- purrr::map_chr(crse_questions, reformat_cols)
      } else {
        equitable_questions <- tibble::tibble(
          question = c("to_what_extent_do_you_agree_or_disagree_with_the_following_statements_i_am_color_blind_when_it_comes_to_my_teaching_i_don_t_think_of_my_students_in_terms_of_their_race_or_ethnicity",
                       "to_what_extent_do_you_agree_or_disagree_with_the_following_statements_the_gap_in_the_achievement_among_students_of_different_races_is_about_poverty_not_race",
                       "to_what_extent_do_you_agree_or_disagree_with_the_following_statements_i_think_about_my_own_background_and_experiences_and_how_those_affect_my_instruction"),
          reverse = c(T, T, F)
        )
        
        high_expectations_questions <- tibble::tibble(
          question = c("to_what_extent_do_you_agree_or_disagree_with_the_following_statements_i_try_to_keep_in_mind_the_limits_of_my_students_ability_and_give_them_assignments_that_i_know_they_can_do_so_that_they_do_not_become_discouraged",
                       "to_what_extent_do_you_agree_or_disagree_with_the_following_statements_before_students_are_asked_to_engage_in_complex_learning_tasks_they_need_to_have_a_solid_grasp_of_basic_skills",
                       "to_what_extent_do_you_agree_or_disagree_with_the_following_statements_it_is_not_fair_to_ask_students_who_are_struggling_with_english_to_take_on_challenging_academic_assignments",
                       "to_what_extent_do_you_agree_or_disagree_with_the_following_statements_teachers_should_provide_all_students_the_opportunity_to_work_with_grade_level_texts_and_tasks"),
          reverse = c(T, T, T, F)
        )
        
        crse_questions <- c("please_rate_your_confidence_on_the_following_items_br_i_am_able_to_adapt_instruction_to_meet_the_needs_of_my_students",
                            "please_rate_your_confidence_on_the_following_items_br_i_am_able_to_identify_ways_that_the_school_culture_e_g_values_norms_and_practices_is_different_from_my_students_home_culture",
                            "please_rate_your_confidence_on_the_following_items_br_i_am_able_to_use_my_students_prior_knowledge_to_help_them_make_sense_of_new_information",
                            "please_rate_your_confidence_on_the_following_items_br_i_am_able_to_revise_instructional_material_to_include_a_better_representation_of_cultural_groups",
                            "please_rate_your_confidence_on_the_following_items_br_i_am_able_to_teach_the_curriculum_to_students_with_unfinished_learning",
                            "please_rate_your_confidence_on_the_following_items_br_i_am_able_to_teach_the_curriculum_to_students_who_are_from_historically_marginalized_groups")
        
        ## Only reversed columns for later calculations
        reversed <- c(equitable_questions %>% dplyr::filter(reverse == T) |> dplyr::pull(question),
                      high_expectations_questions %>% dplyr::filter(reverse == T) |> dplyr::pull(question))
        
        selected <- c(equitable_questions |> dplyr::pull(question),
                      high_expectations_questions |> dplyr::pull(question),
                      crse_questions)
        
        ## Reformatted column names for later
        equitable_reformat <- purrr::map_chr(equitable_questions$question, reformat_cols)
        high_expectations_reformat <- purrr::map_chr(high_expectations_questions$question, reformat_cols)
        crse_reformat <- purrr::map_chr(crse_questions, reformat_cols)
      }
      
      if (grouping == "crse") {
        selected <- crse_questions
        reversed <- NULL
        equitable_questions$question <- NULL 
        high_expectations_questions$question <- NULL
      }
      
      data_sums <- data |>
        ## Select just relevant columns
        dplyr::select(dplyr::all_of(selected), site) |>
        ## Only grab numbers for averaging
        dplyr::mutate(dplyr::across(!site, ~ readr::parse_number(as.character(.x)))) |>
        ## If reversed subtract from 5 
        dplyr::mutate(dplyr::across(reversed, ~ 6 - .x)) |>
        dplyr::group_by(site) |>
        ## Summarise everything with scoring
        dplyr::summarise(dplyr::across(c(equitable_questions$question, high_expectations_questions$question), ~ diagnostic_score(x = .x)),
                         dplyr::across(crse_questions, ~ round(10 * mean(.x, na.rm = T), 2))) |>
        ## Rename with reformatting function
        dplyr::rename_with(.cols = !site, ~ reformat_cols(.x)) |>
        ## Make long format
        tidyr::pivot_longer(!site, names_to = "Question", values_to = "Percent") |>
        ## Add groups and red coloring for negative items
        dplyr::mutate(groups = dplyr::case_when(Question %in% equitable_reformat ~ "Recognition of Race & Culture",
                                                Question %in% high_expectations_reformat ~ "High Expectations & Beliefs",
                                                Question %in% crse_reformat ~ "Self-efficacy in CRSE Practices"),
                      Question = ifelse(Question %in% c(equitable_reformat[1:2], high_expectations_reformat[1:3]),
                                        paste0("<span style = 'color:#cc3336;'>", Question, "</span>"),
                                        Question))
      
      ## Conditionals for summarisation of table
      if (summarise == T & grouping != "crse") {
        data_sums_final <- data_sums |>
          dplyr::select(-Question) |>
          dplyr::group_by(groups, site) |>
          dplyr::summarise(Percent = round(mean(Percent, na.rm = T)))
        
        data_sums_final <- tibble::tibble(groups = "<b>Overall Score</b>", 
                                          Percent = round(mean(data_sums_final$Percent, na.rm = T))) |>
          dplyr::bind_rows(data_sums_final) |>
          dplyr::relocate(groups, .before = 1)
        
      } else if (summarise == F & grouping == "equitable") {
        
        data_sums_final <- data_sums |>
          dplyr::mutate(Question = stringr::str_remove_all(Question, "To what extent do you agree or disagree\\<br\\>with the following statements\\? - |\\."),
                 Question = stringr::str_replace_all(Question, "\\<br\\>", " ")) |>
          dplyr::slice(c(1:3))
        
        data_sums_final <- tibble::tibble(groups = "<b>Overall Score</b>", 
                                          Question = "", 
                                          Percent = round(mean(data_sums_final$Percent, na.rm = T))) %>%
          dplyr::bind_rows(data_sums_final) |>
          dplyr::relocate(groups, .before = 1)
      } else if (summarise == F & grouping == "high_expectations") {
        data_sums_final <- data_sums |>
          mutate(Question = stringr::str_remove_all(Question, "To what extent do you agree or disagree\\<br\\>with the following statements\\? - |\\."),
                 Question = stringr::str_replace_all(Question, "\\<br\\>", " ")) |>
          slice(4:7)
        
        data_sums_final <- tibble::tibble(groups = "<b>Overall Score</b>", 
                                          Question = "", 
                                          Percent = round(mean(data_sums_final$Percent, na.rm = T))) |>
          dplyr::bind_rows(data_sums_final) |>
          dplyr::relocate(groups, .before = 1)
      }
      
      ## Conditionals for creating tables
      if (grouping == "summarise") {
        
        final_gt <- data_sums_final |>
          dplyr::mutate(site = replace_na(site, "All")) |>
          tidyr::drop_na(Percent) |>
          dplyr::rename(` ` = groups) |>
          dplyr::mutate(` ` = factor(` `, levels = c("<b>Overall Score</b>",
                                              "Recognition of Race & Culture",
                                              "High Expectations & Beliefs",
                                              "Self-efficacy in CRSE Practices"))) |>
          arrange(` `) |>
          mutate(` ` = as.character(` `))
        
        if (prepost == T) {
          final_gt
        } else {
          final_gt <- final_gt |>
            gt::gt() |>
            gt::tab_header(title = "Mindsets and Beliefs") |>
            gt::fmt_markdown(columns = c(` `)) |>
            # gtExtras::gt_color_box(
            #   columns = Percent,
            #   domain = 0:100,
            #   palette = "ggsci::blue_material"
            # ) %>%
            gt::fmt_percent(columns = "Percent",
                            decimals = 0,
                            scale_values = F) |>
            gt::data_color(
              columns = Percent,
              colors = scales::col_bin(
                palette = c(
                  TeachingLab::tl_palette(n = 8, color = "blue") |> magrittr::extract(c(4, 6, 8))
                ),
                domain = c(0, 100),
                bins = c(0, 39, 79, 100)
              )
            ) %>%
            {
              if (!is.null(n_size)) gt::tab_source_note(data = ., source_note = paste0("n ranges from ", n_size[1], " to ", n_size[2])) else .
            } %>%
            {
              if (!is.null(n_size_single)) gt::tab_source_note(data = ., source_note = paste0("n = ", n_size_single[1])) else .
            } %>%
            TeachingLab::gt_theme_tl(align = "left")
        }
        
      } else {
        
        if (grouping %!in% c("equitable", "high_expectations")) {
          data_sums_final <- tibble::tibble(groups = "<b>Overall Score</b>", 
                                            Question = "", 
                                            Percent = round(mean(data_sums$Percent, na.rm = T))) %>%
            dplyr::bind_rows(data_sums) %>%
            dplyr::relocate(groups, .before = 1)
        }
        
        final_gt <- data_sums_final |>
          dplyr::mutate(groups = stringr::str_replace_all(
            as.character(stringr::str_extract_all(groups, "<b>Overall Score</b>")), "character\\(0\\)", " ")
          ) |>
          dplyr::rename(` ` = `groups`) %>%
          {
            if (explain == T) dplyr::mutate(., `Response (answer)` = dplyr::case_when(Percent == 20 & !stringr::str_detect(Question, "span") ~ "Strongly disagree",
                                                                                      Percent == 40 & !stringr::str_detect(Question, "span") ~ "Disagree",
                                                                                      Percent == 60 & !stringr::str_detect(Question, "span") ~ "Neither agree nor disagree",
                                                                                      Percent == 80 & !stringr::str_detect(Question, "span") ~ "Agree",
                                                                                      Percent == 100 & !stringr::str_detect(Question, "span") ~ "Strongly agree",
                                                                                      Percent == 20 & stringr::str_detect(Question, "span") ~ "Strongly agree",
                                                                                      Percent == 40 & stringr::str_detect(Question, "span") ~ "Agree",
                                                                                      Percent == 60 & stringr::str_detect(Question, "span") ~ "Neither agree nor disagree",
                                                                                      Percent == 80 & stringr::str_detect(Question, "span") ~ "Disagree",
                                                                                      Percent == 100 & stringr::str_detect(Question, "span") ~ "Strongly disagree",
                                                                                      ` ` == "<b>Overall Score</b>" ~ " ")) else .
          }
        
        if (prepost == T) {
          final_gt
        } else {
          final_gt <- final_gt |>
            gt::gt() |>
            gt::tab_header(title = md(paste0("**", data_sums_final$groups[2], "**"))) %>%
            gt::fmt_markdown(columns = c("Question", " ")) %>%
            # gtExtras::gt_color_box(
            #   columns = Percent,
            #   domain = 0:100,
            #   palette = "ggsci::blue_material"
            # ) %>%
            gt::fmt_percent(columns = "Percent",
                            decimals = 0,
                            scale_values = F) %>%
            gt::data_color(
              columns = Percent,
              colors = scales::col_bin(
                palette = c(
                  TeachingLab::tl_palette(n = 8, color = "blue") |> magrittr::extract(c(4, 6, 8))
                ),
                domain = c(0, 100),
                bins = c(0, 39, 79, 100)
              )
            ) |>
            gt::cols_align(align = "left",
                           columns = "Question") %>%
            gt::tab_footnote(footnote = "For the items in red, responses that align to equitable mindsets are strongly disagree/disagree. They have been reverse coded for these analyses, so that for all items, higher percentages correspond to holding equitable mindsets.",
                             locations = cells_column_labels("Question")) %>%
            {
              if (!is.null(n_size)) gt::tab_source_note(data = ., source_note = paste0("n ranges from ", n_size[1], " to ", n_size[2])) else .
            } %>%
            {
              if (!is.null(n_size_single)) gt::tab_source_note(data = ., source_note = paste0("n = ", n_size_single[1])) else .
            } %>%
            TeachingLab::gt_theme_tl(align = "left")
        }
        }
        
    } else if (grouping %in% c("crse")) {
      
      if (rename == F) {
        selected <- c("please_rate_your_confidence_on_the_following_items_br_i_am_able_to_adapt_instruction_to_meet_the_needs_of_my_students",
                      "please_rate_your_confidence_on_the_following_items_br_i_am_able_to_identify_ways_that_the_school_culture_e_g_values_norms_and_practices_is_different_from_my_students_home_culture",
                      "please_rate_your_confidence_on_the_following_items_br_i_am_able_to_use_my_students_prior_knowledge_to_help_them_make_sense_of_new_information",
                      "please_rate_your_confidence_on_the_following_items_br_i_am_able_to_revise_instructional_material_to_include_a_better_representation_of_cultural_groups",
                      "please_rate_your_confidence_on_the_following_items_br_i_am_able_to_teach_the_curriculum_to_students_with_unfinished_learning",
                      "please_rate_your_confidence_on_the_following_items_br_i_am_able_to_teach_the_curriculum_to_students_who_are_from_historically_marginalized_groups")
      } else {
        selected <- c("Please rate your confidence on the following items. <br>I am able to... - Adapt instruction to meet the needs of my students",
                      "Please rate your confidence on the following items. <br>I am able to... - Identify ways that the school culture (e.g., values, norms, and practices) is different from my students' home culture",
                      "Please rate your confidence on the following items. <br>I am able to... - Use my students' prior knowledge to help them make sense of new information",
                      "Please rate your confidence on the following items. <br>I am able to... - Revise instructional material to include a better representation of cultural groups",
                      "Please rate your confidence on the following items. <br>I am able to... - Teach the curriculum to students with unfinished learning",
                      "Please rate your confidence on the following items. <br>I am able to... - Teach the curriculum to students who are from historically marginalized groups")
      }
      
      data_sums <- data  |> 
        ## Select just relevant columns
        dplyr::select(dplyr::all_of(selected)) |>
        ## Only grab numbers for averaging
        dplyr::mutate(dplyr::across(dplyr::everything(), ~ readr::parse_number(as.character(.x)))) |>
        ## Summarise everything with scoring
        dplyr::summarise(dplyr::across(dplyr::everything(), ~ round(mean(.x, na.rm = T), 2))) |>
        dplyr::ungroup() |>
        ## Rename with reformatting function
        dplyr::rename_with( ~ reformat_cols(.x)) |>
        ## Make long format
        tidyr::pivot_longer(dplyr::everything(), names_to = "Question", values_to = "Percent")
      
      ## Conditionals for summarisation of table
      if (summarise == T) {
        data_sums_final <- data_sums |>
          dplyr::mutate(Percent = Percent * 10,
                        Question = str_replace_all(Question, " e g ", " e\\.g\\. "))
        
        data_sums_final <- tibble::tibble(Question = "<b>Overall Score</b>", 
                                          Percent = round(mean(data_sums_final$Percent, na.rm = T))) |>
          dplyr::bind_rows(data_sums_final)
        
        final_gt <- data_sums_final %>%
          {
            if (explain == T) dplyr::mutate(., `Response (1-10 scale)` = dplyr::case_when(Percent == 10 & Question != "<b>Overall Score</b>" ~ "1",
                                                                                          Percent == 20 & Question != "<b>Overall Score</b>" ~ "2",
                                                                                          Percent == 30 & Question != "<b>Overall Score</b>" ~ "3",
                                                                                          Percent == 40 & Question != "<b>Overall Score</b>" ~ "4",
                                                                                          Percent == 50 & Question != "<b>Overall Score</b>" ~ "5",
                                                                                          Percent == 60 & Question != "<b>Overall Score</b>" ~ "6",
                                                                                          Percent == 70 & Question != "<b>Overall Score</b>" ~ "7",
                                                                                          Percent == 80 & Question != "<b>Overall Score</b>" ~ "8",
                                                                                          Percent == 90 & Question != "<b>Overall Score</b>" ~ "9",
                                                                                          Percent == 100 & Question != "<b>Overall Score</b>" ~ "10",
                                                                                          Question == "<b>Overall Score</b>" ~ " ")) else .
          }
        
        if (prepost == T) {
          final_gt
        } else {
          final_gt %>%
            gt::gt() |>
            gt::fmt_percent(columns = "Percent",
                            decimals = 0,
                            scale_values = F) |>
            gt::data_color(
              columns = Percent,
              colors = scales::col_bin(
                palette = c(
                  TeachingLab::tl_palette(n = 8, color = "blue") |> magrittr::extract(c(4, 6, 8))
                ),
                domain = c(0, 100),
                bins = c(0, 39, 79, 100)
              )
            ) |>
            gt::fmt_markdown(columns = "Question") |>
            gt::cols_align(align = "left",
                           columns = c(Question, Percent)) %>%
            {
              if (!is.null(n_size)) gt::tab_source_note(data = ., source_note = paste0("n ranges from ", n_size[1], " to ", n_size[2])) else .
            } %>%
            {
              if (!is.null(n_size_single)) gt::tab_source_note(data = ., source_note = paste0("n = ", n_size_single[1])) else .
            } %>%
            TeachingLab::gt_theme_tl(align = NULL) %>%
            {
              if (explain == T) gt::cols_align(., align = "center", columns = c(`Response (1-10 scale)`)) else .
            }
        }
        
      } else if (grouping == "crse") {
        data_sums_final <- data_sums #|>
          # mutate(Question = str_replace_all(Question, " e g ", " e\\.g\\. "))
        
        data_sums_final <- tibble::tibble(groups = "<b>Overall Score</b>", 
                                          Question = "", 
                                          Percent = round(mean(data_sums_final$Percent, na.rm = T), 2)) |>
          dplyr::bind_rows(data_sums_final) |>
          dplyr::relocate(groups, .before = 1) |>
          dplyr::mutate(groups = tidyr::replace_na(groups, " "))
        
        final_gt <- data_sums_final %>%
          dplyr::rename(Average = Percent) %>%
          {
            if (explain == T) dplyr::mutate(., `Response (1-10 scale)` = dplyr::case_when(Percent == 10 ~ "1",
                                                                                          Percent == 20 ~ "2",
                                                                                          Percent == 30 ~ "3",
                                                                                          Percent == 40 ~ "4",
                                                                                          Percent == 50 ~ "5",
                                                                                          Percent == 60 ~ "6",
                                                                                          Percent == 70 ~ "7",
                                                                                          Percent == 80 ~ "8",
                                                                                          Percent == 90 ~ "9",
                                                                                          Percent == 100 ~ "10",
                                                                                          ` ` == "<b>Overall Score</b>" ~ " ")) else .
          }
        
        if (prepost == T) {
          final_gt
        } else {
          final_gt %>%
            gt::gt(rowname_col = "groups") |>
            # gt::fmt_integer(columns = "Average") %>%
            gt::fmt_markdown(columns = "Question") |>
            gt::cols_align(align = "left",
                           columns = "Question") %>%
            {
              if (!is.null(n_size)) gt::tab_source_note(data = ., source_note = paste0("n ranges from ", n_size[1], " to ", n_size[2])) else .
            } %>%
            {
              if (!is.null(n_size_single)) gt::tab_source_note(data = ., source_note = paste0("n = ", n_size_single[1])) else .
            } %>%
            TeachingLab::gt_theme_tl(align = "left")
        }
      }
  }
    } else if (is.null(grouping) & summarise == T) {
    
    data_sums <- data |>
      ## Only grab numbers for averaging
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ readr::parse_number(as.character(.x)))) |> 
      ## Summarise everything with scoring
      dplyr::summarise(dplyr::across(dplyr::everything(), ~ round(mean(.x, na.rm = T), 2))) |> 
      ## Rename with reformatting function
      dplyr::rename_with( ~ reformat_cols(.x)) |> 
      ## Make long format
      tidyr::pivot_longer(tidyr::everything(), names_to = "Question", values_to = "Percent") |> 
      dplyr::summarise(`% Overall Score` = mean(Percent, na.rm = T))
    
    final_gt <- data_sums |> 
      gt::gt() |> 
      gt::fmt_percent(gt::everything(),
                    scale_values = F,
                    decimals = 0) %>%
      {
        if (!is.null(n_size)) gt::tab_source_note(data = ., source_note = paste0("n ranges from ", n_size[1], " to ", n_size[2])) else .
      } %>%
      TeachingLab::gt_theme_tl()
  }
  
  
  ## Conditionals for saving
  if (save == F) {
    final_gt
  } else {
    img_return <- gt::gtsave(data = final_gt,
                             path = "images/report_images",
                             filename = tempfile(fileext = ".png"))
    
    knitr::include_graphics(img_return)
  }
  
  
  
}
















