#' gtable_remove_grob
#'
#' Helper function to remove grobs by name, from gtables
#'
#' @param g, gtable with the grob removed
#' @param pattern grob name or pattern to match
#'
#' @return g, with pattern removed.
gtable_remove_grob <- function(g, pattern = "guide-box") {
  matches <- c(grepl(pattern = pattern, g$layout$name))

  g$layout <- g$layout[!matches, , drop = FALSE]

  g$grobs <- g$grobs[!matches]
  return(g)
}

#' gtable_extract_grob
#'
#' Helper function to extract a grob from gtables by name.
#'
#' @param g, the gtable to extract the grob
#' @param pattern, grob name or pattern to match
#'
#' @return g, a grob matching the specified pattern
gtable_extract_grob <- function(g, pattern = "guide-box") {
  matches <- grepl(pattern = pattern, g$layout$name)

  g$layout <- g$layout[matches, , drop = FALSE]

  g$grobs <- g$grobs[matches]
  return(g)
}


#' Five thirty-eight style formatter for Ratios
#'
#' @param labels vector of labels
#'
#' @return formatted ratio labels
#' @export
scale_ratio_labels <- function(labels) {
  labels_out <- as.character(labels)

  labels_out[length(labels)] <- sprintf("%s:1", as.character(labels[length(labels)]))

  return(labels_out)
}

#' Five thirty-eight style formatter for percentages
#'
#' @param labels vector of labels
#'
#' @return formatted percent labels
#' @export
scale_percent_labels <- function(labels) {
  labels <- labels * 100

  labels[length(labels)] <- paste0(labels[length(labels)], "%")

  return(labels)
}

#' Five thirty-eight style formatter for currency
#'
#' @param labels vector of labels
#'
#' @return formatted percent labels
#' @export
scale_dollar_labels <- function(labels) {
  labels <- labels

  labels[length(labels)] <- paste0(labels[length(labels)], "$")

  return(labels)
}


#' @title Calculate nps score
#'
#' @param x A vector of nps scores
#' @return Returns the nps score
#' With formula `%` promoters - `%` detractors where promoters are 9 or 10 ratings and detractors are 0 to 6
#' @export

calc_nps <- function(x) {
  nps <- round((
    (sum(x == 10 | x == 9, na.rm = T) / sum(!is.na(x))) -
      (sum(x == 6 | x == 5 | x == 4 | x == 3 | x == 2 | x == 1 | x == 0, na.rm = T) / sum(!is.na(x)))
  ) * 100, 2)
  return(nps)
}

#' @title Find elements x not in a vector y
#'
#' @name notin
#' @aliases notin
#' @param x A vector of what shouldn't exist
#' @param y A vector to check against
#' @return Returns elements not in vector
#' @export

"%!in%" <- function(x, y) {
  !("%in%"(x, y))
}


#' @title Set Color in rmd with HTML or LaTex
#' @description Creates code to generate a color in x text with color, color.
#'
#' @param x the text to be colored
#' @param color the color of the text
#' @return Returns a string with code to render the correctly colored text
#' @export

colorize <- function(x, color) {
  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}{%s}", color, x)
  } else if (knitr::is_html_output()) {
    sprintf(
      "<span style='color: %s;font-weight:bold;'>%s</span>", color,
      x
    )
  } else {
    x
  }
}



#' @title Round
#' @description round that actually round up 0.5 as it should be
#' @param x the vector to round
#' @param n the number of digits to round
#' @description rounding function as an alternative to round due to occasional decimal errors and problems with rounding from 0.5
#' @return the vector provided rounded

round2 <- function(x, n) {
  posneg <- sign(x)
  z <- abs(x) * 10^n
  z <- z + 0.5 + sqrt(.Machine$double.eps)
  z <- trunc(z)
  z <- z / 10^n
  z * posneg
}


#' @title HTML Text Wrapping
#' @description Takes a string and inserts <br> at the requested intervals
#' @param string the string
#' @param n the width of the string before a <br> tag
#' @return the same string with <br> inserted at the requested interval
#'
#' @examples
#' html_wrap("a random string that has about 40 characters in it")
#' @export

html_wrap <- function(string, n = 40) {
  gsub(
    stringr::str_wrap(string = string, width = n),
    pattern = "\n", 
    replacement = "<br>"
  )
}



#' @title Get Season
#' @description Takes a date and finds the season
#' @param date the date
#' @return the season
#'
#' @examples
#' get_season(as.POSIXct("2016-01-01 12:00:00"))
#' @export

get_season <- function(date) {
  numeric.date <- 100 * as.numeric(format(Sys.Date(), "%m")) + as.numeric(format(Sys.Date(), "%d"))
  ## input Seasons upper limits in the form MMDD in the "break =" option:
  cuts <- cut(numeric.date, breaks = c(0, 319, 0620, 0921, 1220, 1231))
  # rename the resulting groups (could've been done within cut(...levels=) if "Winter" wasn't double
  levels(cuts) <- c("Winter", "Spring", "Summer", "Fall", "Winter")
  return(cuts)
}

#' @title Round to nearest even number
#' @description Takes a whole number and "rounds" it to the nearest even number.
#' @param x the number to round
#' @return an integer
#'
#' @examples
#' round_even(17)
#' @export

round_even <- function(x) {
  2 * ceiling(x / 2)
}

#' @title File path
#' @description Gives the file path without double slash bug
#' @param ... The file path
#' @param fsep the file separation
#' @return fp a file path
#' @export
file.path2 <- function(..., fsep = .Platform$file.sep) {
  fp <- gsub("//", "/", file.path(..., fsep = fsep))
  return(fp)
}


#' @title Percent Agree/Strongly agree
#' @description Calculates percent that are 4, 5, agree, or strongly agree
#' @param agree_col the vector or column to summarise
#' @return integer
#' @export

percent_agree <- function(agree_col) {
  100 * sum(agree_col %in% c("4", "5", "Strongly agree", "Agree", "(5) Strongly agree", "(4) Agree")) /
    sum(!is.na(agree_col))
}

#' @title Coalesce everything
#' @description Takes all columns and splices them into dots for combination
#' @param df the dataframe
#' @return the dataframe coalesced
#' @examples
#' df <- data.frame(
#'   a = c(1, 2, 3),
#'   b = c(NA, NA, NA),
#'   id = c("xxx", "xxx", "xxx")
#' )
#' df %>%
#'   dplyr::group_by(id) %>%
#'   dplyr::summarise_all(TeachingLab::coalesce_by_column)
#' @export

coalesce_by_column <- function(df) {
  return(dplyr::coalesce(!!!as.list(df)))
}


#' @title HTML/CSS Button Content Expander
#' @description Creates a button that will expand or hide content
#' @param before button default, collapsed/not collapsed
#' @param options unclear
#' @param envir also unclear
#' @param name chunk name
#' @return html wrapper
#' @export
drop1 <- function(before = T, options, envir, name) {
  if (before) {
    paste(
      "<p>",
      paste0('<button class="btn btn-primary collapsed" data-toggle="collapse" data-target="', name, '">'),
      "</button>",
      "</p>",
      paste0('<div class="collapse" id="', name, '">'),
      '<div class="card card-body">',
      sep = "\n"
    )
  } else {
    paste("</div>", "</div>", sep = "\n")
  }
}

#' @title Temporary Image Save and Return
#' @description Creates a temporary save location for an image and return it with `knitr::include_graphics`
#' @param img the object to save as an image
#' @return a png image
#' @export
temp_save <- function(img) {
  file_loc <- paste0(tempfile(), ".png")

  if ("gt_tbl" %in% class(img)) {
    print("saving...")

    img_return <- gt::gtsave(
      data = final_gt,
      path = "images/report_images",
      filename = tempfile(fileext = ".png")
    )

    return(img_return)
  }
}

#' @title Password Generator
#' @description Creates a password of n length
#' @param length the length of the password
#' @return a string
#' @export

password_generator <- function(length = 8) {
  sampling <- sample(c(letters, 1:9), size = length)
  paste(sampling, collapse = "")
}

#' @title Conditionally Perform Function
#' @description Wraps a function and conditionally performs it given certain arguments
#' @param fun the function to wrap
#' @examples
#' library(dplyr)
#' cond_filter <- conditionally(filter)
#' cond_select <- conditionally(select)
#' @export
conditionally <- function(fun) {
  function(first_arg, ..., execute) {
    if (execute) {
      return(fun(first_arg, ...))
    } else {
      return(first_arg)
    }
  }
}

#' @title ID Maker
#' @description takes initials and id column and makes an ID
#' @param initials the initials
#' @param birthday the birthday
#'
#' @export
id_maker <- function(initials, birthday) {
  paste0(tolower(initials), "_", birthday)
}

#' @title Generate one random number from a min and max
#' @param min the minimum number allowed
#' @param max the maximum number allowed
#' @return a single integer
#' @export
runif_round <- function(min, max) {
  round(runif(n = 1, min = min, max = max))
}

#' @title First Letter Uppercase
#' @param x string
#' @return string
#' @export
first_up <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


#' @title Random Vector
#' @param N number of scalars to create
#' @param M number to sum to
#' @param sd Standard Deviation
#' @param pos.only T
#'
#' @export
rand_vect <- function(N, M, sd = 1, pos.only = TRUE) {
  vec <- stats::rnorm(N, M / N, sd)
  if (abs(sum(vec)) < 0.01) vec <- vec + 1
  vec <- round(vec / sum(vec) * M)
  deviation <- M - sum(vec)
  for (. in seq_len(abs(deviation))) {
    vec[i] <- vec[i <- sample(N, 1)] + sign(deviation)
  }
  if (pos.only) {
    while (any(vec < 0)) {
      negs <- vec < 0
      pos <- vec > 0
      vec[negs][i] <- vec[negs][i <- sample(sum(negs), 1)] + 1
      vec[pos][i] <- vec[pos][i <- sample(sum(pos), 1)] - 1
    }
  }
  vec
}


#' @title String Replace
#' @description Detects a specific string pattern and replaces entire string with it
#' @param string the string to look within
#' @param string_detect the string to detect
#' @param string_replace the string to replace detected string with
#' @param print FALSE whether or not to print the string
#' @return a string
#' @export
string_replace <- function(string, string_detect, string_replace, print = FALSE) {
  assertthat::are_equal(length(string_detect), length(string_replace))

  new_string <-
    purrr::reduce2(string_detect, string_replace, ~ dplyr::if_else(
      stringr::str_detect(
        string = ..1,
        pattern = ..2
      ),
      ..3,
      as.character(..1)
    ),
    .init = string
    )

  if (print == TRUE) {
    print(new_string)
  }

  new_string
}

#' @title Agree/Strongly agree
#' @description Gets the percent that agree and strongly agree
#' @param data the data
#' @param question a string - the question to get the percentage for
#' @return a string
#' @export
agree_strongly_agree <- function(data, question) {
  
  percent <- data |>
    dplyr::filter(Question == question & Response %in% c("(4) Agree", "(5) Strongly agree",
                                                         "4 - Agree", "5 - Strongly agree",
                                                         "Agree", "Strongly agree")) |>
    dplyr::summarise(Percent = sum(Percent, na.rm = T)) |>
    dplyr::pull(Percent) |>
    round() |>
    paste0("%")
  
  if (data |>
      dplyr::filter(Question == question) |>
      nrow() == 0) {
    percent <- "There is no data to show the % that"
  }
  
  if (percent == "%" & nrow(data |>
                            dplyr::filter(Question == question)) == 0) {
    percent <- "There is no data to show the % that"
  } else if (percent == "%" & nrow(data |>
                                   dplyr::filter(Question == question)) != 0) {
    percent <- "0%"
  }
  
  percent
  
}

#' @title Agree/Strongly agree
#' @description Gets the percent that agree and strongly agree with different variable names
#' @param data the data
#' @param name a string - the question to get the percentage for
#' @return a string
#' @export
agree_strongly_agree2 <- function(data, name) {
  data |>
    dplyr::ungroup() |>
    dplyr::filter(name == name & value %in% c("Agree", "Strongly<br>agree")) |>
    tidyr::drop_na(subject_area) |>
    dplyr::group_by(subject_area, prepost) |>
    dplyr::summarise(percent = sum(percent))
}

#' @title Conditionally slice
#' @description Conditionally reduces the data
#' @param data the data
#' @param max a max length to slice the data for
#' @return a string
#' @export
conditional_slice_sample <- function(data, max) {
  df <- if (nrow(data) < max) {
    data
  } else {
    dplyr::slice_sample(.data = data, n = max)
  }

  df
}

#' @title Conditionally mutate
#' @description Conditionally changes the data
#' @param .data the data
#' @param condition a specific condition to test for
#' @param ... additional arguments for mutate
#' @param envir parent.frame()
#' @return a string
#' @export
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

#' @title Unique, sort, view
#' @description Gets all unique elements of a vector in a sorted view
#' @param vector the vector to view
#' @return a string
#' @examples
#' # example code
#'  c("stuff", "and", "more") |> single_sort_view()
#' @export
single_sort_view <- function(vector) {
  vector |>
    unique() |>
    sort() |> 
    tibble::view()
}

#' @title Get the percent of a column that equals specific values
#' @description Automatically scaled stacked bar chart with TL theming
#' @param data the data for the plotter to use, should include all columns of interest
#' @param percent_equal string inputs to find the percent of the column that equals those values
#' @return a percentage as a string
#' @export

tl_select_percent <- function(data, percent_equal) {
  
  sum_correct <- data |>
    table() |>
    magrittr::extract(percent_equal) |>
    sum(na.rm = T)
  
  sum_table <- sum(!is.na(data))
  
  sum_correct / sum_table
  
}