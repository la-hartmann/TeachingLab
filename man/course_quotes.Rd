% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table_makers.R
\name{course_quotes}
\alias{course_quotes}
\title{Dashboard End of Course Quotes}
\usage{
course_quotes(
  data = TeachingLab::get_course_survey(),
  size = 10,
  n = 3,
  all = F,
  save = NULL,
  include_columns = NULL
)
}
\arguments{
\item{data}{the data to be input}

\item{size}{the number of rows of quotes to return}

\item{n}{The number of words to highlight}

\item{all}{F by default, whether or not to return ALL data}

\item{save}{NULL by default, a save path to make inside current package directory}

\item{include_columns}{A vector of additional columns to include in table}
}
\value{
Returns a gt, unless save in which case it will return a saved file with gtsave
}
\description{
Creates a gt table for qualitative responses of the end of course survey
}
