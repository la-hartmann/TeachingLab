% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graph_makers.R
\name{p_and_n}
\alias{p_and_n}
\title{Fake p1, p2, n1, n2 data}
\usage{
p_and_n(
  p1_range = c(40:80),
  p2_range = c(70:100),
  n1_range = stats::rnorm(1, mean = 80, sd = 10),
  n2_range = stats::rnorm(1, mean = 50, sd = 10)
)
}
\arguments{
\item{p1_range}{the range of values to sample for p1}

\item{p2_range}{the range of values to sample for p2}

\item{n1_range}{the range of values to sample for n1}

\item{n2_range}{the range of values to sample for n2}
}
\value{
a randomised dataset
}
\description{
Creates fake data for knowledge assessments in SXSW report
}
