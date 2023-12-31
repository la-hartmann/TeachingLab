% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scoring_functions.R
\name{score_knowledge_question}
\alias{score_knowledge_question}
\title{Knowledge Assessments Scoring}
\usage{
score_knowledge_question(data, question, correct)
}
\arguments{
\item{data}{the data to evaluate}

\item{question}{a vector of questions}

\item{correct}{a vector of correct answers}
}
\value{
a dataframe of format question1, question2, question3, with percents as answers
}
\description{
Score a knowledge assessments question by selecting a grouping and
choosing the percent that is correct
}
