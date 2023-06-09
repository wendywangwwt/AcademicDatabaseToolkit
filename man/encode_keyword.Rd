% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{encode_keyword}
\alias{encode_keyword}
\title{Encode keywords. For keywords that have special character such as space or hyphen.}
\usage{
encode_keyword(keyword, db, field)
}
\arguments{
\item{keyword}{A string of one keyword, or a vector of keywords. When a vector
of multiple keywords are provided, this is a relationship="and" case.}
}
\description{
Encode keywords. For keywords that have special character such as space or hyphen.
}
