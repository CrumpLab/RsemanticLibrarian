% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sl_clean_vector.R
\name{sl_clean_vector}
\alias{sl_clean_vector}
\title{Clean a vector of sentences}
\usage{
sl_clean_vector(words)
}
\arguments{
\item{words}{character vector}
}
\value{
list each sentence is it's own character vector
}
\description{
Clean a vector of sentences
}
\examples{
sentences <- c("hello my name is","this is another sentence")
sl_clean_vector(sentences)
}
