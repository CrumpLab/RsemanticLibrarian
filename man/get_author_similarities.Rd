% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_author_similarities.R
\name{get_author_similarities}
\alias{get_author_similarities}
\title{Compute Similarities between one author, and all other authors}
\usage{
get_author_similarities(a_name, auth_vectors = AuthorVectors,
  authors = author_list)
}
\arguments{
\item{a_name}{character, name of an author}

\item{auth_vectors}{matrix of semantic vectors for each author}

\item{authors}{character vector of all author names}
}
\value{
dataframe of similarities between target author and all other authors
}
\description{
Compute Similarities between one author, and all other authors
}
\examples{

sims <- get_author_similarities(author_list[1])
}
