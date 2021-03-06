% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_search_article_similarities.R
\name{get_search_article_similarities}
\alias{get_search_article_similarities}
\title{Similarities between term vector and article vector}
\usage{
get_search_article_similarities(search_terms, d_words = dictionary_words,
  w_vectors = WordVectors, a_vectors = article_vectors,
  a_df = article_df, query_type)
}
\arguments{
\item{search_terms}{character vector containing search terms (words must appear in dictionary)}

\item{d_words}{character vector containing the corpus dictionary, the indexes of the words in the dictionary should match the row indices of the \code{WordVectors} matrix}

\item{w_vectors}{matrix containing semantic vectors for each word (e.g., \code{WordVectors} from the sample data in this package)}

\item{a_vectors}{matrix containing semantic vectors for each article (e.g., \code{article_vectors} from the sample data)}

\item{a_df}{dataframe containing necessary abstract information (e.g., \code{article_df} from sample data)}

\item{query_type}{integer, 1 = compound search, 2 = AND search, 3 = OR search}
}
\value{
a dataframe containing article information
}
\description{
Similarities between term vector and article vector
}
\details{
The returned dataframe includes the following:
\itemize{
\item \code{formatted_column}: html printable abstract information
\item \code{title}: abstract title
\item \code{wrap_title}: wrapped title with html breaks
\item \code{year}: abstract year
\item \code{index}: index refering to the row vector in the \code{article_vectors} matrix
\item \code{Similarity}: cosine similarity between search terms and current abstract
}
}
\examples{
search <- c("president")
article_dataframe <- get_search_article_similarities(search,query_type=1)
knitr::kable(head(article_dataframe))

}
