% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/perspectivefunctions.R
\name{perspective_rtweet}
\alias{perspective_rtweet}
\title{Query an rtweet (or rtweetXtras) dataframe of tweets text}
\usage{
perspective_rtweet(rtweet_df, api_key, QPS = 1, summary = TRUE)
}
\arguments{
\item{rtweet_df}{A dataframe of tweets returned by rtweet or rtweetXtras}

\item{api_key}{A Perspective API key. Required.}

\item{QPS}{Queries per second allowed by the Perspective API for the api_key (default is 1)}

\item{summary}{TRUE delivers a condensed response, removing non-essential columns.
FALSE delivers original rtweet_df with additional columns for measures}
}
\description{
Queries the Google Perspective API and returns a tibble of tweets and measures
}
\examples{
df <- rtweet::search_tweets("summer", n=200)
perspective_df <- perspective_rtweet(df, api_key = YourAPIkeyhere, QPS = 1, summary = TRUE)
}
\keyword{Toxicity,}
\keyword{analysis}
\keyword{text}
