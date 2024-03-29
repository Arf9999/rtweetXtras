% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/followerdotplot.R
\name{follower_dot_plot}
\alias{follower_dot_plot}
\title{Plot function for datasets created by rtweetXtras::get_followers_fast}
\usage{
follower_dot_plot(
  follower_df,
  point_colour = "statuses_count",
  show_legend = TRUE,
  suppress_warnings = TRUE,
  include_loess_smooth = FALSE,
  include_lm = FALSE,
  print_immediately = TRUE,
  log_transform = FALSE,
  viridis_option = "magma"
)
}
\arguments{
\item{follower_df}{Required. A dataframe or tibble created with rtweetXtras::get_followers_fast.}

\item{point_colour}{maps the dot colour to one of:
"statuses_count", "lang", "source", "favourites_count", "followers_count", or "earliest_follow". Default is "statuses_count".}

\item{show_legend}{Display chart legend. Default is TRUE,
but where there are many categorical items (e.g. "lang or "source"), recommended to set to FALSE.}

\item{suppress_warnings}{Suppress ggplot warnings from printed plots. Default is TRUE.
Recommended to use default for .rmd knitting.}

\item{include_loess_smooth}{Include loess smoothed curve with se shading. Default is FALSE}

\item{include_lm}{Include a linear model line with se shading. Default is FALSE}

\item{print_immediately}{Force the function to print the plot immediately. Default is TRUE,
set to FALSE to assign the ggplot object.}

\item{log_transform}{Transform the legend scale to a log scale. Default is FALSE.
Useful if there is a concentration of small values that makes it
difficult to see any graduation. Ignored for categorical and date scales.}

\item{viridis_option}{Sets the viridis colour palette. Default is "magma"
Options are "viridis", "plasma", "magma", "inferno" and "cividis"}
}
\description{
This builds a ggplot2 scatter plot with the creation date of twitter followers mapped to the order in which they followed.
Various options of dot colouring are available to examine the follower's attributes.
Optionally, a loess smoothed curve and/or a linear model can be overlaid to analyse the change in follower creation dates over time.
}
\examples{
arfness_followers <- rtweetXtras::get_followers_fast("arfness", file_path = "~/")
follower_dot_plot (arfness_followers, point_colour = "followers_count", show_legend = TRUE, log_transform =TRUE)
}
\keyword{ggplot}
\keyword{rtweet,}
\keyword{rtweetXtras,}
\keyword{twitter,}
