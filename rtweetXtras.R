#' rtweetXtras: Additional Analysis Functions for rtweet
#'
#' The rtweetXtras package includes the following functions:
#' hashtagcloud, profilecloud, barplotmentions, common_follower_analysis, account_activity, rtweet_net and save_csv_edgelist
#'
#' @section hashtagcloud function
#'     This function creates a wordcloud (defaults to 200 terms) from terms in the "hashtags" column of an rtweet tibble of tweets
#' @section profilecloud function
#'     This function creates a wordcloud (defaults to 200 terms) from terms in the "description" column of an rtweet tibble of tweets
#' @section bar_plot_mentions function
#'     This function creates a barplot of mentioned screen_names (default top 20) in an rtweet tibble of tweets
#' @section common_follower_analysis function
#'     This function creates an UpSetR graph of common followers
#'     Code cribbed from Bob Rudis' 21 Recipes for Mining Twitter with Rtweet
#'     https://rud.is/books/21-recipes/visualizing-intersecting-follower-sets-with-upsetr.html
#' @section  account_activity function
#'      This function creates a bubble plot of account activity by hour of a single twitter screen_name
#'      (inspired by python script by twitter user "@@Conspirat0r")
#' @section rtweet_net function
#'      This function creates an igraph network graph from a tibble of tweets details created by rtweet functions
#'      (e.g. search_tweets, get_timeline, parse_stream, lookup_statuses, lookup_tweets etc.)
#' @section save_csv_edgelist function
#'      This function saves an igraph edgelist as a csv for export to network mapping software such as Gephi.
#' @docType package
#' @name rtweetXtras
NULL
