#' rtweetXtras: Additional Analysis Functions for rtweet
#'
#' The rtweetXtras package includes the following functions:
#' hashtagcloud, profilecloud, bar_plot_mentions, common_follower_analysis, account_activity, rtweet_net, save_csv_edgelist, get_followers_fast, create_gexf
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
#'      (inspired by python script by twitter user "@Conspirat0r")
#' @section rtweet_net function
#'      This function creates an igraph network graph from a tibble of tweets details created by rtweet functions
#'      (e.g. search_tweets, get_timeline, parse_stream, lookup_statuses, lookup_tweets etc.)
#' @section save_csv_edgelist function
#'      This function saves an igraph edgelist as a csv for export to network mapping software such as Gephi.
#' @section get_followers_fast function
#'      This function consolidates the process of getting followers and looking up the user details.
#' @section get_friends_fast function
#'      This function consolidates the process of getting friends and looking up the user details.
#' @section number_followers function
#'      This function consolidates the process of numbering followers and indicating earliest possible following date.
#' @section create_gexf function
#'      This function creates a gexf network file of a mentions network for use with Gephi software
#' @section write_csv_compatible function
#'      Saves csv file, appending "RT" and retweeted user name to text of retweets. Adds "text2" variable as backup.
#' @section check_shadowban function
#'       Checks a particular twitter screen_name for temporary search or reply visibility reduction.
#' @section check_shadowban_list  function
#'       Wrapper for check_shadowban for a list of accounts.
#' @section rehydrate_got3_statuses
#'       Rehydrates data from GetOldTweets3 python package to conform to rtweet dataframe.
#' @section snscrape_search function
#'        Wrapper to undertake historical twitter searches calling Python snscrape library.
#' @docType package
#' @name rtweetXtras
NULL
