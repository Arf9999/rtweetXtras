####################################################################################
#'@title Number followers and indicate earliest following date
#'@description This function consolidates the process of numbering followers and indicating earliest following date.
#'WARNING: Obviously not accurate for dates prior to the creation of the target account - manual adjustment required.
#'requires an rtweet dataframe as returned by a combination of get_followers() and lookup_users().
#'@param follower_df Required. An rtweet dataframe of followers.
#'@keywords twitter, rtweet
#'@export
#'@examples
#'jacks_followers <- rtweet::lookup_users(unlist(rtweet::get_followers("jack", n=5000)[,"user_id"]))
#'jacks_followers_numbered <- number_followers(jacks_followers)
#######################################################################################

number_followers <- function(follower_df) {
  require(dplyr, quietly = TRUE)
  require(magrittr, quietly = TRUE)

  follower_df <- follower_df %>%
    mutate(follower_no = (dense_rank(desc(row_number(

    )))))

  follower_df$earliest_follow <- NULL
  creation_dates <- follower_df$account_created_at
  for (x in 1:nrow(follower_df)) {
    follower_df[x, "earliest_follow"] <-
      max(creation_dates[x:nrow(follower_df)])
  }
  return(follower_df)
}
