#############################################################################
#'@title Save rtweet tibble as compatible csv
#'
#'@description Saves csv file, appending "RT" and retweeted user name to text of retweets. Adds "text2" variable as backup.
#'@param rtweet_df A dataframe/tibble of tweets created by rtweet package through twitter API call.
#'@param path_for_csv_file a path and filename for output csv. Must include".csv"
#'@keywords twitter, rtweet, csv, compatibility
#'@export
#'@examples
#'df <- rtweet::get_timelines("jack", n=3200)
#'write_csv_compatible(df, "~/jack_tweets.csv")
###############################################################################
write_csv_compatible <-
  function(rtweet_df, path_for_csv_file) {
    require(rtweet, quietly = TRUE)
    require(dplyr, quietly = TRUE)
    require(magrittr, quietly = TRUE)
    require(readr, quietly = TRUE)

    mod_file <- rtweet_df %>%
      dplyr::mutate(text2 = text) %>%
      dplyr::mutate(text = ifelse(
        is_retweet == TRUE,
        paste0("RT @", retweet_screen_name, ": ", text2),
        text2
      ))
    readr::write_csv(rtweet::flatten(mod_file), path_for_csv_file)
  }
