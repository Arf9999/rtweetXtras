#'@title Twitter visualization of common followers

#'@description This function creates an UpSetR graph of common followers
#'      Code cribbed from Bob Rudis' 21 Recipes for Mining Twitter with Rtweet
#'      https://rud.is/books/21-recipes/visualizing-intersecting-follower-sets-with-upsetr.html
#'@param user_list A list of user_names
#'@param follower_depth The number of most recent followers to graph. Defaults to 200
#'@param no_of_sets <- The number of sets in the UpSetR graph. Defaults to 7.
#'@param token A twitter oauth token. Default is NULL and will use token in Environment
#'@keywords twitter, rtweet, visualization, follower analysis
#'@export
#'@examples
#'users <- c("POTUS", "jack", "elonmusk")
#'common_follower_analysis(user_list = users, follower_depth = 300, no_of_sets = 7)


common_follower_analysis <-
  function (user_list,
            follower_depth = 200,
            no_of_sets = 7,
            token = NULL) {
    require(rtweet, quietly = TRUE)
    require(tidyverse, quietly = TRUE)
    require(UpSetR, quietly = TRUE)


    # scrape the user_id of all followers for each handle in the list and bind into 1 dataframe
    followers <- user_list %>%
      map_df(
        ~ get_followers(
          .x,
          n = follower_depth,
          retryonratelimit = TRUE,
          token = token
        ) %>%
          mutate(account = .x)
      )
    # get a de-duplicated list of all followers
    aRdent_followers <- unique(followers$user_id)

    # for each follower, get a binary indicator of whether they follow each tweeter or not and bind to one dataframe
    binaries <- user_list %>%
      map_dfc( ~ ifelse(
        aRdent_followers %in% filter(followers, account == .x)$user_id,
        1,
        0
      ) %>%
        as.data.frame) # UpSetR doesn't like tibbles

    # set column names
    names(binaries) <- user_list

    # plot the sets with UpSetR
    upset(
      binaries,
      nsets = no_of_sets,
      main.bar.color = "SteelBlue",
      sets.bar.color = "DarkCyan",
      sets.x.label = "Follower Count",
      text.scale = c(rep(1.4, 5), 1),
      order.by = "freq"
    )

  }
