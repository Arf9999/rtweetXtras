#'@title Twitter matrix of friend connection
#'Code modded from boB Rudis' 21 Recipes for Mining Twitter with Rtweet
#'https://rud.is/books/21-recipes/visualizing-intersecting-follower-sets-with-upsetr.html
#'@description This function creates a matrix of friends of a list of twitter users,
#' sums the number of common friends, and then ranks them in descending order.
#'@param user_list A list of user_names
#'@param friend_depth The number of most recent friends to include. Defaults to 200
#'@param token A twitter oauth token. Default is NULL and will use token in Environment
#'@keywords twitter, rtweet, visualization, friend analysis
#'@export
#'@examples
#'users <- c("POTUS", "jack", "elonmusk")
#'common_friend_matrix(user_list = users, friend_depth = 300, token = NULL)



common_friend_matrix <-
  function (user_list,
            friend_depth = 200,
            token = NULL) {
    require(rtweet, quietly = TRUE)
    require(tidyverse, quietly = TRUE)

    # scrape the user_id of all friends for each handle in the list and bind into 1 dataframe
    friends <- user_list %>%
      map_df(
        ~ get_friends(
          .x,
          n = friend_depth,
          retryonratelimit = TRUE,
          token = token
        ) %>%
          mutate(account = .x)
      )
    # get a de-duplicated list of all friends
    aRdent_friends <- unique(friends$user_id)

    # for each friend, get a binary indicator of whether they follow each tweeter or not and bind to one dataframe
    binaries <- user_list %>%
      map_dfc( ~ ifelse(
        aRdent_friends %in% filter(friends, account == .x)$user_id,
        1,
        0
      ) %>%
        as.data.frame) # UpSetR doesn't like tibbles

    # set column names
    names(binaries) <- user_list

    #add screen_names as rows
    binaries$user_id <- aRdent_friends #add column of user_id

    binaries2 <-
      lookup_users(binaries$user_id, token = token) # get user details
    binaries <-
      full_join(binaries2[, c("screen_name", "user_id")], binaries, by = "user_id") #join matrix to user detail

    #rank by common friends - descending.
    binaries2 <- binaries %>%
      mutate(sum_intersections = rowSums(binaries[, c(3:ncol(binaries))])) %>%
      mutate(ranking = (dense_rank(desc(
        sum_intersections
      )))) %>%
      arrange(ranking)

    return(binaries2)
  }

