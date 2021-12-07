#################################################################################
#'@title Wrapper for rtweet::get_followers and rtweet::lookup_users
#'@description This function consolidates the process of getting followers and looking up the user details,
#'It also adds a column for follower number, the earliest follow date and the account screen_name being followed.
#'@param account_for_foll Required. The target twitter account user_id or screen_name - in quotes.
#'NB:Only tested with a single account: iterating tokens may not work effectively with multiple accounts,
#'however, using lapply or purrr::map to iterate over user accounts is feasible.
#'@param token_list Optional. A list of valid tokens loaded into environment. Not specifying will use the default token.
#'If multiple tokens are specified, function will iterate through them to reduce rate-limit pauses for followings larger than 75000
#'NB: this may be contrary to Twitter's API terms and conditions - so use at own risk.
#'@param file_path Optional. If specified, an rds file will be saved to the file path in the form <file_path><account_for_foll>_followers.rds
#'#'@keywords twitter, rtweet
#'@export
#'@examples
#'jacks_followers <- get_followers_fast("arfness",token_list = c(NULL), file_path = "~/")

get_followers_fast <-
  function(account_for_foll,
           token_list = c(NULL),
           file_path = NULL) {
    require(rtweet, quietly = TRUE)
    require(dplyr, quietly = TRUE)
    require(purrr, quietly = TRUE)
    require(lubridate, quietly = TRUE)

    message("For accounts with a large following, this process can take a (very) long time.\nPlease be patient.")

    followers_count <-
      as.numeric(tryCatch(
        lookup_users(account_for_foll)[1, "followers_count"]
      ),
      error = "NA") # get the number of followers
    print(paste(account_for_foll, "follower count =", followers_count))

    ##choose the optimum token from the list to start with
    if (!is.null(token_list)) {
      check_ratelimit <-
        purrr::map_df(token_list, rtweet::rate_limit, query = "get_followers") %>%
        mutate(index = row_number()) %>%
        filter(remaining == max(remaining))
    } else{
      check_ratelimit <- rtweet::rate_limit(query = "get_followers") %>%
        mutate(index = row_number())
    }
    if (check_ratelimit[1, "remaining"] < 1) {
      message(paste(
        "Pausing to reset rate_limit for ",
        as.numeric(check_ratelimit[1, "reset"]),
        " minutes"
      ))
      Sys.sleep(as.numeric(check_ratelimit[1, "reset"]) * 60)
    }
    tokencount <-
      as.numeric(check_ratelimit[1, "index"]) # initial token position in list


    page <- "-1" #initial page for next_token

    ## Get first 75000 followers
    follower_df <- tryCatch({
      get_followers(
        account_for_foll,
        n = 75000,
        page = page,
        retryonratelimit = TRUE,
        token = token_list[tokencount]
      )

    },
    error = function(cond) {
      return(c(cond))
      page <- 0
    })

    page <- next_cursor(follower_df) ## set cursor for paging
    message(paste("Page reference > 0 means more than a single pass required\n Page reference = ", page))

    follower_df_users <- if (!is.na(follower_df[1,1])){
      rtweet::lookup_users(follower_df$user_id, parse = TRUE, token_list[tokencount])
    }else{
      NA
    }


    if (!is.null(file_path)) {
      saveRDS(follower_df_users,
              paste0(file_path, account_for_foll, "_followers.rds"))## save rds file of followers
    }
    print(paste(
      "followers captured:",
      nrow(follower_df),
      "out of",
      followers_count
    ))

    ##get the balance of accounts by iterating through tokens
    while (page != "0") {
      tokencount <-
        ifelse(tokencount < length(token_list), tokencount + 1, 1) #token number increment
      follower_df_temp <- get_followers(
        account_for_foll,
        n = 75000,
        page = page,
        retryonratelimit = TRUE,
        token = token_list[tokencount]
      )

      follower_df_temp_user <-
        lookup_users(follower_df_temp$user_id, token = token_list[tokencount])

      follower_df <- bind_rows(follower_df, follower_df_temp)


      follower_df_users <- follower_df_users %>%
        dplyr::bind_rows(follower_df_temp_user)


      if (!is.null(file_path)) {
        saveRDS(follower_df_users,
                paste0(file_path, account_for_foll, "_followers.rds"))
      }

      page <- next_cursor(follower_df_temp)
      message(paste(
        "followers captured:",
        nrow(follower_df),
        "out of",
        followers_count,
        "\n"
      ))
    }
    followed_acc_user_creation <- rtweet::lookup_users(account_for_foll, token = token_list[tokencount]) %>%
      select(account_created_at) %>%
      as.numeric() %>%
      unname() %>%
      unlist() %>%
      lubridate::as_datetime()



    follower_df_users <- follower_df_users %>%
      dplyr::mutate(follower_of = account_for_foll) %>%
      dplyr::relocate(follower_of) %>%
      rtweetXtras::number_followers() %>%
      mutate(earliest_follow = case_when (earliest_follow < followed_acc_user_creation ~
                                            followed_acc_user_creation,
                                          TRUE ~ earliest_follow)
      )


    if (!is.null(file_path)) {
      saveRDS(follower_df_users,
              paste0(file_path, account_for_foll, "_followers.rds"))## save rds file of followers
    }

    return(follower_df_users)
  }

####################################################################################
#'@title Wrapper for rtweet::get_friends and rtweet::lookup_users
#'@description This function consolidates the process of getting friends and looking up the user details.
#'Additional columns are created for account friendship reference, friend_no, and  earliest friendship date.
#'@param account_for_friend Required. The target twitter account user_id or screen_name - in quotes.
#'NB:Only tested with a single account: iterating tokens may not work effectively with multiple accounts,
#'however, using lapply or purrr::map to iterate over user accounts is feasible.
#'@param token_list Optional. A list of valid tokens loaded into environment. Not specifying will use the default token.
#'If multiple tokens are specified, function will iterate through them to reduce ratelimit pauses for friends more than 75000
#'NB: this may be contrary to Twitter's API terms and conditions - so use at own risk.
#'@param file_path Optional. If specified, an rds file will be saved to the file path in the form <file_path><account_for_friend>_friends.rds
#'#'@keywords twitter, rtweet
#'@export
#'@examples
#'jacks_friends <- get_friends_fast("arfness",token_list = c(NULL), file_path = "~/")

get_friends_fast <-
  function(account_for_friend,
           token_list = c(NULL),
           file_path = NULL) {
    require(rtweet, quietly = TRUE)
    require(dplyr, quietly = TRUE)
    require(purrr, quietly = TRUE)
    require(lubridate, quietly = TRUE)
    require(stringr, quietly = TRUE)

    friend_count <-
      as.numeric(tryCatch(
        lookup_users(account_for_friend)[1, "friends_count"]
      ),
      error = "NA") # get the number of friends
    print(paste(account_for_friend, "friend count =", friend_count))

    ##choose the optimum token from the list to start with
    if (!is.null(token_list)) {
      check_ratelimit <-
        purrr::map_df(token_list, rtweet::rate_limit, query = "get_friends") %>%
        mutate(index = row_number()) %>%
        filter(remaining == max(remaining))
    } else{
      check_ratelimit <- rtweet::rate_limit(query = "get_friends") %>%
        mutate(index = row_number())
    }

    if (check_ratelimit[1, "remaining"] < 1) {
      message(paste(
        "Pausing to reset rate_limit for ",
        as.numeric(check_ratelimit[1, "reset"]),
        " minutes"
      ))
      Sys.sleep(as.numeric(check_ratelimit[1, "reset"]) * 60)
    }
    tokencount <-
      as.numeric(check_ratelimit[1, "index"]) # initial token position in list
    page <- "-1" #initial page for next_token

    ## Get first 75000 friends
    friend_df <- tryCatch({
      get_friends(
        account_for_friend,
        n = 75000,
        page = page,
        retryonratelimit = TRUE,
        token = token_list[tokencount]
      )

    },
    error = function(cond) {
      return(c(cond))
      page <- 0
    })
    page <- next_cursor(friend_df)
    page <-
      ifelse(page != 0, next_cursor(friend_df), 0)  ## set cursor for paging
    print(paste("page = ", page))
    friend_df_users <- if(!is.na(friend_df[1,2])){
      lookup_users(friend_df$user_id, parse = TRUE, token = token_list[tokencount])
    }else{
      NA
    }

    if (!is.null(file_path)) {
      saveRDS(friend_df_users,
              paste0(file_path, account_for_friend, "_friends.rds"))
    }
    print(paste("friends captured:", nrow(friend_df), "out of", friend_count))

    ##get the balance of accounts by iterating through tokens
    while (page != "0") {
      tokencount <-
        ifelse(tokencount < length(token_list), tokencount + 1, 1) #token number increment
      friend_df_temp <- get_friends(
        account_for_friend,
        n = 75000,
        page = page,
        retryonratelimit = TRUE,
        token = token_list[tokencount]
      )
      friend_df_temp_user <-
        lookup_users(friend_df_temp$user_id, token = token_list[tokencount])

      friend_df <- bind_rows(friend_df, friend_df_temp)
      friend_df_users <-
        bind_rows(friend_df_users, friend_df_temp_user)

      if (!is.null(file_path)) {
        saveRDS(friend_df_users,
                paste0(file_path, account_for_friend, "_friends.rds"))
      }
      page <- next_cursor(friend_df_temp)
      message(paste(
        "friends captured:",
        nrow(friend_df),
        "out of",
        friend_count,
        "\n"
      ))
    }

    friend_acc_user_creation <- rtweet::lookup_users(account_for_friend, token = token_list[tokencount]) %>%
      select(account_created_at) %>%
      as.numeric() %>%
      unname() %>%
      unlist() %>%
      lubridate::as_datetime()


    friend_df_users <- friend_df_users %>%
      mutate(friend_of = account_for_friend) %>%
      rtweetXtras::number_followers() %>%
      rename(friend_no = follower_no, earliest_friendship = earliest_follow) %>%
      mutate(earliest_friendship = case_when (earliest_friendship < friend_acc_user_creation ~
                                                friend_acc_user_creation,
                                              TRUE ~ earliest_friendship)
      ) %>%
      relocate(friend_of)

    if (!is.null(file_path)) {
      saveRDS(friend_df_users,
              paste0(file_path, account_for_friend, "_friends.rds"))
    }

    friend_df_users
  }

