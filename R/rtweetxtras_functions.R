





#'@title Hashtag wordcloud for rtweet package
#'
#'@description This function creates a wordcloud (defaults to 200 terms) from terms in the "hashtags" column of an rtweet tibble of tweets
#'@param rtweet_timeline_df A dataframe/tibble of tweets created by rtweet package through twitter API call.
#'@param num_words Maximum number of highest frequency terms to use in wordcloud, defaults to 200.
#'@keywords twitter, rtweet, visualization, wordcloud
#'@export
#'@examples
#'df <- rtweet::get_timelines("jack", n=3200)
#'hashtagcloud(df, num_words = 100)


hashtagcloud <- function (rtweet_timeline_df, num_words = 200) {
  require(rtweet, quietly = TRUE)
  require(dplyr, quietly = TRUE)
  require(quanteda, quietly = TRUE)
  require(RcppParallel, quietly = TRUE)

  require(RColorBrewer, quietly = TRUE)
  require(wordcloud, quietly = TRUE)

  hashtagclean <-
    dplyr::filter(rtweet_timeline_df,!is.na(rtweet_timeline_df$hashtags))
  hashtagclean <- rtweet::flatten(hashtagclean)
  text <- paste(unlist(hashtagclean$hashtags), sep = "")
  myCorpus <- quanteda::corpus(text)
  dfmatrix <- quanteda::dfm(
    myCorpus,
    tolower = TRUE,
    remove = c(stopwords(), ",", ".", "-", "\"", "'", "(", ")", ";", ":")
  )
  textFreq <- quanteda::textstat_frequency(dfmatrix)

  set.seed(123456)
  wordcloud(
    words = textFreq$feature,
    freq = textFreq$frequency,
    min.freq = 1,
    max.words = num_words,
    random.order = FALSE,
    rot.per = 0.35,
    colors = brewer.pal(8, "Paired")
  )
}


#'@title Twitter Mentions Barplot for rtweet Package
#'
#'@description This function creates a barplot of mentioned screen_names (default top 20) in an rtweet tibble of tweets
#'@param rtweet_df A dataframe/tibble of tweets created by rtweet package through twitter API call.
#'@param no_of_bars The number of bars in the plot. Default is 20.
#'@param title The title of the barplot in quotation marks. Default of NULL returns the name of the rtweet tibble and the number of bars.
#'@keywords twitter, rtweet, visualization, mentions
#'@export
#'@examples
#'df <- rtweet::get_timelines("jack", n=3200)
#'bar_plot_mentions(df, no_of_bars = 25, title = "Jack's Mentions")


#function to create a bar plot of mentions
bar_plot_mentions <-
  function (rtweet_df,
            no_of_bars = 20,
            title = NULL) {
    require("dplyr", quietly = TRUE)
    require("ggplot2", quietly = TRUE)
    require("quanteda", quietly = TRUE)
    require("RcppParallel", quietly = TRUE)
    require("rtweet", quietly = TRUE)
    require("viridisLite", quietly = TRUE)


    if (is.null(title)) {
      title <-
        paste(
          "Mentions frequency in ",
          deparse(substitute(rtweet_df)),
          ". Top ",
          no_of_bars,
          " accounts mentioned.",
          sep = ""
        )
    }

    mention_clean <- rtweet_df %>%
      dplyr::filter(!is.na(rtweet_df$mentions_screen_name)) %>% ##remove NAs
      rtweet::flatten() ##unlist the mentions

    text <-
      paste(unlist(mention_clean$mentions_screen_name), sep = "") ##create text vector for mining

    myCorpus <- quanteda::corpus(text) ## create corpus
    dfmatrix <- quanteda::dfm(
      myCorpus,
      tolower = TRUE,
      remove = c(stopwords(), ",", ".", "-", "\"", "'", "(", ")", ";", ":")
    )## covert to a document-feature matrix

    textFreq <-
      quanteda::textstat_frequency(dfmatrix) ##create df of featured screen_names and frequency

    bars_to_plot <-
      textFreq[1:no_of_bars] ## select number of bars in chart

    ##plot chart
    ggplot(bars_to_plot, aes(
      y = reorder(feature, frequency),
      x = frequency,
      fill = feature
    )) +
      geom_bar(stat = "identity", color = "grey") +
      ggtitle(title) +
      theme_classic() +
      scale_fill_viridis_d(option = "plasma", alpha = .8)+
      theme(legend.position = "none") +
      ylab("mentioned screen_names") +
      xlab ("frequency")+
      theme(
        axis.text.x = element_text(hjust = 1,
          face = "plain",
          colour = "black",
          size = 10
        ),
        axis.text.y = element_text(
          face = "plain",
          colour = "black",
          size = 10,
          angle = 0
        )
      )

  }

#'@title Twitter profile wordcloud for rtweet package
#'
#'@description This function creates a wordcloud (defaults to 200 terms) from terms in the "description" column of an rtweet tibble of tweets
#'@param rtweet_timeline_df A dataframe/tibble of tweets or users details created by rtweet package through twitter API call.
#'@param num_words Maximum number of highest frequency terms to use in wordcloud, defaults to 200.
#'@keywords twitter, rtweet, visualization, wordcloud
#'@export
#'@examples
#'df <- rtweet::search_tweets("#rstats", n=3200)
#'profilecloud(df, num_words = 100)


profilecloud <- function (rtweet_timeline_df, num_words = 200) {
  require(rtweet, quietly = TRUE)
  require(dplyr, quietly = TRUE)
  require(quanteda, quietly = TRUE)
  require(RcppParallel, quietly = TRUE)

  require(RColorBrewer, quietly = TRUE)
  require(wordcloud, quietly = TRUE)



  profileclean <-
    dplyr::filter(rtweet_timeline_df,
                  !is.na(rtweet_timeline_df$description)) ## remove empty descriptions
  profileclean <-
    profileclean[!duplicated(profileclean["screen_name"]), ] ##remove duplicate users by screen_name

  profileclean <- rtweet::flatten(profileclean)
  text <- paste(unlist(profileclean$description), sep = "")
  myCorpus <- quanteda::corpus(text)
  dfmatrix <- quanteda::dfm(
    myCorpus,
    tolower = TRUE,
    remove = c(
      stopwords(),
      ",",
      ".",
      "-",
      "\"",
      "'",
      "(",
      ")",
      ";",
      ":",
      "https://",
      "|",
      "t.co",
      "&",
      "/",
      "https",
      "!",
      "@"
    )
  )
  textFreq <- quanteda::textstat_frequency(dfmatrix)

  set.seed(123456)
  wordcloud(
    words = textFreq$feature,
    freq = textFreq$frequency,
    min.freq = 2,
    max.words = num_words,
    random.order = FALSE,
    rot.per = 0.35,
    colors = brewer.pal(8, "Dark2")
  )
}

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

#'@title Twitter matrix of follower connection
#'Code modded from boB Rudis' 21 Recipes for Mining Twitter with Rtweet
#'https://rud.is/books/21-recipes/visualizing-intersecting-follower-sets-with-upsetr.html
#'@description This function creates a matrix of followers of a list of twitter users,
#' sums the number of common followers, and then ranks them in descending order.
#'@param user_list A list of user_names
#'@param follower_depth The number of most recent followers to include. Defaults to 200
#'@param token A twitter oauth token. Default is NULL and will use token in Environment
#'@keywords twitter, rtweet, visualization, follower analysis
#'@export
#'@examples
#'users <- c("POTUS", "jack", "elonmusk")
#'common_follower_matrix(user_list = users, follower_depth = 300, token = NULL)



common_follower_matrix <-
  function (user_list,
            follower_depth = 200,
            token = NULL) {
    require(rtweet, quietly = TRUE)
    require(tidyverse, quietly = TRUE)

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

    #add screen_names as rows
    binaries$user_id <- aRdent_followers #add column of user_id

    binaries2 <-
      lookup_users(binaries$user_id, token = token) # get user details
    binaries <-
      full_join(binaries2[, c("screen_name", "user_id")], binaries, by = "user_id") #join matrix to user detail

    #rank by common followers - descending.
    binaries2 <- binaries %>%
      mutate(sum_intersections = rowSums(binaries[, c(3:ncol(binaries))])) %>%
      mutate(ranking = (dense_rank(desc(
        sum_intersections
      )))) %>%
      arrange(ranking)

    return(binaries2)
  }

#'@title Account Activity Plot for a Twitter Account
#'@description This function creates a bubble plot of account activity by hour of a single twitter screen_name
#'     (inspired by python script by twitter user "@Conspirator0")
#'@param account_name A twitter screen_name, in quotes.
#'@param depth The maximum depth of tweets to be visualised. Starts from most recent tweet.
#'Twitter API maximum and default is 3200. Only those tweets occuring in the no_of_weeks param will be shown
#'@param time_zone The timezone of the account.
#'    Requires timezone in format of TZ database (https://en.wikipedia.org/wiki/List_of_tz_database_time_zones) in quotes.
#'    Default is "Africa/Johannesburg"
#'@param no_of_weeks The number of weeks to display. Default is 4. Plot will automatically scale to exclude any period without activity.
#'@param token A twitter oauth token. Default is NULL,
#'    and will utilise an existing token loaded into environment, but can be over-ridden to use a particular token.
#'@keywords twitter, rtweet, visualization, activity, bubble plot.
#'@export
#'@examples account_activity("jack",
#'    depth = 3200,
#'    time_zone = "America/Los_Angeles",
#'    no_of_weeks = 4,token = NULL)


account_activity <- function(account_name,
                             depth = 3200,
                             time_zone = "Africa/Johannesburg",
                             no_of_weeks = 4,
                             token = NULL) {
  require(rtweet, quietly = TRUE)
  require(tidyverse, quietly = TRUE)
  require(lubridate, quietly = TRUE)

  rtweet::get_timeline(
    account_name,
    n = depth,
    retryonratelimit = TRUE,
    token = token
  )[, 1:6] %>%
    mutate(created_at2 = with_tz(created_at, tzone = time_zone)) %>%
    mutate(tweet_date = lubridate::date(created_at2)) %>%
    mutate(tweet_hour = lubridate::hour(created_at2)) %>%
    group_by(screen_name) %>%
    group_by(source, add = TRUE) %>%
    group_by(tweet_date, add = TRUE) %>%
    group_by(tweet_hour, add = TRUE) %>%
    mutate(hourly_tweet_count = n()) %>%
    ungroup %>%
    mutate(tweet_period = (as.duration(interval(
      max(created_at2), created_at2
    )))) %>%
    filter(tweet_period > as.duration(-604800 * no_of_weeks)) %>%
    group_by(source, tweet_date, tweet_hour) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(bubble_scale = max(hourly_tweet_count) / 5) %>%
    ggplot(
      aes(
        x = tweet_hour,
        y = tweet_date,
        size = hourly_tweet_count,
        colour = source,
        alpha = 0.3
      )
    ) +
    geom_point() +
    scale_size_continuous(name = "Hourly tweet volume") +
    scale_x_discrete(
      limits = c(0:23),
      breaks = c(0:23),
      labels = c(0:23)
    ) +
    expand_limits(x = c(0:23)) +
    theme_minimal() +
    labs(
      title = paste(
        "Account activity: ",
        account_name,
        " (as at ",
        as_datetime(Sys.Date()),
        ")\n",
        " Time zone of tweets: ",
        time_zone,
        sep = ""
      ),
      x = "Hour of day",
      y = "Date"
    ) +
    guides(alpha = FALSE)
}

#'@title Create an Igraph Network of Tweets
#'@description This function creates an igraph network graph from a tibble of tweets details created by rtweet functions
#'    (e.g. search_tweets, get_timeline, parse_stream, lookup_statuses, lookup_tweets etc.)
#'@param tweetdf An rtweet tibble of tweets. (90 columns? - depending on version).
#'@param all_mentions Whether to include all the mentions (TRUE/FALSE). Defaults to TRUE,
#'    if set to FALSE will include only Replies, Retweets and Quotes (any additional tagged screen_names will be ignored)
#'@param from_threshold A filter to simplify graphs, removes edges where the "from" node has less than a set level of connections. Default is 0.
#'@param directed_graph Directed graph? (TRUE/FALSE), default is FALSE (i.e. undirected graph output)
#'@keywords twitter, rtweet, visualization, network, igraph
#'@export
#'@examples
#'tweets <- search_tweets("#rstats", n= 100)
#'network <- rtweet_net(tweets, all_mentions = TRUE, from_threshold =2)
#'plot(network)

rtweet_net <- function(tweetdf,
                       all_mentions = TRUE,
                       from_threshold = 0,
                       directed_graph = FALSE) {
  #dependencies
  require(igraph, quietly = TRUE)
  require(tidyverse, quietly = TRUE)


  ##get edges
  if (all_mentions == FALSE) {
    dplyr::filter(tweetdf, !is.na(reply_to_user_id)) %>%
      dplyr::mutate(
        from = screen_name,
        to = reply_to_screen_name,
        type = "reply",
        ID = status_id
      ) -> reply_edges

    dplyr::filter(tweetdf, is_retweet == TRUE) %>%
      dplyr::mutate(
        from = screen_name,
        to = retweet_screen_name,
        type = "retweet",
        ID = status_id
      ) -> retweet_edges

    filter(tweetdf, is_quote == TRUE)  %>%
      dplyr::mutate(
        from = screen_name,
        to = quoted_screen_name,
        type = "quote",
        ID = status_id
      ) -> quote_edges


    edges <-
      (rbind (reply_edges, retweet_edges, quote_edges))[, 89:92]
  } else{
    tidyr::unnest_legacy(tweetdf[, c("screen_name", "mentions_screen_name", "status_id")], .drop =
                           NA)[, c(1, 3, 2)] %>%
      dplyr::mutate(type = "mention") %>%
      dplyr::rename(from = screen_name, to = mentions_screen_name, ID = status_id) %>%
      dplyr::filter(!is.na(to)) -> edges

    edges <- edges[, c(1, 2, 4, 3)]
  }
  ##reduce no of edges and nodes by defined threshold

  simplified_edges <- edges %>%
    dplyr::group_by(from) %>%
    dplyr::mutate(from_count = n()) %>%
    dplyr::filter(from_count > from_threshold)

  ##nodes

  nodes <-
    unique (rbind(
      tibble::enframe(simplified_edges$from, name = "No", value = "node_id"),
      tibble::enframe(simplified_edges$to, name = "No", value = "node_id")
    )[, 2])

  ##return igraph object

  igraph::graph_from_data_frame(d = simplified_edges, v = nodes, directed = directed_graph)
}

#'@title Save an Edgelist csv from an Igraph Object
#'@description This function saves an igraph edgelist as a csv for export to network mapping software such as Gephi.
#'@param igraphobject An igraph network object
#'@param path Path and file name, in quotes (Filename should ideally have ".csv" extension)
#'@keywords twitter, rtweet, visualization, network, igraph, export
#'@export
#'@examples
#'tweets <- rtweet::search_tweets("#rstats", n= 100)
#'network <- rtweetXtras::rtweet_net(tweets, all_mentions = TRUE, from_threshold =2)
#'save_csv_edgelist(igraphobject = network, path = "~/edgelist_network.csv")


save_csv_edgelist <- function(igraphobject, path) {
  ##dependencies
  require(readr, quietly = TRUE)
  require(dplyr, quietly = TRUE)
  require(magrittr, quietly = TRUE)
  require(igraph, quietly = TRUE)

  ##write edgelist...
  edgelist <- as.data.frame(igraph::get.edgelist(igraphobject)) %>%
    rename("Source" = "V1", "Target" = "V2")


  readr::write_csv(as.data.frame(edgelist), path)
  print(paste("Edgelist saved as: ", path))
}


#################################################################################
#'@title Wrapper for rtweet::get_followers and rtweet::lookup_users
#'@description This function consolidates the process of getting followers and looking up the user details.
#'@param account_for_foll Required. The target twitter account user_id or screen_name - in quotes.
#'NB:Only tested with a single account: iterating tokens may not work effectively with multiple accounts,
#'however, using lapply or purrr::map to iterate over user accounts is feasible.
#'@param token_list Optional. A list of valid tokens loaded into environment. Not specifying will use the default token.
#'If multiple tokens are specified, function will iterate through them to reduce ratelimit pauses for followings larger than 75000
#'NB: this may be contrary to Twitter's API terms and conditions - so use at own risk.
#'@param file_path Optional. If specified, an rds file will be saved to the file path in the form <file_path><account_for_foll>_followers.rds
#'#'@keywords twitter, rtweet
#'@export
#'@examples
#'jacks_followers <- get_followers_fast("jack",token_list = c(token1,token2), file_path = "~/")

get_followers_fast <-
  function(account_for_foll,
           token_list = c(NULL),
           file_path = NULL) {
    require(rtweet, quietly = TRUE)
    require(dplyr, quietly = TRUE)
    require(purrr, quietly = TRUE)

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
    print(paste("page = ", page))
    follower_df_users <- ifelse(!is.na(follower_df$user_id),
                                bind_rows(
                                  lookup_users(follower_df$user_id, parse = TRUE, token = token_list[tokencount])
                                ),
                                c(NA))


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
        rbind(follower_df_temp_user)


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
    follower_df_users
  }
####################################################################################
#'@title Wrapper for rtweet::get_friends and rtweet::lookup_users
#'@description This function consolidates the process of getting friends and looking up the user details.
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
#'jacks_friends <- get_friends_fast("jack",token_list = c(token1,token2), file_path = "~/")

get_friends_fast <-
  function(account_for_friend,
           token_list = c(NULL),
           file_path = "~/") {
    require(rtweet, quietly = TRUE)
    require(dplyr, quietly = TRUE)
    require(purrr, quietly = TRUE)

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
    friend_df_users <- ifelse(!is.na(friend_df$user_id),
                              bind_rows(
                                lookup_users(friend_df$user_id, parse = TRUE, token = token_list[tokencount])
                              ),
                              c(NA))

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
        lookup_users(follower_df_temp$user_id, token = token_list[tokencount])

      friend_df <- rbind(follower_df, friend_df_temp)
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
    friend_df_users
  }


####################################################################################
#'@title Number followers and indicate earliest following date
#'@description This function consolidates the process of numbering followers and indicating earliest following date.
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


####################################################################################
#'@title Create gexf file of mentions network for export to Gephi from rtweet dataframe
#'@description This function creates a gexf network file of a mentions network for use with Gephi software
#'requires an rtweet dataframe as returned by get_timeline() or search_tweets()
#'Requires {rgexf} which may not be available on CRAN.
#'Use devtools::install_github("gvegayon/rgexf") to install
#'@param tweet_df Required. An rtweet dataframe of tweets
#'@param filepath Required. A full filepath with file name. Extension of file should be .gexf
#'@param include_edge_att Include edge att (status ID and Date_time). Default is FALSE to minimise Gephi processing of large networks.
#'@param edge_type "directed", "undirected" or "mutual". default is "directed"
#'#'@keywords twitter, rtweet, Gephi, gexf, sna
#'@export
#'@examples
#'rstats <- rtweet::search_tweets("#rstats", n=100, token = NULL)
#'create_gexf(rstats, "~/rstats.gexf", include_edge_att = FALSE, edge_type = "undirected")
#######################################################################################


create_gexf <-
  function(tweet_df,
           filepath,
           include_edge_att = FALSE,
           edge_type = "directed") {
    require(rgexf, quietly = TRUE)
    require(dplyr, quietly = TRUE)
    require(tidyr, quietly = TRUE)



    # get nodes
    temp <-
      tidyr::unnest_legacy(tweet_df[, c("screen_name",
                                        "mentions_screen_name",
                                        "user_id",
                                        "mentions_user_id")],
                           .drop = NA)
    temp2 <- temp %>%
      dplyr::mutate(nodelabel = screen_name) %>%
      dplyr::rename(id = screen_name, attribute = user_id) %>%
      select(id,nodelabel,attribute)

    temp3 <- temp %>%
      dplyr::rename(nodelabel = mentions_screen_name, attribute = mentions_user_id) %>%
      dplyr::mutate(id = nodelabel) %>%
      dplyr::filter(!is.na(id)) %>%
      select(id,nodelabel,attribute)




    nodes <-
      dplyr::bind_rows(temp2, temp3) %>%
      unique() #consolidate nodes and remove duplicates

    nodeatt <- as.data.frame(nodes$attribute)
    nodes <- nodes[, c("id", "nodelabel")]
    message(paste(nrow(nodes), "nodes"))

    #get edges
    edges <-
      tidyr::unnest_legacy(tweet_df[, c("screen_name",
                                        "mentions_screen_name",
                                        "status_id",
                                        "created_at",
                                        "text")],
                           .drop = NA) %>%
      dplyr::rename(source = screen_name, target = mentions_screen_name) %>%
      dplyr::rename(timestamp = created_at, tweetid = status_id) %>%
      dplyr::mutate(attribute = paste(tweetid, timestamp)) %>%
      dplyr::filter(!is.na(target))

    edgeatt <- as.data.frame(edges$attribute)
    edges <- edges[, c("source", "target")]
    message(paste(nrow(edges), "edges"))

    message ("Creating gexf file. This might take a while...")
    #write gexf file
    rgexf::write.gexf(
      nodes = nodes,
      edges = edges,
      nodesAtt = nodeatt,
      if (include_edge_att) {
        edgesAtt = edgeatt
      },
      defaultedgetype = edge_type,
      output = filepath
    )
  }
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

#############################################################################
#'@title Shadowban check
#'
#'@description Checks a particular twitter screen_name for temporary search or reply visibility reduction.
#'@param screen_name A twitter screen_name (case insensitive)
#'@param timezone Optional, a timezone for the timestamp of the ban check. Default is "UTC"
#'@keywords twitter, shadowban, search ban, ghost ban
#'@export
#'@examples
#'check <- check_shadowban("jack", "Africa/Cairo")
##############################################################################

check_shadowban <- function(screen_name, timezone = "UTC") {
  require("httr", quietly = TRUE)
  require("jsonlite", quietly = TRUE)
  require("dplyr", quietly = TRUE)
  require("tidyr", quietly = TRUE)
  require("tibble", quietly = TRUE)
  require("magrittr", quietly = TRUE)
  ##get response
  resp <-
    httr::GET(url = paste0("https://shadowban.eu/.api/", screen_name))
  ##convert to text
  text <- httr::content(resp, as = "text")
  ##convert from JSON to df
  text_df <- jsonlite::fromJSON(text)
  ##pivot wider and unlist various columns
  text_profile_df <- tibble::enframe(text_df$profile) %>%
    tidyr::pivot_wider()

  if (length(text_df) > 2) {
    text_profile_df <- tibble::enframe(text_df$profile) %>%
      tidyr::pivot_wider()

    profile_df <- tibble::enframe(text_profile_df[[2]][[1]]) %>%
      tidyr::pivot_wider() %>%
      dplyr::bind_cols(text_profile_df) %>%
      dplyr::rename(tweets_counted = counted) %>%
      dplyr::select(-sensitives) %>%
      dplyr::select(screen_name, exists, has_tweets, protected, everything())

    time_stamp <- tibble::enframe(text_df$timestamp) %>%
      tidyr::pivot_wider() %>%
      dplyr::rename(timestamp = 1)


    test_df <- tibble::enframe(text_df$tests) %>%
      tidyr::pivot_wider()

    ghost <- tibble::enframe(test_df$ghost[[1]]) %>%
      tidyr::pivot_wider() %>%
      dplyr::rename(ghost_ban = ban)

    more_replies <-
      if (!is.null(text_df[["tests"]][["more_replies"]])) {
        tibble::enframe(text_df[["tests"]][["more_replies"]]) %>%
          tidyr::pivot_wider()
      } else{
        tibble::tribble( ~ error,
                         NA)
      }
    more_replies <- if ("error" %in% names(more_replies)) {
      more_replies %>%
        dplyr::mutate(reply_test_tweet = NA) %>%
        dplyr::mutate(reply_test_in_reply_to = reply_test_tweet,
                      reply_ban = reply_test_tweet) %>%
        dplyr::select (-error)
    } else{
      more_replies %>%
        dplyr::rename(
          reply_test_tweet = tweet,
          reply_test_in_reply_to = in_reply_to,
          reply_ban = ban
        )
    }
  } else{
    text_profile_df <- tibble::enframe(text_df$profile) %>%
      tidyr::pivot_wider()

    profile_df <- tibble::enframe(text_profile_df[[2]][[1]]) %>%
      tidyr::pivot_wider() %>%
      dplyr::bind_cols(text_profile_df)

    if ("protected" %in% names(profile_df)) {
      profile_df <- profile_df %>%
        dplyr::select(screen_name, exists, has_tweets, protected) %>%
        dplyr::mutate(
          tweets_counted = NA,
          possibly_sensitive = NA,
          possibly_sensitive_editable = NA
        )
    } else{
      profile_df <- profile_df %>%
        dplyr::mutate(protected = NA) %>%
        dplyr::select(screen_name, exists, has_tweets, protected) %>%
        dplyr::mutate(
          tweets_counted = NA,
          possibly_sensitive = NA,
          possibly_sensitive_editable = NA
        )
    }

    time_stamp <- tibble::enframe(text_df$timestamp) %>%
      tidyr::pivot_wider() %>%
      dplyr::rename(timestamp = 1)

    ghost <- tibble::tribble( ~ ghost_ban,
                              NA)
    more_replies <- tibble::tribble( ~ reply_test_tweet,
                                     ~ reply_test_in_reply_to,
                                     ~ reply_ban,
                                     NA,
                                     NA,
                                     NA)

  }
  dplyr::bind_cols(profile_df, test_df[, c("typeahead", "search")],
                   ghost, more_replies, time_stamp) %>%
    dplyr::mutate(timestamp = as.POSIXct(timestamp,
                                         origin = "1970-01-01",
                                         tz = timezone)) %>%
    dplyr::rename(search_suggestions = typeahead) %>%
    dplyr::select(
      c(
        screen_name,
        exists,
        has_tweets,
        protected,
        search_suggestions,
        ghost_ban,
        reply_ban,
        timestamp,
        tweets_counted,
        possibly_sensitive,
        possibly_sensitive_editable
      )
    ) %>%
    tidyr::unnest(
      cols = c(
        screen_name,
        exists,
        has_tweets,
        protected,
        search_suggestions,
        ghost_ban,
        reply_ban,
        tweets_counted,
        possibly_sensitive,
        possibly_sensitive_editable
      )
    )
}
#############################################################################
#'@title User list shadowban test - wrapper for rtweetXtras::check_shadowban()
#'
#'@description Checks a list of screen_names for temporary search or reply visibility reduction.
#'@param user_list A list of twitter screen_names (case insensitive)
#'@param timezone Optional, a timezone for the timestamp of the ban check. Default is "UTC"
#'@keywords twitter, shadowban, search ban, ghost ban
#'@export
#'@examples
#'listcheck <- check_shadowban_list(c("jack","jill","bob"), "Africa/Cairo")
##############################################################################
check_shadowban_list <- function(user_list, timezone = "UTC") {
  require("purrr", quietly = TRUE)
  purrr::map_df(user_list, check_shadowban, timezone)
}

#############################################################################
#'@title Run once function to setup call to Python Library for GetOldTweets3
#'
#'@description Ensures that {reticulate} library in stalled and GOT3 library is installed
#'@keywords twitter, getoldtweets3,
#'@export
#'@examples
#'prepare_got()
###############################################################################
prepare_got <- function() {
  require(reticulate)
  library(reticulate)

  reticulate::conda_install(packages = "GetOldTweets3", pip = TRUE)
}

#############################################################################
#'@title Function to rehydrate tweets into rtweet tibbles from GOT3 data format
#'@param got3_df A dataframe as returned by GetOldTweets3
#'@param token An OAuth token loaded in the environment for twitter's Standard API (REQUIRED if a list of tokens is not supplied)
#'@param token_list A list of OAuth tokens loaded in the environment (REQUIRED if token is not specified)
#'@description Uses status_id to request full {rtweet} tibble of tweet data
#'@keywords twitter, getoldtweets3, rtweet
#'@export
###############################################################################
rehydrate_got3_statuses <-
  function(got3_df,
           token = NULL,
           token_list = NULL) {
    require(rtweet, quietly = TRUE)
    require(dplyr, quietly = TRUE)
    require(purrr, quietly = TRUE)
    require(readr, quietly = TRUE)

    ### Check tokens
    if (is.null(token) & is.null(token_list)) {
      stop(
        "Please designate either a token or a list of tokens that are loaded into the environment"
      )

    }
    if (is.null(token_list)) {
      token_list = c(token)
    }
    ###

    df_length <- nrow(got3_df)  ##check length of GOT list of statuses
    last_capture <- 0L ##initial setting for statuses captured
    #check ratelimits for all tokens
    ratelimits <-
      purrr::map_df(token_list, rtweet::rate_limit, query = "lookup_statuses")


    if (as.numeric(max(ratelimits$remaining)) > df_length / 100) {
      ##if no ratelimit reset or token rotation required.
      message(paste0("lookup of ", df_length, " statuses"))

      row_of_max_rl <- which.max(ratelimits$remaining)
      rehydration <- rtweet::lookup_statuses(unlist(got3_df[, "id"]),
                                             token = token_list[[row_of_max_rl]])


    } else{
      while (last_capture < nrow(got3_df)) {
        # iterate through the rows using token rotation & rate reset pausing
        row_of_max_rl <- which.max(ratelimits$remaining)

        ##first iteration
        if (last_capture == 0L) {
          last_capture <-
            as.numeric(ratelimits[row_of_max_rl, "remaining"] * 100)
          message(paste0("rl requests remain : ", as.numeric(ratelimits[row_of_max_rl, "remaining"])))
          message(paste0("base: ", 1L, " upper: ", last_capture))

          rehydration <-
            rtweet::lookup_statuses(unlist(got3_df[c(1:last_capture), "id"]),
                                    token = token_list[[row_of_max_rl]])
        } else{
          #iterate through remainder of status_ids
          base_capture <- last_capture + 1
          last_capture <-
            min(c(
              base_capture + as.numeric(ratelimits[row_of_max_rl, "remaining"] * 100),
              nrow(got3_df)
            ))
          message(paste0("rl requests remain : ", as.numeric(ratelimits[row_of_max_rl, "remaining"])))
          message(paste0("base: ", base_capture, " upper: ", last_capture))

          rehydration <- dplyr::bind_rows(rehydration,
                                          rtweet::lookup_statuses(unlist(got3_df[c(base_capture:last_capture), "id"]),
                                                                  token = token_list[[row_of_max_rl]]))

        }
        ratelimits <-
          purrr::map_df(token_list, rtweet::rate_limit, query = "lookup_statuses")#check ratelimits



        ###manage ratelimit resets as gracefully as possible - conservatively set to 100 queries as test
        if (max(ratelimits$remaining) < 100 &
            last_capture < nrow(got3_df)) {
          message(paste0(
            "Pausing for ratelimit reset: ",
            min(ratelimits$reset),
            " minutes"
          ))
          Sys.sleep(as.numeric(min(ratelimits$reset) * 60))
          ratelimits <-
            purrr::map_df(token_list, rtweet::rate_limit, query = "lookup_statuses")
        }

      }
    }

    ##check for missing tweets and re-lookup
    orig <- dplyr::as_tibble(got3_df[, "id"]) %>%
      rename(status_id = id)
    message(paste0("original: ", nrow(orig)))

    missing <-
      anti_join(orig, as_tibble(rehydration[, "status_id"]), by = "status_id")

    message(paste0("missing: ", nrow(missing)))

    if (nrow(missing) > 0) {
      ##try again to look up missing statuses
      ratelimits <-
        purrr::map_df(token_list, rtweet::rate_limit, query = "lookup_statuses")
      row_of_max_rl <- which.max(ratelimits$remaining)
      df_length <- nrow(missing)
      message(paste0("Attempting to populate missing tweets: ", df_length))
      rehydration <- bind_rows(rehydration,
                               rtweet::lookup_statuses(unlist(missing[c(1:df_length), "status_id"]),
                                                       token = token_list[[row_of_max_rl]]))

      ##write log file of missing tweets.
      missing <-
        anti_join(orig, as_tibble(rehydration[, "status_id"]), by = "status_id") %>%
        rename(id = status_id) %>%
        left_join(got3_df, by = "id") %>%
        mutate(error = "Status not downloaded")

      message(
        paste0(
          nrow(missing),
          " tweets not downloaded, see log file for details: ",
          "got3_rehydration_missing_log_",
          as.numeric(Sys.time()),
          ".csv"
        )
      )

      readr::write_csv(missing,
                       paste0(
                         "got3_rehydration_missing_log_",
                         as.numeric(Sys.time()),
                         ".csv"
                       ))


    }
    ##Get maximum number of retweets
    retweets <- filter(rehydration, retweet_count > 1) %>%
      select(status_id)

    message(paste ("tweets with retweets:", nrow(retweets)))

    ratelimits <-
      purrr::map_df(token_list, rtweet::rate_limit, query = "get_retweets")
    row_of_max_rl <- which.max(ratelimits$remaining)

    retweet_temp <-
      purrr::map_df(retweets$status_id,
                    rtweet::get_retweets,
                    n = 100,
                    token = token_list[[row_of_max_rl]])
    message(paste0("temp rows:", nrow(retweet_temp)))

    message("Captured ",
            nrow(retweet_temp),
            " retweets (a maximum of 100 per original tweet)")



    rehydration <- bind_rows(rehydration, retweet_temp)

    return(rehydration)

  }

#############################################################################
#'@title Wrapper to undertake historical searches calling Python GetOldTweets3 library
#'@param search_string A search string (in quotes)
#'@param since_date Start date for the search
#'@param until_date Latest date for the search (NB: search works backward)
#'@param n Maximum number of results
#'@param output_file_name temporary name of GOT3 csv file - date will be appended.
#'@param token An OAuth token loaded in the environment for twitter's Standard API (REQUIRED if a list of tokens is not supplied)
#'@param token_list A list of OAuth tokens loaded in the environment (REQUIRED if token is not specified)
#'@description Uses status_id to request full {rtweet} tibble of tweet data
#'@keywords twitter, getoldtweets3, rtweet
#'@export
#'@examples
#'test <- got_search("Trump", since_date = "2016-09-06", until_date = "2016-11-06", n = 1000, output_file_name = "test_", token = my_token)
###############################################################################

got_search <-
  function(search_string,
           since_date,
           until_date,
           n = 100,
           output_file_name,
           token = NULL,
           token_list = NULL) {
    require(rtweet, quietly = TRUE)
    require(readr, quietly = TRUE)
    require(dplyr, quietly = TRUE)

    output_path <- paste0(output_file_name, Sys.Date(), ".csv")
    system(
      paste0(
        "GetOldTweets3 --querysearch \"",
        search_string,
        "\" --since ",
        since_date,
        " --until ",
        until_date,
        " --maxtweets ",
        n,
        " --output \"",
        output_path,
        "\""
      )
    )

    ##convert to tibble
    got_file <- readr::read_csv(output_path,
                                col_types = cols(id = col_character())) %>%
      dplyr::mutate(date = as.Date(date)) %>%
      dplyr::filter((date <= until_date) & (date >= since_date)) %>%
      dplyr::distinct(id, .keep_all = TRUE)

    message(paste(nrow(got_file), "tweets after ads and duplications removed"))

    output <-
      rtweetXtras::rehydrate_got3_statuses(got_file, token = token, token_list = token_list)

    return(output)


  }

#############################################################################
#'@title Wrapper to undertake historical searches calling Python snscrape library.
#'@param search_string A search string (in quotes)
#'@param since_date Start date for the search (iso format date in quotes)
#'@param until_date Latest date for the search (iso formatdate in quotes NB: search works backward)
#'@param n Maximum number of results - twitter API has 90k rate-limit per 15 minutes
#'@param file temporary file name for snscrape URL list, timestamp will be appended.
#'@param token An OAuth token loaded in the environment for twitter's Standard API (if not specified, default token will be used)
#'@param token_list A list of OAuth tokens loaded in the environment (REQUIRED if token is not specified)
#'@param delete_tempfile Clear temp file of statuses default = TRUE
#'@description Calls Python script to pull staus URLs of a search, rtweet to rehdrate those statuses.
#'            See https://github.com/JustAnotherArchivist/snscrape
#'@keywords twitter, snscrape, rtweet
#'@export
#'@examples
#'test <- snscrape_search("Trump", since_date = "2016-09-06", until_date = "2016-11-06", n = 1000, file = "test_", token = my_token)
###############################################################################

function (search_string, #search terms in quotes
          since_date = NULL, #optional iso date in quotes
          until_date = NULL, #optional iso date in quotes
          n = 100, #max number of statuses to retrieve. Fewer than 90K recommend due to rate-limiting
          file = "temp", #temporary name for snscrape text file
          token = NULL, #specify token if required
          delete_tempfile = TRUE) #delete text file od statuses
{
  require(rtweet, quietly = TRUE)
  require(readr, quietly = TRUE)
  require(dplyr, quietly = TRUE)

  if(!is.null(since_date)) {
    search_string <- paste0(search_string," since:",since_date)
  }

  if(!is.null(until_date)){
    search_string <-paste0(search_string, " until:", until_date)
  }

  output_path <- paste0(file, as.numeric(Sys.time()), ".txt")

  ##call Python scraper
  system(paste0("snscrape -n ", n," twitter-search ","\"", search_string,"\"",
                " > ", output_path))

  ##import status_ids from text file
  scrape_statuses <-  read_delim(output_path, "/", escape_double = FALSE,
                                 col_names = FALSE,
                                 col_types = cols(X2 = col_skip(),
                                                  X1 = col_skip(), X3 = col_skip(),
                                                  X5 = col_skip(), X6 = col_character()),
                                 trim_ws = TRUE) %>%
    dplyr::rename(screen_name = X4,
                  status_id = X6) %>%
    dplyr::distinct(status_id, .keep_all = TRUE)

  message(paste(nrow(scrape_statuses), "status URLS captured, rehydrating..."))
  temp_rehydration <- rtweet::lookup_statuses(scrape_statuses$status_id, token = token)

 ##cleanup temp files
   if (delete_tempfile == TRUE & file.exists(output_path)){
    file.remove(output_path)
  }

  return(temp_rehydration)

}

