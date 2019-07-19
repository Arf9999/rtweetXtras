

install.packages(c("rtweet",
       "tidyverse",
       "quanteda",
       "RcppParallel",
       "RColorBrewer",
       "wordcloud",
       "UpSetR",
       "lubridate",
       "igraph"), repos = "https://cloud.r-project.org/")




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
    dplyr::filter(rtweet_timeline_df, !is.na(rtweet_timeline_df$hashtags))
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
    colors = brewer.pal(8, "Dark2")
  )
}


#'@title Twitter Mentions Barplot for rtweet Package
#'
#'@description This function creates a barplot of mentioned screen_names (default top 20) in an rtweet tibble of tweets
#'@param rtweet_df A dataframe/tibble of tweets created by rtweet package through twitter API call.
#'@param no_of_bars The number of bars in the plot. Default is 20.
#'@param title The title of the barplot in quotation marks. Default of NULL returns the name of the rtweet Tbble and the number of bars.
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
    require("dplyr")
    require("ggplot2")
    require("quanteda")
    require("RcppParallel")
    require("rtweet")


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
      paste(unlist(mention_clean$mentions_screen_name), sep = "") ##create text file for mining

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
      x = reorder(feature, frequency, desc),
      y = frequency,
      fill = feature
    )) +
      geom_bar(stat = "identity") +
      ggtitle(title) +
      theme_classic() +
      theme(legend.position = "none") +
      xlab("mentioned screen_names") +
      theme(
        axis.text.x = element_text(
          face = "plain",
          colour = "black",
          size = 10,
          angle = 90
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
    dplyr::filter(rtweet_timeline_df,!is.na(rtweet_timeline_df$description)) ## remove empty descriptions
  profileclean <-
    profileclean[!duplicated(profileclean["screen_name"]),] ##remove duplicate users by screen_name

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
#'@keywords twitter, rtweet, visualization, follower analysis
#'@export
#'@examples
#'users <- c("POTUS", "jack", "elonmusk")
#'common_follower_analysis(user_list = users, follower_depth = 300, no_of_sets = 7)


common_follower_analysis <-
  function (user_list,
            follower_depth = 200,
            no_of_sets = 7) {
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
      map_dfc(~ ifelse(
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
#'@keywords twitter, rtweet, visualization, follower analysis
#'@export
#'@examples
#'users <- c("POTUS", "jack", "elonmusk")
#'common_follower_matrix(user_list = users, follower_depth = 300)



common_follower_matrix <-
  function (user_list, follower_depth = 200) {
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
      map_dfc(~ ifelse(
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
#'     (inspired by python script by twitter user "@Conspirat0r")
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
#'@param tweetdf An rtweet tibble of tweets. (88 columns).
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
    dplyr::filter(tweetdf,!is.na(reply_to_user_id)) %>%
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


    edges <- (rbind (reply_edges, retweet_edges, quote_edges))[, 89:92]
  } else{
    tidyr::unnest(tweetdf[, c("screen_name", "mentions_screen_name", "status_id")], .drop =
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


save_csv_edgelist <- function(igraphobject, path){
  ##dependencies
  require(readr, quietly= TRUE)
  require(igraph, quietly = TRUE)

  ##write edgelist...
  edgelist <- igraph::get.edgelist(igraphobject)
  readr::write_csv(as.data.frame(edgelist),path)
  print(paste("Edgelist saved as: ",path))
}


#################################################################################
#'@title Wrapper for rtweet::get_followers and rtweet::lookup_users
#'@description This function consolidates the process of getting followers and looking up the user details.
#'@param account_for_foll Required. The target twitter account user_id or screen_name - in quotes.
#'NB:Only tested with a single account: iterating tokens mau not work effectively with multiple accounts
#'@param token_list Optional. A list of valid tokens loaded into environment. Not specifying will use the default token.
#'If multiple tokens are specified, function will iterate through them to reduce ratelimit pauses for followings larger than 75000
#'NB: this may be contrary to Twitter's API terms and conditions - so use at own risk.
#'@param file_path Optional. If specified, an rds file will be saved to the file path in the form <file_path><account_for_foll>_followers.rds
#'#'@keywords twitter, rtweet
#'@export
#'@examples
#'jacks_followers <- get_followers_fast("jack",token_list = c(token1,token2), file_path = "~/")

get_followers_fast <- function(account_for_foll, token_list = c(NULL), file_path = NULL){
  require(rtweet, quietly = TRUE)
  require(dplyr, quietly = TRUE)

  followers_count <-
    as.numeric(lookup_users(account_for_foll)[1, "followers_count"]) # get the number of followers
  tokencount <- 1 # initial token position in list
  page <- "-1" #initial page for next_token

  ## Get first 75000 followers
  follower_df <- get_followers(
    account_for_foll,
    n = 75000,
    page = page,
    retryonratelimit = TRUE,
    token = token_list[tokencount]
  )
  page <- next_cursor(follower_df) ## set cursor for paging
  follower_df_users <- lookup_users(follower_df$user_id,token = token_list[tokencount])
  if(!is.null(file_path)) {
    saveRDS(follower_df_users,paste0(file_path,account_for_foll,"_followers.rds"))
  }
  print(paste("followers captured:", nrow(follower_df), "out of", followers_count))

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
    follower_df_temp_user <- lookup_users(follower_df_temp$user_id,token = token_list[tokencount])

    follower_df <- bind_rows(follower_df, follower_df_temp)
    follower_df_users <- bind_rows(follower_df_users, follower_df_temp_user)
    if(!is.null(file_path)) {
      saveRDS(follower_df_users,paste0(file_path,account_for_foll,"_followers.rds"))
    }

    page <- next_cursor(follower_df_temp)
    message(paste("followers captured:", nrow(follower_df), "out of", followers_count,"\n"))
  }
  follower_df_users
}
####################################################################################
#'@title Wrapper for rtweet::get_friends and rtweet::lookup_users
#'@description This function consolidates the process of getting friends and looking up the user details.
#'@param account_for_friend Required. The target twitter account user_id or screen_name - in quotes.
#'NB:Only tested with a single account: iterating tokens may not work effectively with multiple accounts
#'@param token_list Optional. A list of valid tokens loaded into environment. Not specifying will use the default token.
#'If multiple tokens are specified, function will iterate through them to reduce ratelimit pauses for friends more than 75000
#'NB: this may be contrary to Twitter's API terms and conditions - so use at own risk.
#'@param file_path Optional. If specified, an rds file will be saved to the file path in the form <file_path><account_for_friend>_friends.rds
#'#'@keywords twitter, rtweet
#'@export
#'@examples
#'jacks_friends <- get_friends_fast("jack",token_list = c(token1,token2), file_path = "~/")

get_friends_fast <- function(account_for_friend, token_list = c(NULL), file_path = "~/"){
  require(rtweet, quietly = TRUE)
  require(dplyr, quietly = TRUE)

  friend_count <-
    as.numeric(lookup_users(account_for_friend)[1, "friends_count"]) # get the number of friends
  tokencount <- 1 # initial token position in list
  page <- "-1" #initial page for next_token

  ## Get first 75000 friends
  friend_df <- get_friends(
    account_for_friend,
    n = 75000,
    page = page,
    retryonratelimit = TRUE,
    token = token_list[tokencount]
  )
  page <- next_cursor(friend_df) ## set cursor for paging
  friend_df_users <- lookup_users(friend_df$user_id,token = token_list[tokencount])
  if(!is.null(file_path)) {
    saveRDS(friend_df_users,paste0(file_path,account_for_friend,"_friends.rds"))
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
    friend_df_temp_user <- lookup_users(follower_df_temp$user_id,token = token_list[tokencount])

    friend_df <- bind_rows(follower_df, friend_df_temp)
    friend_df_users <- bind_rows(friend_df_users, friend_df_temp_user)
    if(!is.null(file_path)) {
      saveRDS(friend_df_users,paste0(file_path,account_for_friend,"_friends.rds"))
    }
    page <- next_cursor(friend_df_temp)
    message(paste("friends captured:", nrow(friend_df), "out of", friend_count, "\n"))
  }
  friend_df_users
}

