




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
#'Code cribbed from boB Rudis' 21 Recipes for Mining Twitter with Rtweet
#'https://rud.is/books/21-recipes/visualizing-intersecting-follower-sets-with-upsetr.html
#'@description This function creates an UpSetR graph of common followers
#'@param user_list A list of user_names
#'@param follower_depth The number of most recent followers to graph. Defaults to 200
#'@param no_of_sets <- The number of sets in the UpSetR graph. Defaults to 7.
#'@keywords twitter, rtweet, visualization, follower analysis
#'@export
#'@examples
#'users <- c("POTUS", "jack", "elonmusk")
#'common_follower_analysis(user_list = users, follower_depth = 300, no_of_sets = 7)


common_follower_analysis <-
  function (user_list, follower_depth = 200, no_of_sets = 7) {

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

    binaries2 <- lookup_users(binaries$user_id, token = token) # get user details
    binaries <-
      full_join(binaries2[, c("screen_name", "user_id")], binaries, by = "user_id") #join matrix to user detail

    #rank by common followers - descending.
      binaries2 <- binaries%>%
      mutate(sum_intersections = rowSums(binaries[,c(3:ncol(binaries))]))%>%
      mutate(ranking = (dense_rank(desc(sum_intersections)))) %>%
      arrange(ranking)

    return(binaries2)
  }

#'@title Account activity plot for a twitter account using rtweet package (inspired by python script by twitter user "@Conspirat0r")
#'@description This function creates a bubble plot of account activity by hour of a single twitter screen_name
#'@param account_name A twitter screen_name, in quotes.
#'@param depth The maximum depth of tweets to be visualised. Starts from most recent tweet. Twitter API maximum and default is 3200. Only those tweets occuring in the no_of_weeks param will be shown
#'@param time_zone The timezone of the account. Requires timezone in format of TZ database (https://en.wikipedia.org/wiki/List_of_tz_database_time_zones) in quotes. Default is "Africa/Johannesburg"
#'@param no_of_weeks The number of weeks to display. Default is 4. Plot will automatically scale to exclude any period without activity.
#'@param token A twitter oauth token. Default is NULL, and will utilise an existing token loaded into environment, but can be over-ridden to use a particular token.
#'#'@keywords twitter, rtweet, visualization, activity, bubble plot.
#'@export
#'@examples account_activity("jack",depth = 3200, time_zone = "America/Los_Angeles", no_of_weeks = 4, token = readRDS("~/twitter_token.rds"))


account_activity <- function(account_name,
                             depth = 3200,
                             time_zone = "Africa/Johannesburg",
                             no_of_weeks = 4, token = NULL){
  require(rtweet, quietly = TRUE)
  require(tidyverse, quietly = TRUE)
  require(lubridate, quietly = TRUE)

  rtweet::get_timeline(account_name, n=depth,retryonratelimit=TRUE, token = token)[,1:6]%>%
    mutate(created_at2 = with_tz(created_at, tzone = time_zone)) %>%
    mutate(tweet_date = lubridate::date(created_at2))%>%
    mutate(tweet_hour = lubridate::hour(created_at2))%>%
    group_by(screen_name)%>%
    group_by(source, add = TRUE) %>%
    group_by(tweet_date, add = TRUE )%>%
    group_by(tweet_hour, add = TRUE)%>%
    mutate(hourly_tweet_count = n())%>%
    ungroup %>%
    mutate(tweet_period = (as.duration(interval(max(created_at2), created_at2)))) %>%
    filter(tweet_period > as.duration(-604800 * no_of_weeks)) %>%
    group_by(source, tweet_date, tweet_hour) %>%
    slice(1) %>%
    ungroup()%>%
    mutate(bubble_scale = max(hourly_tweet_count)/5) %>%
    ggplot(aes(x= tweet_hour, y= tweet_date, size = hourly_tweet_count, colour = source, alpha =0.3))+
    geom_point()+
    scale_size_continuous(name="Hourly tweet volume")+
    scale_x_discrete(limits = c(0:23),
                     breaks = c(0:23),
                     labels = c(0:23))+
    expand_limits(x=c(0:23))+
    theme_minimal()+
    labs(title = paste("Account activity: ",account_name, " (as at ", as_datetime(Sys.Date()),")\n", " Time zone of tweets: ", time_zone,sep=""),
         x= "Hour of day",
         y="Date")+
    guides(alpha = FALSE)
}
