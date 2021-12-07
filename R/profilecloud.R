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
  require(quanteda.textstats, quietly = TRUE)
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
  textFreq <- quanteda.textstats::textstat_frequency(dfmatrix)

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
