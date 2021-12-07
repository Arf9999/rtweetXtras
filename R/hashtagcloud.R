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
  require(quanteda.textstats, quietly = TRUE)
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
  textFreq <- quanteda.textstats::textstat_frequency(dfmatrix)

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
