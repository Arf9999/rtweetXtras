



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

    textFreq <- quanteda::textstat_frequency(dfmatrix) ##create df of featured screen_names and frequency

    bars_to_plot <- textFreq[1:no_of_bars] ## select number of bars in chart

    ##plot chart
    ggplot(bars_to_plot, aes(
      x = reorder(feature, frequency, desc),
      y = frequency,
      fill = feature
    )) +
      geom_bar(stat = "identity") +
      ggtitle(title) +
      theme_classic()+
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
