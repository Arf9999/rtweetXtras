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
    require("quanteda.textstats", quietly = TRUE)
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
      quanteda.textstats::textstat_frequency(dfmatrix) ##create df of featured screen_names and frequency

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
