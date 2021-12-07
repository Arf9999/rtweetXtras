####################################################################################
#'@title Interactive timeline visualisation for rtweet dataset using Apache echarts
#'@description Interactive timeline of tweets indicating time of tweet, author, type (tweet, retweet, quote, reply),
#'and rate of tweets (posts per second) - posts per second is calculated as moving average over 10 tweets.
#'Posts per second are calculated separately for content tweets (tweets, quotes and replies) and copy tweets (retweets)
#'@param df Required. A dataframe or tibble created with {rtweet}
#'@param title Chart title (in quotes)
#'@param print_immediately Force the function to print the plot immediately. Default is TRUE,
#'set to FALSE to assign the object (an HTML widget)
#'@keywords twitter, rtweet, rtweetXtras, echarts, echarts4r, interactive
#'@export
#'@examples
#'df1 <- rtweet::searchtweets("micky mouse", n= 100)
#'scatter_ts_interactive (df1, title = "100 Micky Mouse tweets")
################################################################################################

scatter_ts_interactive <-function(df,
                                  title = "",
                                  print_immediately = TRUE){


  require(dplyr, quietly = TRUE)
  require(lubridate, quietly = TRUE)
  require(echarts4r, quietly = TRUE)

  options(scipen = 8)

  plot <- df %>%
    arrange(status_id) %>%
    dplyr::group_by(is_retweet) %>%
    dplyr::mutate(
      time = lubridate::as_datetime(created_at),
      row_no = row_number() - 1,
      time_temp10 = case_when(
        row_no == 0 ~ time,
        row_no < 11 ~ lag(time, 1),
        row_no >= 11 ~ lag(time, 10)
      ),
      int = case_when(
        row_no > 10 ~ as.numeric(as.period(interval(time_temp10, time))) / 10,
        row_no < 11 ~ as.numeric(as.period(interval(time_temp10, time))) /
          row_no
      ),
      posts_per_second = case_when (is.nan(int)  ~ 0.000001,!is.nan(int) ~ (signif((
        1 / int
      ), digits = 3)))
    ) %>%
    dplyr::relocate(posts_per_second, int, time, time_temp10) %>%
    dplyr::mutate(tweet_type = case_when(
      !is.na(reply_to_status_id) ~ "Reply",
      (is_retweet == TRUE) ~ "Retweet",
      (is_quote == TRUE) ~ "Quote",
      TRUE ~ "Tweet"
    )) %>%
    dplyr::mutate(text2 = stringr::str_c(screen_name, text, sep = ":")) %>%
    dplyr::relocate(tweet_type) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(tweet_type) %>%
    echarts4r::e_chart(time) %>%
    echarts4r::e_y_axis(type = "log", logBase = 10) %>%
    echarts4r::e_scatter(
      posts_per_second,
      bind = text2,
      legend = TRUE,
      symbol_size = 5,
      hoverAnimation = TRUE,
      opacity = 0.3
    ) %>%
    echarts4r::e_axis_labels(y = "Posts per second (log)", x = "Time") %>%
    echarts4r::e_tooltip(extraCssText = "width:200px; white-space:pre-wrap") %>%
    echarts4r::e_title(text = title, x = "center", y = "top") %>%
    echarts4r::e_legend(orient = "vertical", x = "right", y = "center")

  if (print_immediately) {
    print(plot)
  } else{
    return(plot)
  }

}
