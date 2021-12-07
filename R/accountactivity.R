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
      limits = factor(c(0:23)),
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
    guides(alpha = "none")
}
