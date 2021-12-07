
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
