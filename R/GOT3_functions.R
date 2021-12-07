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

