#############################################################################
#'@title Wrapper to undertake historical searches calling Python snscrape library.
#'@param search_string A search string (in quotes)
#'@param since_date Start date for the search (iso format date in quotes)
#'@param until_date Latest date for the search (iso format date in quotes NB: search works backward)
#'@param n Maximum number of results - twitter API has 90k rate-limit per 15 minutes
#'@param file temporary file name for snscrape URL list, timestamp will be appended.
#'@param token An OAuth token loaded in the environment for twitter's Standard API (if not specified, default token will be used)
#'@param delete_tempfile Clear temp file of statuses default = TRUE
#'@description Calls Python script to pull status URLs of a search, rtweet to rehdrate those statuses.
#'            See https://github.com/JustAnotherArchivist/snscrape
#'@keywords twitter, snscrape, rtweet
#'@export
#'@examples
#'test <- snscrape_search("Trump", since_date = "2016-09-06", until_date = "2016-11-06", n = 1000, file = "test_", token = NULL)
###############################################################################

snscrape_search <- function(search_string, #search terms in quotes
                            since_date = NULL, #optional iso date in quotes
                            until_date = NULL, #optional iso date in quotes
                            n = 100, #max number of statuses to retrieve. Fewer than 90K recommend due to rate-limiting
                            file = "temp", #temporary name for snscrape text file
                            token = NULL, #specify token if required
                            delete_tempfile = TRUE) #delete text file of statuses
{
  require(rtweet, quietly = TRUE)
  require(readr, quietly = TRUE)
  require(dplyr, quietly = TRUE)

  if(!is.null(since_date)) {
    search_string <- paste0(search_string," since:",since_date)
  }

  if(!is.null(until_date)){
    search_string <-paste0(search_string, " until:", until_date)
  }

  output_path <- paste0(file, as.numeric(Sys.time()), ".txt")

  ##call Python scraper
  system(paste0("snscrape -n ", n," twitter-search ","\"", search_string,"\"",
                " > ", output_path))

  ##import status_ids from text file
  scrape_statuses <-  read_delim(output_path, "/", escape_double = FALSE,
                                 col_names = FALSE,
                                 col_types = cols(X2 = col_skip(),
                                                  X1 = col_skip(), X3 = col_skip(),
                                                  X5 = col_skip(), X6 = col_character()),
                                 trim_ws = TRUE) %>%
    dplyr::rename(screen_name = X4,
                  status_id = X6) %>%
    dplyr::distinct(status_id, .keep_all = TRUE)

  message(paste(nrow(scrape_statuses), "status URLS captured, rehydrating..."))
  temp_rehydration <- rtweet::lookup_statuses(scrape_statuses$status_id, token = token)

  ##cleanup temp files
  if (delete_tempfile == TRUE & file.exists(output_path)){
    file.remove(output_path)
  }

  return(temp_rehydration)

}

#############################################################################
#'@title Wrapper to pull the timeline of a user
#'@param screen_name AA twitter username/handle (no"@" required) or user_id (in quotes)
#'@param n Maximum number of results - twitter API has 90k rate-limit per 15 minutes
#'@param file temporary file name for snscrape URL list, timestamp will be appended.
#'@param token An OAuth token loaded in the environment for twitter's Standard API (if not specified, default token will be used)
#'@param delete_tempfile Clear temp file of statuses default = TRUE
#'@description Calls Python script to pull status URLs of a user search, rtweet to rehdrate those statuses.
#'            See https://github.com/JustAnotherArchivist/snscrape
#'@keywords twitter, snscrape, rtweet
#'@export
#'@examples
#'test <- snscrape_get_timeline("Jack",  n = 1000, file = "test_")
###############################################################################

snscrape_get_timeline <- function (screen_name,
                                   n = 100,
                                   file = "temp",
                                   token = NULL,
                                   delete_tempfile = TRUE)
{
  require(rtweet, quietly = TRUE)
  require(readr, quietly = TRUE)
  require(dplyr, quietly = TRUE)


  output_path <- paste0(file, as.numeric(Sys.time()), ".txt")

  ##call Python scraper
  system(paste0("snscrape -n ", n," twitter-user ", screen_name,
                " > ", output_path))

  ##import status_ids from text file
  scrape_timeline <-  read_delim(output_path, "/", escape_double = FALSE,
                                 col_names = FALSE,
                                 col_types = cols(X2 = col_skip(),
                                                  X1 = col_skip(), X3 = col_skip(),
                                                  X5 = col_skip(), X6 = col_character()),
                                 trim_ws = TRUE) %>%
    dplyr::rename(screen_name = X4,
                  status_id = X6) %>%
    dplyr::distinct(status_id, .keep_all = TRUE)

  message(paste(nrow(scrape_timeline), "status URLS captured, rehydrating timeline..."))
  timeline_rehydration <- rtweet::lookup_statuses(scrape_timeline$status_id, token = token)

  return(timeline_rehydration)
}
