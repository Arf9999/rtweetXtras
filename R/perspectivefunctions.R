#'@title Query Google Perspective API with text
#'
#'@description Queries the Google Perespective API and returns a tibble
#'of various probability measures
#'@param text A character string of text. Maximum size 1MB.
#'@param api_key A Perspective API key. Required.
#'@keywords Toxicity, text analysis
#'@export
#'@examples
#'df <- get_perspective("Hello, How are you?", api_key = YourAPIkeyhere)

get_perspective <- function(text, api_key) {

  require(dplyr, quietly = TRUE)
  require(httr, quietly = TRUE)
  require(stringr, quietly = TRUE)
  require(jsonlite, quietly = TRUE)
  require(magrittr, quietly = TRUE)
  require(tibble, quietly = TRUE)

  text <- stringr::str_replace_all(text, "\"", "") #API fails with " included in query, so quotes are removed

  temp <- httr::VERB(verb = "POST", url = "https://commentanalyzer.googleapis.com/v1alpha1/comments:analyze",
                     httr::add_headers(), body = paste0("{comment: {text:\"",text,"\"},languages: [\"en\"], requestedAttributes:{TOXICITY:{}, SEVERE_TOXICITY:{}, IDENTITY_ATTACK:{}, INSULT:{}, PROFANITY:{}, THREAT:{}, SEXUALLY_EXPLICIT:{}, FLIRTATION:{}, INCOHERENT:{}, SPAM:{} }}"),
                     encode = "json", query = list(key = api_key))

  pretty <- jsonlite::fromJSON((prettify(temp)))

  temp2 <- tibble::as_tibble (NULL)
  score_names <- c("TOXICITY", "SEVERE_TOXICITY", "IDENTITY_ATTACK", "INSULT", "PROFANITY", "THREAT", "SEXUALLY_EXPLICIT", "FLIRTATION", "INCOHERENT", "SPAM")
  for (x in seq_along(score_names)){
    temp2[1,x] <- pretty[["attributeScores"]][[score_names[x]]][["summaryScore"]][["value"]]
  }
  names(temp2) <- c("toxicity", "severe_toxicity", "identity_attack", "insult", "profanity", "threat", "sexually_explicit", "flirtation", "incoherent", "spam")

  temp2 <- temp2 %>%
    mutate("text" = as.character(text)) %>%
    relocate(text)


  return(temp2)
}

#'@title Query an rtweet (or rtweetXtras) dataframe of tweets text
#'
#'@description Queries the Google Perspective API and returns a tibble of tweets and measures
#'@param rtweet_df A dataframe of tweets returned by rtweet or rtweetXtras
#'@param api_key A Perspective API key. Required.
#'@param QPS Queries per second allowed by the Perspective API for the api_key (default is 1)
#'@param summary TRUE delivers a condensed response, removing non-essential columns.
#'FALSE delivers original rtweet_df with additional columns for measures
#'@keywords Toxicity, text analysis
#'@export
#'@examples
#'df <- rtweet::search_tweets("summer", n=200)
#'perspective_df <- perspective_rtweet(df, api_key = YourAPIkeyhere, QPS = 1, summary = TRUE)

perspective_rtweet <- function(rtweet_df, api_key, QPS = 1,summary = TRUE){

  require(tidyverse, quietly = TRUE)
  require(utils, quietly = TRUE)

  rtweet_df <- filter(rtweet_df, !is_retweet) #remove non-original tweets

  pb <-txtProgressBar(min=0, max = nrow(temp), char = "=", width = 30)
  message("0%...........50%............100%")
  message("|             |              |")
  getTxtProgressBar(pb)

  for (y in 1:nrow(rtweet_df)){

    temp <- if (y>1){
      get_perspective(rtweet_df[y, "text"], api_key = api_key) %>%
        mutate(status_id = as.character(rtweet_df[y, "status_id"])) %>%
        bind_rows(temp)

    }else{
      get_perspective(rtweet_df[y, "text"], api_key) %>%
        mutate(status_id = as.character(rtweet_df[y, "status_id"]))
    }
    setTxtProgressBar(pb, x)
    Sys.sleep(1/QPS)
  }
  rtweet_df <- left_join(rtweet_df, temp, by = "status_id") %>%
    rename(text = text.x) %>%
    select (-text.y)

  if(summary){
    rtweet_df <- rtweet_df %>%
      select(user_id,screen_name,created_at,text,
             toxicity, severe_toxicity, identity_attack, insult, profanity, threat, sexually_explicit, flirtation, incoherent, spam,
             user_id,status_id, reply_to_status_id,reply_to_screen_name, quoted_status_id, quoted_screen_name)
  }
  print("\n")
  return(rtweet_df)
}
