#'@title Create an Igraph Network of Tweets
#'@description This function creates an igraph network graph from a tibble of tweets details created by rtweet functions
#'    (e.g. search_tweets, get_timeline, parse_stream, lookup_statuses, lookup_tweets etc.)
#'@param tweetdf An rtweet tibble of tweets. (90 columns? - depending on version).
#'@param all_mentions Whether to include all the mentions (TRUE/FALSE). Defaults to TRUE,
#'    if set to FALSE will include only Replies, Retweets and Quotes (any additional tagged screen_names will be ignored)
#'@param from_threshold A filter to simplify graphs, removes edges where the "from" node has less than a set level of connections. Default is 0.
#'@param directed_graph Directed graph? (TRUE/FALSE), default is FALSE (i.e. undirected graph output)
#'@keywords twitter, rtweet, visualization, network, igraph
#'@export
#'@examples
#'tweets <- rtweet::search_tweets("#rstats", n= 100)
#'network <- rtweet_net(tweets, all_mentions = TRUE, from_threshold =2)
#'plot(network)

rtweet_net <- function(tweetdf,
                       all_mentions = TRUE,
                       from_threshold = 0,
                       directed_graph = FALSE) {
  #dependencies
  require(igraph, quietly = TRUE)
  require(tidyverse, quietly = TRUE)


  ##get edges
  if (all_mentions == FALSE) {
    dplyr::filter(tweetdf, !is.na(reply_to_user_id)) %>%
      dplyr::mutate(
        from = screen_name,
        to = reply_to_screen_name,
        type = "reply",
        ID = status_id
      ) -> reply_edges

    dplyr::filter(tweetdf, is_retweet == TRUE) %>%
      dplyr::mutate(
        from = screen_name,
        to = retweet_screen_name,
        type = "retweet",
        ID = status_id
      ) -> retweet_edges

    filter(tweetdf, is_quote == TRUE)  %>%
      dplyr::mutate(
        from = screen_name,
        to = quoted_screen_name,
        type = "quote",
        ID = status_id
      ) -> quote_edges


    edges <-
      (rbind (reply_edges, retweet_edges, quote_edges))[, 89:92]
  } else{
    tidyr::unnest_legacy(tweetdf[, c("screen_name", "mentions_screen_name", "status_id")], .drop =
                           NA)[, c(1, 3, 2)] %>%
      dplyr::mutate(type = "mention") %>%
      dplyr::rename(from = screen_name, to = mentions_screen_name, ID = status_id) %>%
      dplyr::filter(!is.na(to)) -> edges

    edges <- edges[, c(1, 2, 4, 3)]
  }
  ##reduce no of edges and nodes by defined threshold

  simplified_edges <- edges %>%
    dplyr::group_by(from) %>%
    dplyr::mutate(from_count = n()) %>%
    dplyr::filter(from_count > from_threshold)

  ##nodes

  nodes <-
    unique (rbind(
      tibble::enframe(simplified_edges$from, name = "No", value = "node_id"),
      tibble::enframe(simplified_edges$to, name = "No", value = "node_id")
    )[, 2])

  ##return igraph object

  igraph::graph_from_data_frame(d = simplified_edges, v = nodes, directed = directed_graph)
}

#'@title Save an Edgelist csv from an Igraph Object
#'@description This function saves an igraph edgelist as a csv for export to network mapping software such as Gephi.
#'@param igraphobject An igraph network object
#'@param path Path and file name, in quotes (Filename should ideally have ".csv" extension)
#'@keywords twitter, rtweet, visualization, network, igraph, export
#'@export
#'@examples
#'tweets <- rtweet::search_tweets("#rstats", n= 100)
#'network <- rtweetXtras::rtweet_net(tweets, all_mentions = TRUE, from_threshold =2)
#'save_csv_edgelist(igraphobject = network, path = "~/edgelist_network.csv")


save_csv_edgelist <- function(igraphobject, path) {
  ##dependencies
  require(readr, quietly = TRUE)
  require(dplyr, quietly = TRUE)
  require(magrittr, quietly = TRUE)
  require(igraph, quietly = TRUE)

  ##write edgelist...
  edgelist <- as.data.frame(igraph::get.edgelist(igraphobject)) %>%
    rename("Source" = "V1", "Target" = "V2")


  readr::write_csv(as.data.frame(edgelist), path)
  print(paste("Edgelist saved as: ", path))
}
