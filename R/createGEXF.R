####################################################################################
#'@title Create gexf file of mentions network for export to Gephi from rtweet dataframe
#'@description This function creates a gexf network file of a mentions network for use with Gephi software
#'requires an rtweet dataframe as returned by get_timeline() or search_tweets()
#'Requires {rgexf} which may not be available on CRAN.
#'Use devtools::install_github("gvegayon/rgexf") to install
#'@param tweet_df Required. An rtweet dataframe of tweets
#'@param filepath Required. A full filepath with file name. Extension of file should be .gexf
#'@param include_edge_att Include edge att (status ID and Date_time). Default is FALSE to minimise Gephi processing of large networks.
#'@param edge_type "directed", "undirected" or "mutual". default is "directed"
#'#'@keywords twitter, rtweet, Gephi, gexf, sna
#'@export
#'@examples
#'rstats <- rtweet::search_tweets("#rstats", n=100, token = NULL)
#'create_gexf(rstats, "~/rstats.gexf", include_edge_att = FALSE, edge_type = "undirected")
#######################################################################################


create_gexf <-
  function(tweet_df,
           filepath,
           include_edge_att = FALSE,
           edge_type = "directed") {
    require(rgexf, quietly = TRUE)
    require(dplyr, quietly = TRUE)
    require(tidyr, quietly = TRUE)



    # get nodes
    temp <-
      tidyr::unnest_legacy(tweet_df[, c("screen_name",
                                        "mentions_screen_name",
                                        "user_id",
                                        "mentions_user_id")],
                           .drop = NA)
    temp2 <- temp %>%
      dplyr::mutate(nodelabel = screen_name) %>%
      dplyr::rename(id = screen_name, attribute = user_id) %>%
      select(id,nodelabel,attribute)

    temp3 <- temp %>%
      dplyr::rename(nodelabel = mentions_screen_name, attribute = mentions_user_id) %>%
      dplyr::mutate(id = nodelabel) %>%
      dplyr::filter(!is.na(id)) %>%
      select(id,nodelabel,attribute)




    nodes <-
      dplyr::bind_rows(temp2, temp3) %>%
      unique() #consolidate nodes and remove duplicates

    nodeatt <- as.data.frame(nodes$attribute)
    nodes <- nodes[, c("id", "nodelabel")]
    message(paste(nrow(nodes), "nodes"))

    #get edges
    edges <-
      tidyr::unnest_legacy(tweet_df[, c("screen_name",
                                        "mentions_screen_name",
                                        "status_id",
                                        "created_at",
                                        "text")],
                           .drop = NA) %>%
      dplyr::rename(source = screen_name, target = mentions_screen_name) %>%
      dplyr::rename(timestamp = created_at, tweetid = status_id) %>%
      dplyr::mutate(attribute = paste(tweetid, timestamp)) %>%
      dplyr::filter(!is.na(target))

    edgeatt <- as.data.frame(edges$attribute)
    edges <- edges[, c("source", "target")]
    message(paste(nrow(edges), "edges"))

    message ("Creating gexf file. This might take a while...")
    #write gexf file
    rgexf::write.gexf(
      nodes = nodes,
      edges = edges,
      nodesAtt = nodeatt,
      if (include_edge_att) {
        edgesAtt = edgeatt
      },
      defaultedgetype = edge_type,
      output = filepath
    )
  }
