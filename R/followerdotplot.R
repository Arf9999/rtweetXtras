####################################################################################
#'@title Plot function for datasets created by rtweetXtras::get_followers_fast
#'@description This builds a ggplot2 scatter plot with the creation date of twitter followers mapped to the order in which they followed.
#'Various options of dot colouring are available to examine the follower's attributes.
#'Optionally, a loess smoothed curve and/or a linear model can be overlaid to analyse the change in follower creation dates over time.
#'@param follower_df Required. A dataframe or tibble created with rtweetXtras::get_followers_fast.
#'@param point_colour maps the dot colour to one of:
#'"statuses_count", "lang", "source", "favourites_count", "followers_count", or "earliest_follow". Default is "statuses_count".
#'@param show_legend Display chart legend. Default is TRUE,
#'but where there are many categorical items (e.g. "lang or "source"), recommended to set to FALSE.
#'@param suppress_warnings Suppress ggplot warnings from printed plots. Default is TRUE.
#'Recommended to use default for .rmd knitting.
#'@param include_loess_smooth Include loess smoothed curve with se shading. Default is FALSE
#'@param include_lm Include a linear model line with se shading. Default is FALSE
#'@param print_immediately Force the function to print the plot immediately. Default is TRUE,
#'set to FALSE to assign the ggplot object.
#'@param log_transform Transform the legend scale to a log scale. Default is FALSE.
#'Useful if there is a concentration of small values that makes it
#'difficult to see any graduation. Ignored for categorical and date scales.
#'@param viridis_option Sets the viridis colour palette. Default is "magma"
#'Options are "viridis", "plasma", "magma", "inferno" and "cividis"
#'@keywords twitter, rtweet, rtweetXtras, ggplot
#'@export
#'@examples
#'arfness_followers <- rtweetXtras::get_followers_fast("arfness", file_path = "~/")
#'follower_dot_plot (arfness_followers, point_colour = "followers_count", show_legend = TRUE, log_transform =TRUE)
################################################################################################


follower_dot_plot <- function(follower_df,
                              point_colour = "statuses_count",
                              show_legend = TRUE,
                              suppress_warnings = TRUE,
                              include_loess_smooth = FALSE,
                              include_lm = FALSE,
                              print_immediately = TRUE,
                              log_transform = FALSE,
                              viridis_option = "magma") {
  require(dplyr, quietly = TRUE)
  require(ggplot2, quietly = TRUE)
  require(viridisLite, quietly = TRUE)
  require(lubridate, quietly = TRUE)
  require(scales, quietly = TRUE)

  ##get latest date in dataset
  last_date_ref <- as.numeric(follower_df$created_at) %>%
    order(decreasing = TRUE, na.last = TRUE)

  last_date_ref <- unlist(last_date_ref[1])

  last_date <- follower_df[last_date_ref, "created_at"] %>%
    unlist() %>%
    as.numeric() %>%
    unname() %>%
    lubridate::as_datetime()


  ## get account_name
  account_name <- as.character(unlist(follower_df[1, 1]))

  follower_df <- follower_df %>%
    mutate(earliest_follow = lubridate::as_date(earliest_follow))

  ##define colour

  if (point_colour == "source") {
    log_transform <- FALSE
    legend_label <- "Source Application"
    discrete_colours <- TRUE
    sub_title <- "Coloured by tweet source application"

  }
  if (point_colour == "lang") {
    legend_label <- "Language"
    discrete_colours <- TRUE
    log_transform <- FALSE
    sub_title <- "Coloured by tweet language"
  }
  if (point_colour == "statuses_count") {
    legend_label <- paste("Status Count", if (log_transform)
      "(log)")
    discrete_colours <- FALSE
    sub_title <- "Coloured by tweet status count"
  }

  if (point_colour == "favourites_count") {
    legend_label <- paste("Favourites Count", if (log_transform)
      "(log)")
    discrete_colours <- FALSE
    sub_title <- "Coloured by favourite/like count"
  }
  if (point_colour == "followers_count") {
    legend_label <- paste("Followers Count", if (log_transform)
      "(log)")
    discrete_colours <- FALSE
    sub_title <- "Coloured by followers count"
  }
  if (point_colour == "earliest_follow") {
    legend_label <- "Earliest Following Date"
    discrete_colours <- FALSE
    log_transform <- FALSE
    sub_title <- "Coloured by estimated date of following"
  }

  ##build plot
  plot <-
    ggplot(follower_df, aes(x = follower_no, y = account_created_at)) +
    {
      if (log_transform)
        geom_point(alpha = 0.3, aes(colour = log(eval(
          as.name(point_colour)
        ))))
    } +
    {
      if (!log_transform)
        geom_point(alpha = 0.3, aes(colour = eval(as.name(point_colour))))
    } +
    {
      if (!discrete_colours & point_colour != "earliest_follow")
        scale_color_viridis_c(
          begin = 0.1,
          end = 0.9,
          option = viridis_option,
          labels = comma
        )
    } +
    {
      if (discrete_colours)
        scale_color_viridis_d(
          begin = 0.1,
          end = 0.9,
          option = viridis_option,
          if(!(point_colour %in% c("source", "lang")))labels = comma
        )
    } +
    {
      if (point_colour == "earliest_follow")
        scale_color_viridis_c(
          begin = 0.1,
          end = 0.9,
          option = viridis_option,
          #labels = lubridate::as_date
          trans = "date"
        )
    } +
    labs(
      title = paste0("Followers of ", account_name, " as at: ", last_date),
      colour = legend_label,
      subtitle = sub_title
    ) +
    {
      if (include_lm)
        stat_smooth(method = "lm", color = "green")
    } +
    {
      if (include_loess_smooth)
        geom_smooth(colour = "red")
    } +
    xlab("Follower number") +
    ylab("Follower creation date") +
    theme_minimal() +
    if (!show_legend) {
      theme(legend.position = "blank")
    }


  if (print_immediately) {
    if (suppress_warnings) {
      suppressWarnings(print(plot))
    } else{
      print(plot)
    }
  } else{
    return(plot)
  }
}
