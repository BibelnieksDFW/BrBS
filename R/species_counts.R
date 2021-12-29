#' Sum counts across stations for island-wide totals
#'
#' @param bbs_dat Dataframe with BBS data.
#'
#' @return Dataframe with YrQtr, Species_Code, and Count.
#' @export
#'
islandwide_counts <- function(bbs_dat) {
  island_wide <- bbs_dat %>%
    filter(!is.na(.data$Count)) %>%
    group_by(.data$YrQtr, .data$Station, .data$Species_Code) %>%
    # For replicates, average across for counts (round to even)
    summarize(Count = round(mean(.data$Count))) %>%
    group_by(.data$YrQtr, .data$Species_Code) %>%
    summarize(Count = sum(.data$Count)) %>%
    ungroup()
}


#' Create plot for individual bird.
#'
#' @param bird String for Species_Code found in bdat.
#' @param bdat BBS dataframe with data for 'bird' Species_Code. May have more as well, intended to be passed through lapply in plot_counts.
#'
#' @return ggplot of counts over time for bird.
#'
plot_bird_count <- function(bird, bdat) {
  plt <- bdat %>%
         filter(.data$Species_Code == bird) %>%
         ggplot(aes(x = .data$YrQtr, y = .data$Count)) +
         geom_point() +
         geom_line() +
         geom_smooth(color = "red", method = "loess", formula = y ~ x) +
         labs(title = bird)
}

#' Create plots
#'
#' @param bbs_dat Dataframe with BBS data.
#' @param birds Character vector with bird species codes.
#'
#' @return List of ggplots for each bird species in birds.
#' @export
#'
plot_counts <- function(bbs_dat, birds = c()) {
  # If bird unspecified, grab all of them
  if(length(birds) == 0) {
    birds = unique(bbs_dat$Species_Code)
  }

  # apply plot function across all species
  lapply(birds, plot_bird_count, bdat = bbs_dat)
}
