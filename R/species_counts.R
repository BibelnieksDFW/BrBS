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
#' @param bbs_dat BBS dataframe with data for 'bird' Species_Code. May have more as well, intended to be passed through lapply in plot_counts.
#'
#' @return ggplot of counts over time for bird.
#'
plot_bird_count <- function(bird, bbs_dat) {
  # Get the specific bird
  bdat <- bbs_dat %>% filter(.data$Species_Code == bird)
  # Make basic plot
  plt <- bdat %>%
         ggplot(aes(x = .data$YrQtr, y = .data$Count)) +
         geom_point() +
         geom_line() +
         labs(title = bird)
  # If more than 10 counts available, add a loess trend line
  if(nrow(bdat) > 10) {
    plt <- plt + geom_smooth(color = "red", method = "loess", formula = y ~ x)
  }

  plt
}

#' Create plots for multiple bird species. If no species specified, plot for all available.
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
  lapply(birds, plot_bird_count, bbs_dat = bbs_dat)
}

#' Convenient wrapper for filtering stations in BBS data.
#'
#' @param bbs_dat Dataframe with BBS data.
#' @param active_stations Character or numeric vector with stations to retain.
#'
#' @return Dataframe filtered to only include active stations.
#' @export
#'
filter_stations <- function(bbs_dat, active_stations) {
  bbs_dat %>% filter(.data$Station %in% active_stations)
}

#' Title Convenient wrapper for filtering bird species in BBS data.
#'
#' @param bbs_dat Dataframe with BBS data.
#' @param active_birds Character vector of species codes to retain.
#'
#' @return Dataframe filtered to only include active birds.
#' @export
#'
filter_birds <- function(bbs_dat, active_birds) {
  bbs_dat %>% filter(.data$Species_Code %in% active_birds)
}

