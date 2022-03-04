#' Get number of stations per survey from BBS data.
#'
#' @param bbs_dat Dataframe with BBS data. Must have YrQtr and Station.
#'
#' @return Dataframe number of stations by YrQtr.
#' @export
get_n_stats <- function(bbs_dat) {
  n_stats <- bbs_dat %>%
              select(.data$YrQtr, .data$Station) %>%
              unique() %>%
              group_by(.data$YrQtr) %>%
              summarize(N_Stations = n())

  return(n_stats)
}

#' Plot number of stations per survey over time
#'
#' @param n_stats Dataframe from get_n_stats.
#'
#' @return ggplot with number of stations surveyed over time.
#' @export
#'
plot_n_stats <- function(n_stats) {
  minyr <- floor(min(n_stats$YrQtr))
  maxyr <- ceiling(max(n_stats$YrQtr))
  plt <- n_stats %>%
          mutate(Qtr = factor((.data$YrQtr-floor(.data$YrQtr))*4+1)) %>%
          ggplot(aes(x = .data$YrQtr, y = .data$N_Stations)) +
          geom_point(aes(shape = .data$Qtr)) +
          geom_line(linetype = "dotted") +
          scale_x_continuous(breaks = seq(minyr, maxyr, 5), minor_breaks = seq(minyr, maxyr, 1)) +
          labs(x = "Year",
               y = "Stations Surveyed")
  return(plt)
}

#' Sum counts across species for station-level total counts.
#'
#' @param bbs_dat Dataframe iwht BBS data
#'
#' @return Dataframe with YrQtr, Station, and Count.
#' @export
station_counts <- function(bbs_dat) {
  stat_counts <- station_level <- bbs_dat %>%
                  filter(!is.na(.data$Count)) %>%
                  group_by(.data$YrQtr, .data$Station, .data$Species_Code) %>%
                  # For replicates, average across for counts (round to even)
                  summarize(Count = round(mean(.data$Count))) %>%
                  group_by(.data$YrQtr, .data$Station) %>%
                  summarize(Count = sum(.data$Count)) %>%
                  ungroup()
  return(stat_counts)
}

#' Create plot for individual station.
#'
#' @param station Character vector for Station found in bdat.
#' @param bbs_dat BBS dataframe with data for 'station' Station. May have more as well, intended to be passed through lapply in plot_counts.
#'
#' @return ggplot of counts over time for bird.
#'
plot_station_count <- function(station, bbs_dat) {
  # Get the specific bird
  sdat <- bbs_dat %>% filter(.data$Station == station)
  min_yr <- floor(min(sdat$YrQtr))
  max_yr <- ceiling(max(sdat$YrQtr))
  # Make basic plot
  plt <- sdat %>%
    ggplot(aes(x = .data$YrQtr, y = .data$Count)) +
    geom_point() +
    labs(title = sprintf("Station %s", station)) +
    scale_x_continuous(limits = c(min_yr, max_yr),
                       breaks = seq(min_yr, max_yr, 5),
                       minor_breaks = seq(min_yr,max_yr,1))
  # Only connect the dots if more than one count
  if(nrow(sdat) > 1) {
    plt <- plt + geom_line()
    # If more than 10 counts available, add a loess trend line
    if(nrow(sdat) > 10) {
      plt <- plt + geom_smooth(color = "red", method = "loess", formula = y ~ x)
    }
  }


  return(plt)
}



#' Create plots for multiple stations. If no station specified, plot for all available.
#'
#' @param bbs_dat Dataframe with BBS data.
#' @param stations Character vector with stations.
#'
#' @return List of ggplots for each bird species in birds.
#' @export
#'
plot_station_counts <- function(bbs_dat, stations = c()) {
  # If bird unspecified, grab all of them
  if(length(stations) == 0) {
    stations = unique(bbs_dat$Station)
  }

  # apply plot function across all species
  plts <- lapply(stations, plot_station_count, bbs_dat = bbs_dat)

  return(plts)
}
