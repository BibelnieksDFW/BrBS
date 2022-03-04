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
  return(island_wide)
}


#' Create plot for individual bird.
#'
#' @param bird Character vector for Species_Code found in bdat.
#' @param bbs_dat BBS dataframe with data for 'bird' Species_Code. May have more as well, intended to be passed through lapply in plot_counts.
#'
#' @return ggplot of counts over time for bird.
#'
plot_bird_count <- function(bird, bbs_dat) {
  # Get the specific bird
  bdat <- bbs_dat %>% filter(.data$Species_Code == bird)
  min_yr <- floor(min(bdat$YrQtr))
  max_yr <- ceiling(max(bdat$YrQtr))
  # Make basic plot
  plt <- bdat %>%
         ggplot(aes(x = .data$YrQtr, y = .data$Count)) +
         geom_point() +
         labs(title = bird) +
         scale_x_continuous(limits = c(min_yr, max_yr),
                            breaks = seq(min_yr, max_yr, 5),
                            minor_breaks = seq(min_yr,max_yr,1))
  # Only connect the dots if more than one count
  if(nrow(bdat) > 1) {
    plt <- plt + geom_line()
    # If more than 10 counts available, add a loess trend line
    if(nrow(bdat) > 10) {
      plt <- plt + geom_smooth(color = "red", method = "loess", formula = y ~ x)
    }
  }


  return(plt)
}

#' Create plots for multiple bird species. If no species specified, plot for all available.
#'
#' @param bbs_dat Dataframe with BBS data.
#' @param birds Character vector with bird species codes.
#'
#' @return List of ggplots for each bird species in birds.
#' @export
#'
plot_species_counts <- function(bbs_dat, birds = c()) {
  # If bird unspecified, grab all of them
  if(length(birds) == 0) {
    birds = unique(bbs_dat$Species_Code)
  }

  # apply plot function across all species
  plts <- lapply(birds, plot_bird_count, bbs_dat = bbs_dat)

  return(plts)
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

#' Convenient wrapper for filtering bird species in BBS data.
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

#' Create a summary table for count data availability.
#'
#' @param counts Count dataframe from islandwide_counts.
#' @param order_col Character vector for column to sort table by. One of Species_Code, firstYr, lastYr, propSurveys, lastCount.
#' @param descending Logical to sort in descending order. Default is ascending.
#'
#' @return gt table with data availability info.
#' @export
#'
species_counts_summary <- function(counts, order_col = "firstYr", descending = FALSE) {
  # Total number of surveys recorded
  n_surveys <- length(unique(counts$YrQtr))
  # Create summary for each species
  gt_dat <- counts %>%
            group_by(.data$Species_Code) %>%
            summarise(firstYr = floor(min(.data$YrQtr)),
                      lastYr = floor(max(.data$YrQtr)),
                      propSurveys = n() / n_surveys,
                      lastCount = .data$Count[which(.data$YrQtr == max(.data$YrQtr))])
  # Order by column, ascending is default
  if(descending) {
    gt_dat <- arrange(gt_dat, desc(gt_dat[[order_col]]))
  } else {
    gt_dat <- arrange(gt_dat, gt_dat[[order_col]])
  }

  # Build table
  sum_gt <- gt_dat %>%
            gt::gt(rowname_col = "Species_Code") %>%
            gt::fmt_percent(columns = "propSurveys",
                            decimals = 0) %>%
            gt::cols_label(Species_Code = "Species Code",
                           firstYr = "First Detected",
                           lastYr = "Last Detected",
                           propSurveys = "% of Surveys Detected In",
                           lastCount = "Most Recent Count") %>%
            gt::tab_header("Summary of Counts by Species")

  return(sum_gt)
}
