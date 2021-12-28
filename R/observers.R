#' Gather observers over time
#'
#' @param bbs_dat Dataframe with BBS data. Must have columns YrQtr, Observer, and Replicate
#'
#' @return Dataframe with year-quarter, observer's initials, chronological order, and number of surveys conducted.
#' @export
#'
#' @examples
get_observers <- function(bbs_dat) {
  # Get observers for every year-quarter
  obs_dat <- bbs_dat %>%
              mutate(Replicate = as.factor(.data$Replicate)) %>%
              select(.data$YrQtr, .data$Observer, .data$Replicate) %>%
              unique() %>%
              mutate(Observer = gsub("\\.", "", .data$Observer))

  # Identify unique observers by initials, breaking up multiples
  # Single initials
  obs_dat_single <- obs_dat[grep("^[[:upper:]]{2,3}$", obs_dat$Observer),]
  # Full names
  obs_dat_full <- obs_dat[grep("^[[:upper:]][^[:upper:]]", obs_dat$Observer),] %>%
    mutate(Observer = gsub("[^[:upper:]]", "", .data$Observer))
  # Multiple Initials
  obs_dat_mult <- obs_dat[grep("^[[:upper:]]{2,3}\\W", obs_dat$Observer),] %>%
    mutate(Observer = stringr::str_extract_all(.data$Observer,"[[:upper:]]{2,3}")) %>%
    tidyr::unnest(cols = .data$Observer)
  # Recombine initials, collapse 3-letter initials to 2-letter
  obs_dat <- bind_rows(obs_dat_single,
                       obs_dat_full,
                       obs_dat_mult) %>%
             mutate(Observer = ifelse(nchar(.data$Observer)==3 &
                                        .data$Observer != "UNK",
                                      yes = paste0(substr(.data$Observer,1,1),
                                             substr(.data$Observer,3,3)),
                                      no = .data$Observer))
  # Add summary info: chronological order & number of years
  obs_dat <- obs_dat %>%
    left_join(
      obs_dat %>%
        filter(.data$Observer != "UNK") %>%
        group_by(.data$Observer) %>%
        summarise(start = min(.data$YrQtr)) %>%
        arrange(.data$start) %>%
        mutate(order = row_number(), start = NULL) %>%
        bind_rows(data.frame(Observer = "UNK", order = Inf)),
      by = "Observer"
    ) %>%
    left_join(
      obs_dat %>%
        group_by(.data$Observer) %>%
        summarise(nyrs = n()),
      by = "Observer"
    ) %>%
    arrange(order)
}


#' Generate plot of observers over time
#'
#' @param obs_dat Observer dataframe from get_obervers.
#' @param filter_obs Filter observers to only show those who have conducted more than one survey. Default does not filter.
#'
#' @return Plot of observers over time.
#' @export
#'
#' @examples
plot_observers <- function(obs_dat, filter_obs = FALSE) {
  min_yr <- floor(min(obs_dat$YrQtr))
  max_yr <- ceiling(max(obs_dat$YrQtr))
  if(filter_obs) {
    plt <- obs_dat %>%
            filter(.data$nyrs>1) %>%
            mutate(Qtr = factor((.data$YrQtr-floor(.data$YrQtr))*4+1)) %>%
            ggplot(aes(y = stats::reorder(.data$Observer, order), x = .data$YrQtr)) +
            geom_point(aes(shape = .data$Qtr, color = .data$Replicate)) +
            geom_line(linetype = "dotted") +
            scale_x_continuous(breaks = seq(min_yr, max_yr, 5), minor_breaks = seq(min_yr,max_yr,1)) +
            scale_color_manual(values = c("black","red")) +
            labs(y = "Observer (>1 Survey)",
                       shape = "Quarter") +
            theme(axis.title.x = element_blank(),
                        legend.position = c(.1, .75),
                        legend.box = "horizontal")
  } else {
    plt <- obs_dat %>%
      mutate(Qtr = factor((.data$YrQtr-floor(.data$YrQtr))*4+1)) %>%
      ggplot(aes(y = stats::reorder(.data$Observer, order), x = .data$YrQtr)) +
      geom_point(aes(shape = .data$Qtr, color = .data$Replicate)) +
      geom_line(linetype = "dotted") +
      scale_x_continuous(breaks = seq(min_yr, max_yr, 5), minor_breaks = seq(min_yr,max_yr,1)) +
      scale_color_manual(values = c("black","red")) +
      labs(y = "Observer",
                    shape = "Quarter") +
      theme(axis.title.x = element_blank(),
                     legend.position = c(.1, .75),
                     legend.box = "horizontal")
  }

  plt
}
