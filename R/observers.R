#' Gather observers over time
#'
#' @param all_yrs List of dataframes produced by get_island_year_list.
#' @param yrs Numeric vector of years to pull from all_yrs
#'
#' @return Dataframe with year-quarter, observer's initials, chronological order, and number of surveys conducted.
#' @export
#'
#' @examples
get_observers <- function(all_yrs, yrs) {
  # all_yrs=all_yrs;yrs=year_ranges_Saipan$year; i=22
  # Get observers for every year-quarter
  obs_dat <- list()
  for(i in seq_along(yrs)) {
    yr <- yrs[i]
    yr_dat <- all_yrs[[i]]
    # Fix date column for 2021
    if(yr == 2021) {
      yr_dat <- fix_date_2021(yr_dat)
    }
    obs_dat[[i]] <- yr_dat %>%
      dplyr::mutate(YrQtr = Year + (lubridate::quarter(Date)-1)*0.25) %>%
      dplyr::select(YrQtr, Observer, Replicate) %>%
      dplyr::mutate(Replicate = as.factor(ifelse(is.na(Replicate) | Replicate == 3, 1, Replicate))) %>%
      unique()
  }
  obs_dat <- dplyr::bind_rows(obs_dat)
  # Identify unique observers by initials, breaking up multiples
  # Single initials
  obs_dat_single <- obs_dat[grep("^[[:upper:]]{2,3}$", obs_dat$Observer),]
  # Full names
  obs_dat_full <- obs_dat[grep("^[[:upper:]][^[:upper:]]", obs_dat$Observer),] %>%
    dplyr::mutate(Observer = gsub("[^[:upper:]]", "", Observer))
  # Multiple Initials
  obs_dat_mult <- obs_dat[grep("^[[:upper:]]{2,3}\\W", obs_dat$Observer),] %>%
    dplyr::mutate(Observer = stringr::str_extract_all(Observer,"[[:upper:]]{2,3}")) %>%
    tidyr::unnest(cols = Observer)
  # Recombine initials
  obs_dat <- dplyr::bind_rows(obs_dat_single,
                       obs_dat_full,
                       obs_dat_mult)
  # Add summary info: chronological order & number of years
  obs_dat <- obs_dat %>%
    dplyr::left_join(
      obs_dat %>%
        dplyr::filter(Observer != "UNK") %>%
        dplyr::group_by(Observer) %>%
        dplyr::summarise(start = min(YrQtr)) %>%
        dplyr::arrange(start) %>%
        dplyr::mutate(order = dplyr::row_number(), start = NULL) %>%
        dplyr::bind_rows(data.frame(Observer = "UNK", order = max(.$order)+1)),
      by = "Observer"
    ) %>%
    dplyr::left_join(
      obs_dat %>%
        dplyr::group_by(Observer) %>%
        dplyr::summarise(nyrs = dplyr::n()),
      by = "Observer"
    ) %>%
    dplyr::arrange(order)
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
  min_yr <- floor(min(obs$YrQtr))
  max_yr <- ceiling(max(obs$YrQtr))
  if(filter_obs) {
    plt <- obs_dat %>%
            dplyr::filter(nyrs>1) %>%
            dplyr::mutate(Qtr = factor((YrQtr-floor(YrQtr))*4+1)) %>%
            ggplot2::ggplot(ggplot2::aes(y = stats::reorder(Observer, order), x = YrQtr)) +
            ggplot2::geom_point(ggplot2::aes(shape = Qtr, color = Replicate)) +
            ggplot2::geom_line(linetype = "dotted") +
            ggplot2::scale_x_continuous(breaks = seq(min_yr, max_yr, 5), minor_breaks = seq(min_yr,max_yr,1)) +
            ggplot2::scale_color_manual(values = c("black","red")) +
            ggplot2::labs(y = "Observer (>1 Survey)",
                       shape = "Quarter") +
            ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                        legend.position = c(.1, .75),
                        legend.box = "horizontal")
  } else {
    plt <- obs_dat %>%
      dplyr::mutate(Qtr = factor((YrQtr-floor(YrQtr))*4+1)) %>%
      ggplot2::ggplot(ggplot2::aes(y = stats::reorder(Observer, order), x = YrQtr)) +
      ggplot2::geom_point(ggplot2::aes(shape = Qtr, color = Replicate)) +
      ggplot2::geom_line(linetype = "dotted") +
      ggplot2::scale_x_continuous(breaks = seq(min_yr, max_yr, 5), minor_breaks = seq(min_yr,max_yr,1)) +
      ggplot2::scale_color_manual(values = c("black","red")) +
      ggplot2::labs(y = "Observer",
                    shape = "Quarter") +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     legend.position = c(.1, .75),
                     legend.box = "horizontal")
  }

  plt
}
