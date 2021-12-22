# Gather observers over time
#' Title
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
