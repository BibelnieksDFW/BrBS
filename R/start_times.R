#' Gather survey start times to use in relation to sunrise as a covariate
#'
#' @param all_yrs List of dataframes produced by get_island_year_list.
#' @param yrs Numeric vector of years to pull from all_yrs
#' @param isl One of "Saipan", "Rota", "Tinian".
#'
#' @return Dataframe with year-quarter, start time in minutes from sunrise, and bins for more/less than 30 minutes before/after sunrise
#' @export
#'
#' @examples
get_survey_start_times <- function(all_yrs, yrs, isl) {
  strt_yr_date <- list()
  for(i in seq_along(yrs)) {
    yr <- yrs[i]
    yr_dat <- all_yrs[[i]]

    # Fix date column for 2021
    if(yr == 2021) {
      yr_dat <- fix_date_2021(yr_dat)
    }

    # Fix formatting of Start to just %H:%M
    if(lubridate::is.POSIXct(yr_dat$Start)) {
      yr_dat <- yr_dat %>%
                dplyr::mutate(Start = format(Start, "%H:%M"))
    } else if(is.character(yr_dat$Start) &
              length(grep("[:digit:]*:[:digit:]*",yr_dat$Start)) != length(yr_dat$Start)) {
      # Don't change any that are already ok
      ok <- data.frame(idx = grep("[:digit:]*:[:digit:]*",yr_dat$Start),
                       fixed = yr_dat$Start[grep("[:digit:]*:[:digit:]*",yr_dat$Start)])
      # Whatever read in as numbers needs to be reformatted
      need_fix <- data.frame(idx = grep("0\\.[:digit:]*",yr_dat$Start),
                             fixed = (yr_dat$Start[grep("0\\.[:digit:]*",yr_dat$Start)] %>%
                                        as.numeric() * 86400) %>%
                               as.POSIXct(origin = "1899-12-30", tz = "UTC") %>%
                               format("%H:%M"))


      Start_fixed <- rbind(ok, need_fix) %>%
                      dplyr::arrange(idx) %>%
                      dplyr::select(fixed)
      yr_dat <- yr_dat %>%
                 dplyr::mutate(Start = as.character(Start_fixed$fixed))
    }



    strt_yr_date[[i]] <- yr_dat %>%
      dplyr::mutate(Month = lubridate::month(Date),
             Day = lubridate::day(Date)) %>%
      dplyr::select(Year, Station, Month, Day, Start) %>%
      unique() %>%
      dplyr::group_by(Year, Month, Day) %>%
      dplyr::summarise(N_Stations = dplyr::n(), Earliest = min(Start)) %>%
      dplyr::mutate(Qtr = ceiling(Month/3))
    print(sprintf("Year %d complete", yr))
  }
  strt_yr_date <- dplyr::bind_rows(strt_yr_date)

  # Get island lat/long

  isl_lat <- dplyr::case_when(isl == "Saipan" ~ 15.183,
                       isl == "Rota" ~ 15.183,
                       isl == "Tinian" ~ 15.183)
  isl_long <- dplyr::case_when(isl == "Saipan" ~ 145.75,
                        isl == "Rota" ~ 15.183,
                        isl == "Tinian" ~ 15.183)

  # Add in sunrise times
  strt_yr_date <- strt_yr_date %>%
    dplyr::mutate(Sunrise = suncalc::getSunlightTimes(date = lubridate::make_date(Year, Month, Day),
                                      lat = isl_lat,
                                      lon = isl_long,
                                      keep = "sunrise", tz = "Pacific/Saipan") %>% .$sunrise,
           Diff_to_Sunrise = difftime(lubridate::make_datetime(Year, Month, Day,
                                                    hour = Earliest %>%
                                                      substr(1, 2) %>%
                                                      as.numeric(),
                                                    min = Earliest %>%
                                                      substr(4, 5) %>%
                                                      as.numeric(),
                                                    tz = "Pacific/Saipan"),
                                      Sunrise, units = "mins") %>% as.numeric)


  # Create bins at Quarter level for discretizing time before/after sunrise that the survey began
  strt_yr_qtr_binned <- strt_yr_date %>%
    dplyr::mutate(YrQtr = Year + (Qtr-1)*0.25) %>%
    dplyr::group_by(YrQtr) %>%
    dplyr::summarise(Mean_Sunrise_Diff = mean(Diff_to_Sunrise)) %>%
    dplyr::mutate(Mean_Sunrise_Diff_Class = factor(
      dplyr::case_when(Mean_Sunrise_Diff < 0 & Mean_Sunrise_Diff > -30 ~ "<30 Before",
                Mean_Sunrise_Diff <= -30 & Mean_Sunrise_Diff > -200 ~ ">30 Before",
                Mean_Sunrise_Diff > 0 & Mean_Sunrise_Diff < 30 ~ "<30 After",
                Mean_Sunrise_Diff >= 30 & Mean_Sunrise_Diff < 200 ~ ">30 After",
                TRUE ~ "Not_Recorded")),
      Mean_Sunrise_Diff = ifelse(Mean_Sunrise_Diff_Class == "Not_Recorded", NA, Mean_Sunrise_Diff))
}
