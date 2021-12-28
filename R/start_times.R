#' Gather survey start times to use in relation to sunrise as a covariate
#'
#' @param all_yrs List of dataframes produced by get_island_year_list.
#' @param isl One of "Saipan", "Rota", "Tinian".
#'
#' @return Dataframe with year-quarter, start time in minutes from sunrise, and bins for more/less than 30 minutes before/after sunrise
#' @export
#'
#' @examples
get_survey_start_times <- function(all_yrs, isl) {
  strt_yr_date <- list()
  for(i in 1:length(all_yrs)) {
    yr_dat <- all_yrs[[i]]

    # Fix date column for 2021
    if(!lubridate::is.POSIXct(yr_dat$Date)) {
      yr_dat <- fix_date(yr_dat)
    }

    # Fix formatting of Start to just %H:%M
    if(lubridate::is.POSIXct(yr_dat$Start)) {
      yr_dat <- yr_dat %>%
                mutate(Start = format(.data$Start, "%H:%M"))
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
                      arrange(.data$idx) %>%
                      select(.data$fixed)
      yr_dat <- yr_dat %>%
                 mutate(Start = as.character(Start_fixed$fixed))
    }



    strt_yr_date[[i]] <- yr_dat %>%
      mutate(Month = lubridate::month(.data$Date),
             Day = lubridate::day(.data$Date)) %>%
      select(.data$Year, .data$Station, .data$Month, .data$Day, .data$Start) %>%
      unique() %>%
      group_by(.data$Year, .data$Month, .data$Day) %>%
      summarise(N_Stations = n(), Earliest = min(.data$Start)) %>%
      mutate(Qtr = ceiling(.data$Month/3))
    print(sprintf("Year %d complete", yr_dat$Year))
  }
  strt_yr_date <- dplyr::bind_rows(strt_yr_date)

  # Get island lat/long

  isl_lat <- case_when(isl == "Saipan" ~ 15.183,
                       isl == "Rota" ~ 15.183,
                       isl == "Tinian" ~ 15.183)
  isl_long <- case_when(isl == "Saipan" ~ 145.75,
                        isl == "Rota" ~ 15.183,
                        isl == "Tinian" ~ 15.183)

  # Add in sunrise times
  strt_yr_date <- strt_yr_date %>%
    mutate(Sunrise = suncalc::getSunlightTimes(date = lubridate::make_date(.data$Year, .data$Month, .data$Day),
                                      lat = isl_lat,
                                      lon = isl_long,
                                      keep = "sunrise", tz = "Pacific/Saipan") %>% .data$sunrise,
           Diff_to_Sunrise = difftime(lubridate::make_datetime(.data$Year, .data$Month, .data$Day,
                                                    hour = .data$Earliest %>%
                                                      substr(1, 2) %>%
                                                      as.numeric(),
                                                    min = .data$Earliest %>%
                                                      substr(4, 5) %>%
                                                      as.numeric(),
                                                    tz = "Pacific/Saipan"),
                                      .data$Sunrise, units = "mins") %>% as.numeric)


  # Create bins at Quarter level for discretizing time before/after sunrise that the survey began
  strt_yr_qtr_binned <- strt_yr_date %>%
    mutate(YrQtr = .data$Year + (.data$Qtr-1)*0.25) %>%
    group_by(.data$YrQtr) %>%
    summarise(Mean_Sunrise_Diff = mean(.data$Diff_to_Sunrise)) %>%
    mutate(Mean_Sunrise_Diff_Class = factor(
      case_when(.data$Mean_Sunrise_Diff < 0 & .data$Mean_Sunrise_Diff > -30 ~ "<30 Before",
                .data$Mean_Sunrise_Diff <= -30 & .data$Mean_Sunrise_Diff > -200 ~ ">30 Before",
                .data$Mean_Sunrise_Diff > 0 & .data$Mean_Sunrise_Diff < 30 ~ "<30 After",
                .data$Mean_Sunrise_Diff >= 30 & .data$Mean_Sunrise_Diff < 200 ~ ">30 After",
                TRUE ~ "Not_Recorded")),
      Mean_Sunrise_Diff = ifelse(.data$Mean_Sunrise_Diff_Class == "Not_Recorded", NA, .data$Mean_Sunrise_Diff))
}

#' Plot start times over time.
#'
#' @param start_dat Start time dataframe from get_survey_start_times
#'
#' @return Plot of start times over time.
#' @export
#'
#' @examples
plot_start_times <- function(start_dat) {

  min_yr <- floor(min(start_dat$YrQtr))
  max_yr <- ceiling(max(start_dat$YrQtr))

  plt <- start_dat %>%
    mutate(Qtr = factor((.data$YrQtr-floor(.data$YrQtr))*4+1)) %>%
    filter(.data$Mean_Sunrise_Diff_Class != "Not_Recorded") %>%
    ggplot(aes(x = .data$YrQtr, y = .data$Mean_Sunrise_Diff,
               color = .data$Mean_Sunrise_Diff_Class,
               shape = .data$Qtr)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_x_continuous(breaks = seq(min_yr, max_yr, 5), minor_breaks = seq(min_yr,max_yr,1)) +
    labs(y = "Avg Start Time (Minutes from Sunrise)",
         color = "Interval",
         shape = "Quarter") +
    theme(axis.title.x = element_blank(),
          legend.position = c(.25, .75),
          legend.box = "horizontal")

  plt
}
