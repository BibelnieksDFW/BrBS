#' Gather survey start times to use in relation to sunrise as a covariate
#'
#' @param bbs_dat Dataframe with BBS data. Must have columns Year, Station, Date,
#'
#' @return Dataframe with year-quarter, start time in minutes from sunrise, and bins for more/less than 30 minutes before/after sunrise
#' @export
#'
#' @examples
get_survey_start_times <- function(bbs_dat) {
  strt_yr_date <- bbs_dat %>%
    mutate(Date = .data$Date) %>%
    select(.data$Date, .data$Start) %>%
    unique() %>%
    group_by(.data$Date) %>%
    summarise(Earliest = min(.data$Start))


  # Get island lat/long
  isl <- bbs_dat$Island[1]

  isl_lat <- case_when(isl == "Saipan" ~ 15.183,
                       isl == "Rota" ~ 14.1509,
                       isl == "Tinian" ~ 15.0043)
  isl_long <- case_when(isl == "Saipan" ~ 145.75,
                        isl == "Rota" ~ 145.2149,
                        isl == "Tinian" ~ 145.6357)

  # Add in sunrise times
  strt_yr_date <- strt_yr_date %>%
    mutate(Sunrise = suncalc::getSunlightTimes(date = as.Date(.data$Date),
                                      lat = isl_lat,
                                      lon = isl_long,
                                      keep = "sunrise",
                                      tz = "Pacific/Saipan") %>%
                      select(.data$sunrise) %>%
                      pull(),
           Diff_to_Sunrise = difftime(as.POSIXct(paste(.data$Date, .data$Earliest),
                                                            format = "%Y-%m-%d %H:%M"),
                                      .data$Sunrise, units = "mins") %>% as.numeric)


  # Create bins at Quarter level for discretizing time before/after sunrise that the survey began
  strt_yr_qtr_binned <- strt_yr_date %>%
    mutate(YrQtr = lubridate::year(.data$Date) + (lubridate::quarter(.data$Date)-1)*0.25) %>%
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
