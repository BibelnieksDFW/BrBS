# Import frequently used operators/functions/packages
#' @importFrom magrittr %>%
NULL
#' @importFrom readxl read_xlsx cell_cols
NULL
#' @import dplyr
NULL
#' @import ggplot2
NULL
#' @importFrom rlang .data
NULL

#' Get rows for each year
#'
#' @param db_file Path to the BBS .xlsx database file.
#' @param sheet Sheet to read. Either string with island name or integer.
#'
#' @return Dataframe with island, year, and first/last rows in Excel sheet corresponding to that year.
#' @export
#'
get_year_rows <- function(db_file, sheet) {

  all_year <- read_xlsx(db_file, sheet = sheet, range = cell_cols("A:B"))
  # Create row numbers to be trimmed to first occurrence
  all_year$first <- seq(1,nrow(all_year))
  # Find where the changes are
  yrDiff <- all_year$Year-c(0,all_year$Year[1:nrow(all_year)-1])
  # Start building return df by trimming to changes
  ret <- all_year[which(yrDiff!=0),]
  # Get last by subtracting 1 from next first
  ret$last <- c(ret$first[2:nrow(ret)]-1, all_year$first[nrow(all_year)])
  # Set names to lowercase
  names(ret) <- c("island", "year", "first", "last")
  # Count cumulative occurrences of each year
  ret <- ret %>%
         group_by(.data$year) %>%
         mutate(occ = 1:n(), nocc = n()) %>%
         ungroup()

  ret
}


#' Read rows from database. Used to retrieve contiguous survey segments
#'
#' @param db_file Path to the BBS .xlsx database file.
#' @param db_names Column names in BBS database file.
#' @param year_range Dataframe with 1 row of from get_year_rows. Specifies island, year, and corresponding rows in database file.
#'
#' @return Dataframe containing BBS observations from that island and year.
#' @export
#'
get_rows <- function(db_file, db_names, year_range) {
  # Build range string
  rng <- paste0(year_range$island,"!A",year_range$first+1,":R",year_range$last+1)
  # Read from db file
  ret <- read_xlsx(db_file, col_names = db_names, range = rng)
}


#' Get all years for an island and store as a list - avoid slow re-reading from Excel
#'
#' @param db_file Path to the BBS .xlsx database file.
#' @param year_ranges Dataframe of the kind returned by get_year_rows.
#' @param isl One of "Saipan", "Rota", "Tinian".
#'
#' @return List where each entry is a dataframe with yearly data for that island.
#' @export
#'
get_island_year_list <- function(db_file, year_ranges, isl) {
  # Get column names from first row of sheet
  db_names <- names(read_xlsx(db_file, sheet = isl, n_max = 1))
  # Start reading in rows in year-batches
  yr_dat_list <- list()
  for(i in 1:nrow(year_ranges)) {
    yr_dat_list[[i]] <- get_rows(db_file, db_names, year_ranges[i,])
    print(sprintf("Year %d Read (%d/%d) ",
                  year_ranges$year[i],
                  year_ranges$occ[i],
                  year_ranges$nocc[i]
                  )
          )
  }

  yr_dat_list
}


#' Clean and combine list from get_island_year_list into one dataframe.
#'
#' @param all_yrs List from get_island_year_list
#'
#' @return Dataframe from combining entries of all_yrs list, with additional column YrQtr
#' @export
#'
year_list_to_df <- function(all_yrs) {
  for(i in 1:length(all_yrs)) {
    yr_dat <- all_yrs[[i]]
    # Check if Date still needs to be type-corrected
    if(!lubridate::is.POSIXct(yr_dat$Date)) {
      yr_dat <- fix_date(yr_dat)
    }
    # Format Start to %H:%M string
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
    # Create or clean up Replicate column
    if("Replicate" %in% names(yr_dat)) {
      # Change NA or 3 (code for partial survey) to 1
      yr_dat <- yr_dat %>%
        mutate( Replicate = as.factor(ifelse(is.na(.data$Replicate) |
                                               .data$Replicate == 3,
                                             yes = 1,
                                             no = .data$Replicate)))
    } else {
      # If there isn't a Replicate column, make one
      yr_dat <- yr_dat %>%
        mutate(Replicate = factor(1, levels = c(1,2)))
    }
    yr_dat <- yr_dat %>%
              mutate(Station = as.character(.data$Station),
                     YrQtr = .data$Year + (lubridate::quarter(.data$Date)-1)*0.25
                     ) %>%
              select(.data$Island,
                     .data$YrQtr,
                     .data$Year,
                     .data$Observer,
                     .data$Station,
                     .data$X,
                     .data$Y,
                     .data$Date,
                     .data$Start,
                     .data$Species_Code,
                     .data$Count,
                     .data$Replicate
                     )
    all_yrs[[i]] <- yr_dat
    print(sprintf("%d Cleaned (%d/%d)",
                  floor(min(yr_dat$YrQtr)),
                  i,
                  length(all_yrs)
          )
    )
  }
  # All years should now have same format - bind into df and return
  bind_rows(all_yrs)
}



#' Format dates that aren't already of the standard forms "%Y-%m-%d" or "%Y/%m/%d"
#'
#' @param dat Dataframe with BBS year data to be corrected.
#'
#' @return Same dataframe as input, but correctly formatted date column
#' @export
#'
fix_date <- function(dat) {
  # Identify forms of date strings
  # Numeric - number of days since Excel's weird origin
  num_idx <- grep("^[[:digit:]]*$", dat$Date)
  # Out of standard order - %m/%d/%Y
  slash_idx <- grep("[[:digit:]]{1,2}/[[:digit:]]{1,2}/[[:digit:]]{2,4}", dat$Date)

  fix_num <- data.frame(idx = num_idx,
                         fixed =
                           # For the ones that are numbers count days from Excel's weird origin - 12/30/1899
                           as.Date(as.numeric(dat$Date[num_idx]),
                                   origin = "1899-12-30"))
  fix_slash <- data.frame(idx = slash_idx,
                         fixed =
                           # For the almost date ones, it's real easy to get them there
                           as.Date(dat$Date[slash_idx],
                                   format = "%m/%d/%y"))
  Date_fixed <- rbind(fix_num, fix_slash) %>%
                arrange(.data$idx) %>%
                select(.data$fixed)
  dat <- dat %>%
          mutate(Date = Date_fixed$fixed)
}

#' Function to create rows for missing 0's in yr_dat
#'
#' @param Island String - island of observation.
#' @param Year Integer - year of observation
#' @param Observer String - initials of observer or "UNK" for unknown
#' @param Station String - station of observation
#' @param X Numeric - x coord of station
#' @param Y Numeric - y coord of station
#' @param Date Date - date of observation
#' @param Start Time - start time of survey
#' @param Species_Code Character vector with all species codes of birds to create rows for
#' @param Count Integer - number of birds counted, typically 0 in this usage
#'
#' @return Dataframe with new rows for adding into BBS dataframe.
#' @export
#'
create_spec_count_rows <- function(Island = NA, Year = NA,
                                   Observer = "UNK", Station = NA,
                                   X = NA, Y = NA, Date = NA, Start = NA,
                                   Species_Code = NA, Count = NA) {
  # create number of rows equal to number of species
  nrows <- length(Species_Code)
  new_rows <- data.frame(Island = rep(Island, nrows),
                         Year = rep(Year, nrows),
                         Observer = rep(Observer, nrows),
                         Station = rep(Station, nrows),
                         X = rep(X, nrows),
                         Y = rep(Y, nrows),
                         Date = rep(Date, nrows),
                         Start = rep(Start, nrows),
                         End = rep(NA, nrows),
                         Noise = rep(NA, nrows),
                         Wind = rep(NA, nrows),
                         Rain = rep(NA, nrows),
                         Cloud = rep(NA, nrows),
                         Species_Code = Species_Code,
                         Count = rep(Count, nrows),
                         Distance = rep(NA, nrows),
                         Comments = rep(NA, nrows),
                         Replicate = rep(NA, nrows)
  )
}
