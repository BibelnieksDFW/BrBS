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
#' @examples
get_year_rows <- function(db_file, sheet) {

  all_year <- read_xlsx(db_file, sheet = "Rota", range = cell_cols("A:B"))
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
         group_by(year) %>%
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
#' @examples
get_rows <- function(db_file, db_names, year_range) {
  # Build range string
  rng <- paste0(year_range$island,"!A",year_range$first+1,":R",year_range$last+1)
  # Read from db file
  ret <- read_xlsx(db_file, col_names = db_names, range = rng)
}


#' Get all years for an island and store as a list - avoid slow re-reading from Excel
#'
#' @param db_file Path to the BBS .xlsx database file.
#' @param db_names Column names in BBS database file.
#' @param year_ranges Dataframe of the kind returned by get_year_rows.
#' @param isl One of "Saipan", "Rota", "Tinian".
#'
#' @return List where each entry is a dataframe with yearly data for that island.
#' @export
#'
#' @examples
get_island_year_list <- function(db_file, db_names, year_ranges, isl) {
  yr_dat_list <- list()
  for(i in 1:nrow(year_ranges)) {
    yr_dat_list[[i]] <- get_rows(db_file, db_names, year_ranges[i,])
    print(sprintf("Year %d (%d/%d) ",
                  year_ranges$year[i],
                  year_ranges$occ[i],
                  year_ranges$nocc[i]
                  )
          )
  }

  yr_dat_list
}


#' Fix inconsistent date formatting for 2021
#'
#' @param dat Dataframe with 2021 BBS year data
#'
#' @return Same dataframe as input, but correctly formatted date column
#' @export
#'
#' @examples
fix_date_2021 <- function(dat) {
  fix_len5 <- data.frame(idx = which(nchar(dat$Date) == 5),
                         fixed =
                           # For the ones that are numbers count days from Excel's weird origin - 12/30/1899
                           as.Date(as.numeric(dat$Date[which(nchar(dat$Date) == 5)]),
                                   origin = "1899-12-30"))
  fix_len8 <- data.frame(idx = which(nchar(dat$Date) == 8),
                         fixed =
                           # For the almost date ones, it's real easy to get them there
                           as.Date(dat$Date[which(nchar(dat$Date) == 8)],
                                   format = "%m/%d/%y"))
  Date_fixed <- rbind(fix_len5, fix_len8) %>%
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
#' @examples
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
