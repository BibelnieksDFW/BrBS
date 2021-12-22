#' Make pipe available for use
#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


#' Get rows for each year
#'
#' @param db_file Path to the BBS .xlsx database file.
#' @param sheets Sheet to read. Either string with island name or integer.
#'
#' @return Dataframe with island, year, and first/last rows in Excel sheet corresponding to that year.
#' @export
#'
#' @examples
get_year_rows <- function(db_file, sheets) {
  year_ranges <- data.frame()
  for(sheet in sheets) {
    # Read in first 2 columns to get island and years
    all_year <- readxl::read_xlsx(db_file, sheet = sheet, range = cell_cols("A:B"))
    # Find first and last rows for each
    last <- length(all_year$Year)-match(unique(all_year$Year),rev(all_year$Year))+1
    first <- c(1,last[1:(length(last)-1)]+1)
    # Create return df
    year_ranges <- rbind(year_ranges, data.frame(island = all_year$Island[1],
                                                 year = unique(all_year$Year),
                                                 first = first, last = last))
  }
  return(year_ranges)
}


#' Read data for a given island and year
#'
#' @param db_file Path to the BBS .xlsx database file.
#' @param db_names Column names in BBS database file.
#' @param year_ranges Dataframe of the kind returned by get_year_rows.
#' @param isl One of "Saipan", "Rota", "Tinian".
#' @param yr Year to retrieve as an integer.
#'
#' @return Dataframe containing BBS observations from that island and year.
#' @export
#'
#' @examples
get_island_year <- function(db_file, db_names, year_ranges, isl, yr) {
  # Get first and last rows
  rows <- year_ranges %>%
    dplyr::filter(island == isl, year == yr) %>%
    dplyr::select(first, last)
  # Build range string
  rng <- paste0(isl,"!A",rows$first+1,":R",rows$last+1)
  # Read from db file
  ret <- readxl::read_xlsx(db_file, col_names = db_names, range = rng)
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
  for(i in seq_along(year_ranges$year)) {
    yr <- year_ranges$year[i]
    yr_dat_list[[i]] <- get_island_year(db_file, db_names, year_ranges, isl, yr)
    print(sprintf("Year %d complete", yr))
  }
  return(yr_dat_list)
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
  Date_fixed <- rbind(fix_len5, fix_len8) %>% arrange(idx) %>% select(fixed)
  dat <- dat %>%
          dplyr::mutate(Date = Date_fixed$fixed)
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
