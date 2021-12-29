#' Sum counts across stations for island-wide totals
#'
#' @param bbs_dat Dataframe with BBS data.
#'
#' @return Dataframe with YrQtr, Species_Code, and Count.
#' @export
#'
#' @examples
islandwide_counts <- function(bbs_dat) {
  island_wide <- bbs_dat %>%
    filter(!is.na(.data$Count)) %>%
    group_by(.data$YrQtr, .data$Station, .data$Species_Code) %>%
    # For replicates, average across for counts (round to even)
    summarize(Count = round(mean(.data$Count))) %>%
    group_by(.data$YrQtr, .data$Species_Code) %>%
    summarize(Count = sum(.data$Count)) %>%
    ungroup()
}
