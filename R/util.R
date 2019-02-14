# Various utility functions.  Some material from tables.R could move here.

#' Convert date text strings into YYY-MM-DD
#'
#' Convert month/day/year date stamps, like 5/31/2016, or other permutations of
#' those positions (e.g., 31/5/2016, 2016/5/31), to the  ISO 8601 standard of
#' YYY-MM-DD.  Any NA values in output that do not have associated blank or NA
#' values in input trigger an error.  This is just a wrapper around
#' \code{as.Date} to help with formatting and error checking.
#'
#' @param datetxt character vector of dates
#' @param ord_input character vector containing the values "M", "D", and "Y"
#'   ordered to specify the month, date, and year fields
#'
#' @return character vector of ISO 8601-formatted date stamps
#'
#' @export
#'
#' @examples
#' dates <- c("5/31/2016", "10/1/2018", "1/2/2019")
#' datemunge(dates)
#' dates_euro <- c("31/5/2016", "1/10/2018", "2/1/2019")
#' datemunge(dates_euro, ord_input = c("D", "M", "Y"))
datemunge <- function(datetxt, ord_input = c("M", "D", "Y")) {
  # build a format string from the order of the fields
  fmt <- ord_input
  fmt <- gsub("M", "m", fmt)
  fmt <- gsub("D", "d", fmt)
  fmt <- paste0("%", fmt, collapse = "/")
  dateout <- format(as.Date(datetxt, fmt), "%Y-%m-%d")
  # NA or blank input can give NA output, but otherwise we'll strictly disallow
  # the usual sloppy casting in R.
  baddates <- is.na(dateout) & ! blank(datetxt)
  if (any(baddates)) {
    stop(paste("Date formatting failed for:",
               paste(datetxt[baddates], collapse = ", ")))
  }
  return(dateout)
}
