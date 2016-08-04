#' date function for matching
#'
#' @param dat1,dat2 a vector of dates to match
#' @param dat1Format,dat2Format format of dates to pass to \pkg{lubridate} in
#'   the form of "mdy" or "ymd" etc.
#' @param threshold in days, otherwise returns scaled values
#'
#' @details The formats for dates are passed to \pkg{lubridate}'s function
#'   \code{\link{parse_date_time}}, so they need to be in the format "mdy" or
#'   "ymd", etc.
#'
#' @return a pairwise matrix of scores from 0 (exact match) to 1 (no match)
#' @export
#' @importFrom lubridate parse_date_time
#'
#' @examples
#' library('epimatch')
#'
#' # Two data sets in Month-Day-Year and Year-Month-Day format, respectively.
#' # Note that the way the dates can vary within data sets, but it's assumed
#' # that the order within a data set is consistant.
#'
#' test1 <- data.frame(x = c("Jan-21-01", "01-25-02"), stringsAsFactors = FALSE)
#' test2 <- data.frame(x = c("01-JAN-21", "2001-Jan-31"), stringsAsFactors = FALSE)
#'
#' # match dates within 11 days of each other
#' dateDists(test1, test2, dat1Format = "mdy", dat2Format = "ymd", threshold = 11)
#'
#' # exact match of dates
#' dateDists(test1, test2, dat1Format = "mdy", dat2Format = "ymd")
#'
#' # relative distance of dates by setting threshold to NULL
#' dateDists(test1, test2, dat1Format = "mdy", dat2Format = "ymd", threshold = NULL)
#'
dateDists <- function(dat1, dat2 = NULL, dat1Format = "mdy", dat2Format = "mdy",
                      threshold = 0L){
  # Function for preprocessing of formats. Since a date with 2001 forces a date
  # with 01 to be the year 0001, we need to modify the select_formats parameter
  # for parse_date_time. I don't fully understand the mechanics of this, but the
  # function below ensures that the two digit years are also in context of the
  # four digit years.
  sf <- function(trained){
    n_fmts <- nchar(gsub("[^%]", "", names(trained))) + grepl("%y", names(trained))*1.5
    names(trained[ which.max(n_fmts) ])
  }
  date_col <- lubridate::parse_date_time(dat1[[1]], dat1Format, select_formats = sf)
  # Read in a column, with defined format. If necessary combine with dat2
  if (!is.null(dat2))
  {
    dat2     <- lubridate::parse_date_time(dat2[[1]], dat2Format, select_formats = sf)
    date_col <- c(date_col, dat2)
  }

  # Threshold distances if asked
  dists     <- as.matrix(dist(date_col))
  diststime <- as.difftime(dists, units = "secs")

  if (!is.null(threshold))
  {
    dists <- diststime > as.difftime(threshold, units = "days")
    mode(dists) <- "integer"
  }
  else
  {
    out <- matrix(0.0, nrow = nrow(dists), ncol = ncol(dists))
    out[] <- as.double(dists/as.integer(max(dists, na.rm = TRUE)))
    dists <- out
  }

  return(dists)
}

