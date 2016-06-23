#' date function for matching
#'
#' @param dat1 a vector of dates to match
#' @param dat2 a vector of dates to match
#' @param dat1Format format of dat1 dates
#' @param dat2Format format of dat2 dates
#' @param threshold in days, otherwise returns scaled values
#'
#' @return a pairwise matrix of scores from 0 (exact match) to 1 (no match)
#' @export
#' @importFrom lubridate parse_date_time
#'
#' @examples
#' test <- data.frame(c("21-Jan-01", "25-Jan-02", "21-Jan-01"), stringsAsFactors = F)
#' dateDists(test)
dateDists <- function(dat1, dat2=NULL, dat1Format="mdy", dat2Format="mdy",
                      threshold=NULL)
{

  date_col <- lubridate::parse_date_time(dat1[[1]], dat1Format)
  # Read in a column, with defined format. If necessary combine with dat2
  if (!is.null(dat2))
  {
    dat2 <- lubridate::parse_date_time(dat2[[1]], dat2Format)
    date_col <- c(date_col, dat2)
  }

  # Threshold distances if asked
  dists <- as.matrix(dist(date_col))
  diststime <- as.difftime(m, units = "secs")

  if (!is.null(threshold))
  {
    dists <- diststime >= as.difftime(threshold, units = "hours")
    mode(dists) <- "integer"
  }
  else
  {
    out <- matrix(0.0, nrow = nrow(dists), ncol = ncol(dists))
    out[] <- as.double(dists/as.integer(max(dists)))
    dists <- out
  }

  return(dists)
}
