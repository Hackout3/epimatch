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
#'
#' @examples
#' test <- data.frame(c("21-Jan-01", "25-Jan-02", "21-Jan-01"), stringsAsFactors = F)
#' dateDists(test)
dateDists <- function(dat1, dat2=NULL,
                      dat1Format="%d-%b-%y", dat2Format="%d-%b-%y",
                      threshold=NULL)
{
  # Read in a column, with defined format. If necessary combine with dat2
  date_col = as.Date(dat1[,1],dat1Format)
  if (!is.null(dat2))
  {
    dat1 <- c(date_col, as.Date(dat2[,1],dat2Format))
  }

  # Threshold distances if asked
  dists <- as.matrix(dist(date_col))
  if(!is.null(threshold))
  {
    dists[dists < threshold] <- 0
    dists[dists >= threshold] <- 1
  }
  else
  {
    dists <- dists/max(dists)
  }

  return(dists)
}
