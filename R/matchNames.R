#' Clean and match names column by removing punctuation and replacing
#' punctuation with an underscore
#'
#' @param dat1 data frame containing name information
#' @param dat2 data frame containing name information
#'
#' @return a distance matrix giving the number of mismatches in names.
#' @export
#'
#' @examples
#' set.seed(9)
#' x <- data.frame(x = letters, y = LETTERS, z = 1:26)
#' x <- x[sample(nrow(x), 10, replace = TRUE), ]
#' nameDists(x)
nameDists <- function(dat1, dat2 = NULL){
  if (!is.null(dat2))
  {
    combined <- c(clean_names(dat1), clean_names(dat2))
  }
  else
  {
    combined <- clean_names(dat1)
  }

  datdist <- adist(combined)
  res <- datdist/max(datdist, na.rm = TRUE)
  return(res)
}
