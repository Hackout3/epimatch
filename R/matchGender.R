#' gender function for matching
#' A thin wrapper around generic, which converts some known male and female
#' codes to binary
#'
#' @param dat1 a vector of genders to match
#' @param dat2 a vector of genders to match
#' @param m format of males
#' @param f format of females
#'
#' @return a pairwise matrix of scores from 0 (exact match) to 1 (no match)
#' @export
#'
#' @examples
#' test <- data.frame(c("male", "f", "m"), stringsAsFactors = F)
#' genderDists(test)
genderDists <- function(dat1, dat2=NULL,
                      m=c("m", "male", "mal", "1"), f=c("f", "female", "fem", "2"))
{
  # Convert to zeros and ones
  dat1 <- cleanString(dat1)
  dat1[dat1[,1] %in% m,1] <- 0
  dat1[dat1[,1] %in% f,1] <- 1

  if(!is.null(dat2))
  {
    dat2 <- cleanString(dat2)
    dat2[dat2[,1] %in% m,1] <- 0
    dat2[dat2[,1] %in% f,1] <- 1
  }

  return(genericDists(dat1, dat2))
}
