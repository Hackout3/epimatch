#' generic function for exact matching single columns
#'
#' @param dat1 a vector of data to determine matching
#' @param dat2 a vector of data to determine matching
#' @param strings whether to use the edit distance rather than exact matches
#'
#' @return a pairwise matrix of scores from 0 (exact match) to 1 (no match)
#' @export
#'
#' @examples
#' genericDists(1:3)
genericDists <- function(dat1, dat2 = NULL, strings = FALSE){
	dat1 <- cleanString(dat1)

  if (!is.null(dat2))
	{
	  dat1 <- rbind(dat1, cleanString(dat2))
	}

  # String distance matching
  if (strings)
  {
    dists <- adist(dat1)
  }
  # General exact matches
  else
  {
    n <- seq_len(nrow(dat1))
    id <- expand.grid(n, n)
    dists <- matrix(rowSums(dat1[id[,1], ,drop = FALSE] != dat1[id[,2], ,drop = FALSE]), ncol=length(n))
    diag(dists) <- 0
  }
  return(dists/max(dists))
}
