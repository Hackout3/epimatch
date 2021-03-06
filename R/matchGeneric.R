#' generic function for exact matching single columns
#'
#' @param dat1,dat2 data frames of data to match
#' @param strings whether to use the edit distance rather than exact matches
#'
#' @return a pairwise matrix of scores from 0 (exact match) to 1 (no match)
#' @export
#'
#' @examples
#' set.seed(9)
#' x <- data.frame(dat = sample(10, replace = TRUE))
#' x$let <- letters[x$dat]
#' genericDists(x["dat"])
#' genericDists(x["let"], strings = TRUE)
genericDists <- function(dat1, dat2 = NULL, strings = FALSE){
	dat1 <- cleanString(dat1)

  if (!is.null(dat2))
	{
    names(dat1) <- "var"
	  dat1 <- rbind(dat1, setNames(cleanString(dat2), "var"))
	}

  # String distance matching
  if (strings)
  {
    dists <- adist(dat1[[1]])
  }
  # General exact matches
  else
  {
    n <- seq_len(nrow(dat1))
    id <- expand.grid(n, n)
    compare1 <- dat1[id[, 1], ,drop = FALSE]
    compare2 <- dat1[id[, 2], ,drop = FALSE]
    dists <- matrix(rowSums(compare1 != compare2), ncol=length(n))
    diag(dists) <- 0
  }
	if (sum(dists > 0, na.rm = TRUE)){
	  dists <- dists/max(dists, na.rm = TRUE)
	}
  return(dists)
}
