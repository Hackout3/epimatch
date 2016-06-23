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
#' generic_match(1:3)
genericDists <- function(dat1, dat2=NULL, strings=F){
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
    dists <- matrix(rowSums(dat1[id[,1],] == dat1[id[,2],]), ncol=length(n))
    diag(dists) <- 0
  }
  return(dists/max(dists))
}

# This is duplicated - not how to share these functions in an R package?
# Is this done automatically?
cleanString <- function(dat)
{
  clean_dat <- apply(dat, 2, tolower)
  clean_dat <- apply(clean_dat, 2, trimws)
  return(data.frame(clean_dat, stringsAsFactors = F))
}
