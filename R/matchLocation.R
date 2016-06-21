#test <- data.frame(country=c("Uganda", "Uganda", "Kenya"),
#                   city=c("Kampala", "Kampala", "Kilifi"), stringsAsFactors = F)

# TODO: match even if fields out of order
# TODO: split fields input by comma into columns
locationDists <- function(dat){
  cleaned <- apply(dat, 2, tolower)

  # This returns a matrix with rowwise comparisons,
  # elements are the fraction of matching fields
  n <- seq_len(nrow(cleaned))
  id <- expand.grid(n, n)
  dists <- matrix(rowSums(cleaned[id[,1],] == cleaned[id[,2],]), ncol=length(n))
  diag(dists) <- 0

  dists <- dists/ncol(cleaned)

  return(dists)
}
