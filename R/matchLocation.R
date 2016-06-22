#test <- data.frame(country=c("Uganda", "Uganda", "Kenya"),
#                   city=c("Kampala", "Kampala", "Kilifi"), stringsAsFactors = F)
#test2 <- data.frame(location=c("uganda,kampala", "uganda,kampala", "kenya"),
#stringsAsFactors = F)

# Returns a distance matrix with proportion of matched fields
locationDists <- function(a, b){
  # split commas
  split_a <- splitComma(a)
  split_b <- splitComma(b)

  # convert to lower case, remove whitespace
  clean_a <- cleanString(split_a)
  clean_b <- cleanString(split_b)

  # This returns a matrix with rowwise comparisons,
  # elements are the fraction of matching fields
  # No longer vectorised, sorry
  mat <- matrix(0, nrow=nrow(clean_a), ncol=nrow(clean_b))
  poss <- min(ncol(clean_a), ncol(clean_b))
  for (i in 1:(nrow(clean_a)-1))
  {
    for (j in (i+1):nrow(clean_b))
    {
      mat[i,j] <- sum(clean_a[i,] %in% clean_b[j,])/poss
      mat[j,i] <- mat[i,j]
    }
  }

  return(mat)
}

# Lower case and whitespace trimmed strings
cleanString <- function(dat)
{
  clean_dat <- apply(dat, 2, tolower)
  clean_dat <- apply(clean_dat, 2, trimws)
  return(clean_dat)
}

# Split a single column with commas into a matrix with multiple columns
splitComma <- function(dat)
{
  if(ncol(dat) == 1)
  {
    new_dat <- strsplit(dat[,1], ",")
    new_dat <- do.call(rbind, new_dat)
    for(i in 1:nrow(new_dat))
    {
      if(any(duplicated(new_dat[i,])))
      {
        new_dat[i,which(duplicated(new_dat[i,]))] <- NA
      }
    }
  }
  return(new_dat)
}
