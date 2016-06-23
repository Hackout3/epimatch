#test <- data.frame(country=c("Uganda", "Uganda", "Kenya"),
#                   city=c("Kampala", "Kampala", "Kilifi"), stringsAsFactors = F)
#test2 <- data.frame(location=c("uganda,kampala", "uganda,kampala", "kenya"),
#stringsAsFactors = F)

# Returns a distance matrix with proportion of matched fields
#' Compare locations
#'
#' @param dat1 data frame containing location information
#' @param dat2 data frame containing location information
#'
#' @return a distance matrix
#' @export
#'
#' @examples
#' test <- data.frame(country=c("Uganda", "Uganda", "Kenya"),
#'                    city=c("Kampala", "Kampala", "Kilifi"), stringsAsFactors = F)
#' test2 <- data.frame(location=c("uganda,kampala", "uganda,kampala", "kenya"),
#' stringsAsFactors = F)
#' locationDists(test, test2)
locationDists <- function(dat1, dat2 = NULL){
  # split commas
  split_a <- splitComma(dat1)

  # convert to lower case, remove whitespace
  clean_a <- cleanString(split_a)

  # Combine data frames
  if (!is.null(dat2))
  {
    # Cleaning of set 2
    split_b <- splitComma(dat2)
    clean_b <- cleanString(split_b)

    if (ncol(clean_a) > ncol(clean_b))
    {
      padding <- matrix(nrow = nrow(clean_b), ncol = ncol(clean_a) - ncol(clean_b))
      clean_b <- cbind(clean_b, padding)
    }
    else if (ncol(clean_a) < ncol(clean_b))
    {
      padding <- matrix(nrow = nrow(clean_a), ncol = ncol(clean_b) - ncol(clean_a))
      clean_a <- cbind(clean_a, padding)
    }
    colnames(clean_a) <- colnames(clean_b)
    combined <- rbind(clean_a, clean_b)
  }
  else
  {
    combined <- clean_a
  }

  # This returns a matrix with rowwise comparisons,
  # elements are the fraction of matching fields
  # No longer vectorised, sorry
  mat <- matrix(0, nrow=nrow(combined), ncol=nrow(combined))
  poss <- min(ncol(clean_a), ncol(clean_b))
  for (i in 1:(nrow(combined)-1))
  {
    for (j in (i+1):nrow(combined))
    {
      dists <- adist(combined[i,], combined[j,]) # Distances between all columns
      dists <- dists[rowSums(is.na(dists))!=ncol(dists),] # Remove NAs

      # Return the closest match
      if (!is.null(nrow(dists))) # This happens when only one row is left
      {
        mat[i,j] <- sum(apply(dists, 1, min, na.rm=T))/poss
      }
      else
      {
        mat[i,j] <- sum(min(dists))/poss
      }
      mat[j,i] <- mat[i,j]
    }
  }

  return(mat/max(mat, na.rm = TRUE))
}

# test result should be
# locationDists(test, test2)
#[,1] [,2] [,3] [,4] [,5] [,6] [,7]
#[1,]  0.0  0.0  4.5  0.0  0.0  4.5    0
#[2,]  0.0  0.0  4.5  0.0  0.0  4.5    0
#[3,]  4.5  4.5  0.0  5.0  5.0  2.5    5
#[4,]  0.0  0.0  5.0  0.0  0.0  4.5    0
#[5,]  0.0  0.0  5.0  0.0  0.0  4.5    0
#[6,]  4.5  4.5  2.5  4.5  4.5  0.0    2
#[7,]  0.0  0.0  5.0  0.0  0.0  2.0    0

