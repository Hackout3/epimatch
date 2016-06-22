#' return matching indices
#'
#' @param the number of rows for both datasets (ordered as they have been for all functions) and a distance matrix
#'  thresh: distance threshold (smaller means more conservative matches)
#' @return list where each index are the row indices for data rows in dataset 1 (d1),
#' and 2 (d2) that correspond to the same person
#' @export
#'
#' @examples
returnMatches <- function(nRowD1, nRowD2, distMatrix, thresh){
  rowCount <- 1
  numMatchClust <- 0
  distMatrix[upper.tri(distMatrix, diag= TRUE)] <- NA
  matchIndices <- list()
  numDistRow <- nrow(distMatrix)
  while(any(distMatrix < thresh, na.rm = TRUE) && rowCount <= numDistRow){
    indices <- which(distMatrix[, rowCount] < thresh)
    if(length(indices) >= 1){
      numMatchClust <- numMatchClust + 1
      matchIndices[[numMatchClust]] <- list()
      if(!missing(nRowD2)){ #dealing with 2 datasets?
          indices1 <- indices[which(indices <= nRowD1)]
          indices2 <- indices[which(indices > nRowD1)]
          #must also "tack on" the row index for this while loop:
          if(rowCount <= nRowD1){
            indices1 <- c(rowCount, indices1)
          }else{
            indices2 <- c(rowCount, indices2)
          }
          matchIndices[[numMatchClust]]$d1 <- indices1
          if(length(indices2 > 0)){
            #must re-adjust indices to start at one for dataset 2
            matchIndices[[numMatchClust]]$d2 <- indices2 - nrow(nRowD2)
          }else{ #just have this be blank like indices2 is
            matchIndices[[numMatchClust]]$d2 <- indices2
        }
      }else{
        matchIndices[[numMatchClust]]$d1 <- c(rowCount, indices)
      }
      #NA these out so that we don't compare them again
      distMatrix[indices, indices] <- NA
    }
    rowCount <- rowCount + 1
 }
  return(matchIndices)
}
