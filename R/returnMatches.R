#'
#'
#' @param the number of rows for both datasets (ordered as they have been for all functions) and a distance matrix
#'  thresh: distance threshold (smaller means more conservative matches)
#' @return list where each index are the row indices for data rows in that cluster,
#' and clusters are ordered from the highest to lowest match ranking
#' @export index list of matches
#'
#' @examples

returnMatches <- function(nRowD1, nRowD2, distMatrix, thresh){
  rowCount <- 1
  numMatchClust <- 1
  distMatrix[upper.tri(distMatrix, diag= TRUE)] <- NA
  matchIndices <- list()
  while(any(distMatrix) < thresh){
    indices <- which(distMatrix[, rowCount] > thresh)
    if(length(indices) > 1){
      numMatchClust <- numMatchClust + 1
      if(!is.missing(nRowD2)){
        if(rowCount <= nRowD1){
          indices1 <- indices[which(indices <= nRowD1)]
          indices2 <- indices[which(indices > nRowD1)]
          matchIndices[[numMatchClust]]$nRowD1 <- indices1
          #must re-adjust indices to start at one for dataset 2
          matchIndices[[numMatchClust]]$nRowD2 <- indices2 - nrow(nRowD2)
        }else{
          matchIndices[[numMatchClust]] <- indices
        }
      }else{
        matchIndices[[numMatchClust]] <- indices
      }
      #NA these out so that we don't compare them again
      distMatrix[indices, indices] <- NA
    }
    rowCount <- rowCount + 1
 }
  return(matchIndices)
}
