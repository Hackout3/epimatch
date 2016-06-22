#'
#'
#' @param two datasets (ordered as they have been for all functions) and a distance matrix
#'  thresh: distance threshold (smaller means more conservative matches)
#' @return list where each index are the row indices for data rows in that cluster,
#' and clusters are ordered from the highest to lowest match ranking
#' @export index list of matches
#'
#' @examples

returnMatches <- function(d1, d2, distMatrix, thresh){
  rowCount <- 1
  numMatchClust <- 1
  distMatrix[upper.tri(distMatrix, diag= TRUE)] <- NA
  matchIndices <- list()
  while(any(distMatrix) < thresh){
    indices <- which(distMatrix[, rowCount] > thresh)
    if(length(indices) > 1){
      numMatchClust <- numMatchClust + 1
      if(!is.missing(d2)){
        if(rowCount <= nrow(d1)){
          indices1 <- indices[which(indices <= nrow(d1))]
          indices2 <- indices[which(indices > nrow(d1))]
         # matches <- rbind(d1[c(rowCount, indices1), ], d2[(indices2- nrow(d2)), ])
          matchIndices[[numMatchClust]]$d1 <- indices1
          #must re-adjust indices to start at one for dataset 2
          matchIndices[[numMatchClust]]$d2 <- indices2 - nrow(d2)
        }else{
          #matches <- d2[c(rowCount, indices), ]
          matchIndices[[numMatchClust]] <- indices
        }
      }else{
        #matches <- d2[c(rowCount, indices), ]
        matchIndices[[numMatchClust]] <- indices
      }
      #NA these out so that we don't compare them again
      distMatrix[indices, indices] <- NA
    }
    rowCount <- rowCount + 1
 }
  return(matchIndices)
}
