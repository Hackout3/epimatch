#' return matching indices
#'
#' @param the number of rows for both datasets (ordered as they have been for all functions) and a distance matrix
#'  thresh: distance threshold (smaller means more conservative matches)
#' @return list where each index are the row indices for data rows in dataset 1 (d1),
#' and 2 (d2) that correspond to the same person
#' @export
#'
#' @examples
#' caseIDVector <- c("AB-10-1", "AB-10-5", "AB-10_1")
#' distMatrix <- adist(c(caseIDVector, caseIDVector), c(caseIDVector, caseIDVector))
#' returnMatches(3, 3, distMatrix, thresh = 0.5)
returnMatches <- function(nRowD1, nRowD2, distMatrix, thresh){
  rowCount <- 1
  distMatrix[upper.tri(distMatrix, diag= TRUE)] <- NA
  numDistRow <- nrow(distMatrix)
  matchIndices <- vector("list", numDistRow)
  while(any(distMatrix < thresh, na.rm = TRUE) && rowCount <= numDistRow){
    indices <- which(distMatrix[, rowCount] < thresh)
    scoreSum <- sum(distMatrix[indices, rowCount], na.rm = TRUE)
    if(length(indices) >= 1){
      if(!missing(nRowD2)){ #dealing with 2 datasets?
          matchIndices[[rowCount]] <- vector("list",2)
          names(matchIndices[[rowCount]]) <- c("d1", "d2")
          names(matchIndices)[rowCount] <- scoreSum
          indices1 <- indices[which(indices <= nRowD1)]
          scores1 <- distMatrix[indices1, rowCount]
          indices2 <- indices[which(indices > nRowD1)]
          scores2 <- distMatrix[indices2, rowCount]
          #must also "tack on" the row index for this while loop:
          if(rowCount <= nRowD1){
            indices1 <- c(rowCount, indices1)
            scores1 <- c(0, scores1)
          }else{
            indices2 <- c(rowCount, indices2)
            scores2 <- c(0, scores2)
          }
          names(scores1) <- indices1
          matchIndices[[rowCount]][[1]] <- scores1
          if(length(indices2) > 0){
            #must re-adjust indices to start at one for dataset 2
            names(scores2) <- indices2 - nRowD2
            matchIndices[[rowCount]][[2]] <- scores2
          }else{ #just have this be blank like indices2 is
            names(scores2) <- indices2
            matchIndices[[rowCount]][[2]] <- scores2
        }
      }else{
        scores <- c(0, distMatrix[indices, rowCount])
        matchIndices[[rowCount]] <- vector("list",1)
        names(matchIndices)[rowCount] <- scoreSum
        names(scores) <-  c(rowCount, indices)
        matchIndices[[rowCount]][[1]] <- scores
        names(matchIndices[[rowCount]]) <- c("d1")
      }
      #NA these out so that we don't compare them again
      distMatrix[indices, indices] <- NA
    }
    rowCount <- rowCount + 1
  }
  matchIndices <- matchIndices[!is.na(names(matchIndices))]
  #order with lowest scoreSum (closest match) first
  matchIndices <- matchIndices[sort.int(as.numeric(names(matchIndices)),
                                                   decreasing = FALSE,
                                                   index.return= TRUE)$ix]
  return(matchIndices)
}
