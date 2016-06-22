#'
#'
#' @param two datasets (ordered as they have been for all functions) and a distance matrix
#'  thresh: distance threshold (smaller means more conservative matches)
#'  #assumes the two datasets have matching, ordered columns (even if some are
#'  #padded with NAs to make this match)
#' @return list where each index are the row indices for data rows in that cluster,
#' and clusters are ordered from the highest to lowest match ranking
#' @export
#'
#' @examples

returnMatches <- function(d1, d2, distMatrix, thresh){
  rowCount <- 1
  distMatrix[upper.tri(distMatrix, diag= TRUE)] <- NA
  while(any(distMatrix) < thresh){
    indices <- which(distMatrix[, 1] > thresh)
    if(length(indices) > 1){
      if(!is.missing(d2)){
        if(rowCount <= nrow(d1)){
          indices1 <- indices[which(indices <= nrow(d1))]
          indices2 <- indices[which(indices > nrow(d1))]
          matches <- rbind(d1[c(rowCount, indices1), ], d2[indices2, ] )
        }else{

        }
      }else{
        matches <- rbind(d1[c(rowCount, indices)])
      }
    }else{
      #NA out first row
      distMatrix[, 1] <- NA
    }
    rowCount <- rowCount + 1
 }

}
