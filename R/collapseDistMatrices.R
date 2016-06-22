#' Collapse/sum matrices using weights
#'
#' @param distance matrices 1 and 2 and a vector of corresponding weights
#' default: all equal weights for each input matrix
#' @return a single weighted distance matrix
#' @export
#'
#' @examples
#' just sum two of the same matrices:
#' caseIDVector <- c("AB-10-1", "AB-10-5", "AB-10_1")
#' dist1 <- exactMatchCaseIDIntraDataset(caseIDVector)
#' d2 <- exactMatchCaseIDIntraDataset(caseIDVector)
#' summedDistMatrix <- collapseDistMatrices(dist1, d2, c(0.5, 0.5))

collapseDistMatrices <- function(dist1, d2, weightVector = c(0.5, 0.5)){
    if(any(is.na(dist1))){
      dist1[is.na(dist1)] <- 0
    }
     if(any(is.na(d2))){
      d2[is.na(d2)] <- 0
     }
    dist1 <- dist1*(1-weightVector[1]) + d2*(1-weightVector[2])
  return(dist1)
}
