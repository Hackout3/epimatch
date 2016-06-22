#' Collapse/sum matrices using weights
#'
#' @param list of distances matrices and a vector of corresponding weights
#' default: all equal weights for each input matrix
#' @return a single weighted distance matrix
#' @export
#'
#' @examples
#' just sum two of the same matrices:
#' caseIDVector <- c("AB-10-1", "AB-10-5", "AB-10_1")
#' m1 <- exactMatchCaseIDIntraDataset(caseIDVector)
#' m2 <- exactMatchCaseIDIntraDataset(caseIDVector)
#' summedDistMatrix <- collapseDistMatrices(list(m1, m2), c(0.5, 0.5))

collapseDistMatrices <- function(distMatrixList, weightVector = rep.int(1/(length(distMatrixList)),
                                                            times = length(distMatrixList))){
  for(m in 1:length(distMatrixList)){
    #replace NAs with zeros
    if(any(is.na(distMatrixList[[m]]))){
      distMatrixList[[m]][is.na(distMatrixList[[m]])] <- 0
    }
    if(m==1){
      finalMatrix <- distMatrixList[[m]]*(1-weightVector[m])
    }else{
      finalMatrix <- finalMatrix + distMatrixList[[m]]*(1-weightVector[m])
    }
  }
  return(finalMatrix)

}
