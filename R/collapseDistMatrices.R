#' Collapse/sum matrices using weights
#'
#' @param distMatrixList list of distances matrices and a vector of
#'   corresponding weights default: all equal weights for each input matrix
#' @param weightVector a vector of weights for each matrix. If \code{NULL}, then
#'   each matrix will be weighted equally.
#' @return a single weighted distance matrix
#' @export
#'
#' @examples
#' # just sum two of the same matrices:
#' caseIDVector <- data.frame(c("AB-10-1", "AB-10-5", "AB-10_1"))
#' m1 <- nameDists(caseIDVector)
#' m2 <- nameDists(caseIDVector)
#' summedDistMatrix <- collapseDistMatrices(list(m1, m2), c(0.5, 0.5))

collapseDistMatrices <- function(distMatrixList,weightVector = NULL){
  matlen <- length(distMatrixList)
  if(is.null(weightVector) || length(weightVector != matlen)){
    weightVector <- rep.int(1/(matlen), times = matlen)
  }
  weightVector <- 1 - weightVector + (.Machine$double.eps)^0.5
  for(m in 1:matlen){
    #replace NAs with zeros
    missing_data <- is.na(distMatrixList[[m]])
    if(any(missing_data)){
      distMatrixList[[m]][missing_data] <- 0
    }
    if (m == 1){
      finalMatrix <- distMatrixList[[m]]*weightVector[m]
    } else {
      finalMatrix <- finalMatrix + (distMatrixList[[m]]*weightVector[m])
    }
  }
  return(finalMatrix)
}
