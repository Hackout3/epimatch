
#' Collapse/sum matrices using weights
#'
#' @param distMatrixList list of distances matrices and a vector of
#'   corresponding weights default: all equal weights for each input matrix
#' @param weightVector a vector of weights for each matrix. If \code{NULL}, then
#'   each matrix will be weighted equally.
#' @param NAweight a custom parameter specifying the score to be assigned for
#'   missing data.
#' @return a single weighted distance matrix
#' @description This function collapses/sums the individual matrices from
#' different feature columns together.
#'
#' Right now, the weights are not actually implemented (multiplying each matrix
#' by 1 minus its weight did not give distances that clearly stratified between
#' true and false patient matches.)
#'
#' The matrices are simply summed together and then divided by the total number
#' of matrices fed in to provide a single output distance matrix, where each
#' index is on a scale of 0 to 1, with 0 being a perfect match (zero distance
#' between the two rows.)
#' @export
#'
#' @examples
#' # just sum two of the same matrices:
#' caseIDVector <- data.frame(c("AB-10-1", "AB-10-5", "AB-10_1"))
#' m1 <- nameDists(caseIDVector)
#' m2 <- nameDists(caseIDVector)
#' summedDistMatrix <- collapseDistMatrices(list(m1, m2), c(0.5, 0.5))
collapseDistMatrices <- function(distMatrixList,weightVector = NULL,
                                 NAweight = 0.5){
  matlen <- length(distMatrixList)
  if(is.null(weightVector) || length(weightVector != matlen)){
    weightVector <- rep.int(1/(matlen), times = matlen)
  }
  #don't want a weight vector of zeros if they are initially all one
  #this would result in all distances being zero.
  weightVector <- 1 - weightVector + (.Machine$double.eps)^0.5
  numMatrices <- length(distMatrixList)
  for(m in 1:matlen){
    distMatrixList[[m]] <- as.matrix(distMatrixList[[m]])
    if(length(dim(distMatrixList[[m]])) != 2){
      numMatrices <-  numMatrices -1
      dM <- 0 #just make zero in case m==1; can add to other matrices later then.
      warning(paste0("Distance matrix number ", m , " is not two-dimensional in the collapseDistMatrices function."))
    }else{  #replace NAs with zeros
      dM <- distMatrixList[[m]]
      missing_data <- is.na(dM)
      #set internal penalty of 0.5 for a missing value.
      if(any(missing_data)){
        dM[missing_data] <- NAweight
      }
    }
    if (m == 1){
      finalMatrix <- dM #*weightVector[m]
    } else {
      finalMatrix <- finalMatrix + (dM) #*weightVector[m])
    }
  }
  if(numMatrices >0){
    finalMatrix <- finalMatrix/numMatrices
  }else{
    warning("No distance matrices found in collapseDistMatrices function.")
  }
  return(finalMatrix)
}
