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
#' d1 <- exactMatchCaseIDIntraDataset(caseIDVector)
#' d2 <- exactMatchCaseIDIntraDataset(caseIDVector)
#' summedDistMatrix <- collapseDistMatrices(d1, d2, c(0.5, 0.5))

collapseDistMatrices <- function(d1, d2, weightVector = rep.int(1/(nrow(d1) + nrow(d2))),
                                                            times = (nrow(d1) + nrow(d2))){

    if(any(is.na(d1))){
      d1[is.na(d1)] <- 0
    }
     if(any(is.na(d2))){
      d2[is.na(d2)] <- 0
     }
    d1 <- d1 + d2
  return(d1)
}
