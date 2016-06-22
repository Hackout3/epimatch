
#find matching case IDs within a single dataset
#test: caseIDVector <- c("AB-10-1", "AB-10-5", "AB-10_1")

#' match case ID given a vector of caseIDs
#'
#' @param caseIDVector a vector of caseIDs
#'
#' @return a distance matrix
#' @export
#'
#' @examples
#' caseIDVector <- c("AB-10-1", "AB-10-5", "AB-10_1")
#' exactMatchCaseIDIntraDataset(caseIDVector)
exactMatchCaseIDIntraDataset <- function(caseIDVector){
  #replace all punctuation with a lowercase character.
  caseIDVector <- gsub("[[:punct:]][[:blank:]]+", "_", caseIDVector)
  #make all characters lowercase
  caseIDVector <- tolower(caseIDVector)
  return(adist(caseIDVector))
}

