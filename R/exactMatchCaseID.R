
#find matching case IDs within a single dataset
#test: caseIDVector <- c("AB-10-1", "AB-10-5", "AB-10_1")

#' match case ID given a vector of caseIDs
#'
#' @param dat1 a vector of caseIDs
#' @param dat2 a vector of caseIDs
#'
#' @return a distance matrix
#' @export
#'
#' @examples
#' caseIDVector <- c("AB-10-1", "AB-10-5", "AB-10_1")
#' exactMatchCaseIDIntraDataset(caseIDVector)
exactMatchCaseIDIntraDataset <- function(dat1, dat2=NULL){
  if (!is.null(dat2))
  {
    combined <- rbind(cleanID(dat1), cleanID(dat2))
  }
  else
  {
    combined <- cleanID(dat1)
  }
  return(adist(combined))
}

cleanID <- function(dat){
  #replace all punctuation with a lowercase character.
  #note: adding [[:blank:]] resulted it it not finding all of the
  #special punctuation
  caseIDVector <- gsub("[[:punct:]]+", "_", dat)
  #make all characters lowercase
  caseIDVector <- tolower(caseIDVector)
  return(caseIDVector)
}
