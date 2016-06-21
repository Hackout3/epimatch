
#find matching case IDs within a single dataset
#test: caseIDVector <- c("AB-10-1", "AB-10-5", "AB-10_1")
exactMatchCaseIDIntraDataset <- function(caseIDVector){
  #replace all punctuation with a lowercase character.
  caseIDVector <- gsub("[[:punct:]][[:blank:]]+", "_", caseIDVector)
  #make all characters lowercase
  caseIDVector <- tolower(caseIDVector)
  return(adist(caseIDVector))
}

#input: two vectors of case IDs

# exactMatchCaseID <- function(caseIDVector1, caseIDVector2){
#   #replace all puncutation with a lowercase character.
#   caseIDVector1 <- gsub("[[:punct:]]", "_", caseIDVector1)
#   caseIDVector2 <- gsub("[[:punct:]]", "_", caseIDVector2)
#   matchMatrix <- matrix(data = FALSE, nrow = length(caseIDVector1),
#                         ncol = length(caseIDVector2))
#   return()
# }
