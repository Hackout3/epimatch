#'
#'
#' @param distance matrix
#'
#' @return list where each index are the row indices for data rows in that cluster,
#' and clusters are ordered from the highest to lowest match ranking
#' @export
#'
#' @examples

clusterDistMatrix <- function(distMatrix, numClust = 5){
  #number of clusters must be >= number of rows in dataset
  if(numClust > nrow(distMatrix)){
    numClust <- nrow(distMatrix)
  }
  hclustOut <- hclust(dist(distMatrix), method = "ward.D2")
  clustIDs <- cutree(hclustOut, k=numClust)
  return(clustIDs)
}
