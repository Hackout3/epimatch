#' epimatch.
#'
#' Find matching patient records across tabular datasets
#'
#' @description This package provides an interactive way to visualize
#'   potentially duplicated records across tabular data sets by calculating
#'   dissimilarity scores on user-specified columns in the data.
#'
#' @section Running the User Interface: The user interface can be invoked with
#'   the function \code{\link{launch}}. This will launch the app in your
#'   browser.
#'
#' @section Backend: The backend to the user interface is a modular set of
#'   functions that can calculate dissimilarity scores on any column(s) of the
#'   data. Once dissimilarity scores are calculated, they are given weights
#'   based on importance, summed, and scaled from zero to one. This resulting
#'   matrix is traversed, and indices below the given threshold are returned.
#'
#' @section Wrapper Functions:
#'
#'   The wrapper functions provide a way to programmatically execute the
#'   distance functions on the data. They retun a list of matrices and a list of
#'   matching indices, respectively.
#'
#'   \itemize{
#'      \item \code{\link{processFunctionList}}
#'      \item \code{\link{matchEpiData}}
#'      \item \code{\link{tablesFromMatch}}
#'   }
#'
#' @section Dissimilarity Functions: Each dissimilarity function returns a
#'   distance matrix scaled from 0 to 1 where 0 indicates a perfect match and 1
#'   indicates no match. The following distances are available:
#'
#' \itemize{
#'   \item      \code{\link{ageDists}}
#'   \item     \code{\link{dateDists}}
#'   \item   \code{\link{genderDists}}
#'   \item  \code{\link{genericDists}}
#'   \item \code{\link{locationDists}}
#'   \item     \code{\link{nameDists}}
#' }
#'
#' @section Matrix Summary Functions:
#'
#'   Once matrices are computed and stored in a list, they have weights applied,
#'   and are summed. When summing, missing values are given a custom defined
#'   weight (default 0.5). The following functions work with the matrices:
#'
#'   \itemize{
#'      \item \code{\link{collapseDistMatrices}}
#'      \item \code{\link{returnMatches}}
#'    }
#'
#' @name epimatch
#' @docType package
NULL
