#' Find duplicates in one or two data sets
#'
#' @inheritParams processFunctionList
#' @param thresh a threshold below which to consider two rows nearly identical.
#' @param giveWeight a logical parameter indicating whether or not the output
#'   should be a list of weights or indices (default).
#' @details this function will take in one or two data sets, a list of functions
#'   to apply to specific columns of the data set, and a threshold to determine
#'   what is a match. It will return a list from \code{\link{returnMatches}}
#'   where each element represents a different potential match. Within each
#'   element, there is a two-element list where each contains either indices or
#'   weights for each sample that matched below the threshold.
#'
#' @return something
#' @export
#'
#' @examples
#' ## Loading Data
#' library('epimatch')
#' indata <- system.file("files", package = "epimatch")
#' indata <- dir(indata, full.names = TRUE)
#' x <- lapply(indata, read.csv, stringsAsFactors = FALSE)
#' names(x) <- basename(indata)
#'
#' # We will use one data set from the case information and lab results
#' case <- x[["CaseInformationForm.csv"]]
#' lab <- x[["LaboratoryResultsForm7.csv"]]
#'
#' funlist <- list(
#'              list(d1vars = "ID",
#'                   d2vars = "ID",
#'                   fun = "nameDists",
#'                   extraparams = NULL,
#'                   weight = 1),
#'              list(d1vars = c("Surname", "OtherNames"),
#'                   d2vars = c("SurnameLab", "OtherNameLab"),
#'                   fun = "nameDists",
#'                   extraparams = NULL,
#'                   weight = 0.5)
#'            )
#' # This will get all of the indices that match the ID and Names with a
#' # threshold of 0.25
#' res <- matchEpiData(dat1 = case,
#'                     dat2 = lab,
#'                     funlist = funlist,
#'                     thresh = 0.25,
#'                     giveWeight = FALSE)
#' # List of indices
#' res
#'
#' tablesFromMatch(case, lab, funlist, matchList = res)
matchEpiData <- function(dat1, dat2 = NULL, funlist = list(), thresh = 0.05, giveWeight = FALSE){
  the_matrices <- processFunctionList(dat1, dat2, funlist)
  the_weights  <- unlist(lapply(funlist, function(i) i$weight))
  MASTER_MAT   <- collapseDistMatrices(the_matrices, the_weights)
  out          <- returnMatches(nrow(dat1), nrow(dat2), MASTER_MAT, thresh)
  # For processing in the shiny app, it's useful to display either the weights
  # as a named index vector or the indices themselves
  if (!giveWeight){
    out <- getIndexList(out)
  }
  return(out)
}
