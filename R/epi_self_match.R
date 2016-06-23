#' Find duplicates in single data set
#'
#'
#' @inheritParams process_matching
#' @details this function will take in one or two data sets and also a
#'
#' @return something
#' @export
#'
#' @examples
#' indata <- system.file("data", package = "epimatch")
#' indata <- dir(indata, full.names = TRUE)
#' x <- lapply(indata, read.csv, stringsAsFactors = FALSE)
#'
#' epi_self_match(dat1 = x[[1]],
#'                  dat2 = NULL,
#'                  funlist = list(
#'                    ID = list(d1vars = "Outbreak.ID.",
#'                              d2vars = NULL,
#'                              fun = "nameDists",
#'                              extraparams = NULL,
#'                              weight = 0.5),
#'                    names = list(d1vars = "Name..as.given.",
#'                                 d2vars = NULL,
#'                                 fun = "nameDists",
#'                                 extraparams = NULL,
#'                                 weight = 0.5)
#'                    ),
#'                  thresh = 0.5)
epi_self_match <- function(dat1, dat2 = NULL, funlist = list(), thresh = 0.05){
  the_matrices <- process_matching(dat1, dat2, funlist)
  the_weights  <- unlist(lapply(funlist, function(i) i$weights))
  MASTER_MAT   <- collapseDistMatrices(the_matrices, the_weights)
  out          <- returnMatches(nrow(dat1), nrow(dat2), MASTER_MAT, thresh)
  return(out)
}
