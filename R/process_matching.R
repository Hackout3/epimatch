#' Process the functions used for matching data
#'
#' Input one or two data sets, process them with a list of containing functions,
#' column names, and function parameters, and return a list of matrices by
#' function.
#'
#' @param dat1 An input linelist
#' @param dat2 An optional extra linelist
#' @param funlist A list containing lists containing:
#' \itemize{
#'  \item d1vars - variable names for dataset 1
#'  \item d2vars - variable names for dataset 2
#'  \item fun - function name to process on these variables
#'  \item extraparams - extra parameters that need to be applied with the function.
#'  \item weights - a weight vector to scale each matrix (not used in processFunctionList).
#' }
#'
#' @return a list of distance matrices scaled by weight
#'
#' @export
#'
#' @examples
#' indata <- system.file("files", package = "epimatch")
#' indata <- dir(indata, full.names = TRUE)
#' x      <- lapply(indata, read.csv, stringsAsFactors = FALSE)
#'
#' processFunctionList(dat1 = x[[1]],
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
#'                                 weight = 0.5),
#'                    list(d1vars = c("Age", "AgeUnit"),
#'                         d2vars = NULL,
#'                         fun = "ageDists",
#'                         extraparams = list(e = 5),
#'                         weight = 0.5)
#'                    )
#'                  )
#'
processFunctionList <- function(dat1, dat2 = NULL, funlist = list()){
  if (length(funlist) == 0){
    stop("Please provide a list for the funlist parameter")
  }
  if (!is.data.frame(dat1)){
    stop("input datasets must be data frames")
  }
  funs <- vapply(funlist, function(i) i$fun, "FUN!")
  if ("" %in% funs){
    badfun <- which(funs == "")
    badfunmsg <- paste("A function appears to be missing in the following fields:",
                       badfun, "\nPlease check your functions")
    stop(badfunmsg, call. = FALSE)
  }
  null_d1 <- vapply(funlist, function(i) is.null(i$d1vars), TRUE)
  if (any(null_d1)){
    msg <- paste("Please specify columns for the following fields for data set 1:",
                 which(null_d1))
    stop(msg, call. = FALSE)
  }
  if (!is.null(dat2)){
    null_d2 <- vapply(funlist, function(i) is.null(i$d2vars), TRUE)
    if (any(null_d2)){
      msg <- paste("Please specify columns for the following fields for data set 2:",
                   which(null_d2))
      stop(msg, call. = FALSE)
    }
  }
  out_matrices <- vector(mode = "list", length = length(funlist))
  for (i in seq(funlist)){
    paramlist <- funlist[[i]]
    fun       <- paramlist$fun
    d1 <- dat1[, paramlist$d1vars, drop = FALSE]
    d2 <- if (!is.null(dat2)) dat2[, paramlist$d2vars, drop = FALSE] else NULL
    out_matrices[[i]] <- do.call(fun, c(list(d1, d2), paramlist$extraparams))
  }
  if (!is.null(names(funlist))){
    names(out_matrices) <- names(funlist)
  }
  return(out_matrices)
}
