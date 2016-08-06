#' Generate a list of tables aggregating the matched values
#'
#' @inheritParams processFunctionList
#' @param matchList a list derived from \code{\link{matchEpiData}}. If this is
#'   \code{NULL}, a threshold can be supplied instead to calculate the list on
#'   the fly.
#' @param collapse When \code{TRUE}, the list of data frames will be collapsed
#'   into one data frame an extra column specifying the group appended.
#'
#' @details This will collect all of the data from \code{\link{matchEpiData}}
#'   and present it in table format. It will collapse them into tidy tables and,
#'   if provided, the score of the matches will be provided.
#' @return a list of data frames sorted in decreasing order of matching-ness
#'   with three extra columns:
#'   \describe{
#'       \item{source}{ data set of origin}
#'       \item{index}{ index in data set}
#'       \item{score}{ the score of the matches to the first item in the group.}
#'   }
#'   When \code{collapse = TRUE}, a fourth column, \code{groups} is appended.
#'
#' @export
#'
#' @examples
#' ## Loading Data
#'
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
#'                     giveWeight = TRUE)
#' tablesFromMatch(case, lab, funlist, matchList = res, collapse = FALSE)
#' tablesFromMatch(case, lab, funlist, matchList = 0.25)
tablesFromMatch <- function(dat1, dat2 = NULL, funlist = list(),
                            matchList = NULL, collapse = TRUE){

  # Check if incoming data are cromulent
  dat2exists <- !is.null(dat2) && is.data.frame(dat2)
  if (!is.data.frame(dat1) | !dat2exists){
    stop("data must be in data frame format")
  }
  # Check if there are any matches at all
  if (is.null(matchList) || length(matchList) == 0){
    stop("no matches found!")
  }
  # If the matchList is a threshold parameter, pass it to the matchEpiData
  # function.
  if (is.numeric(matchList) && length(matchList) == 1){
    matchList <- matchEpiData(dat1, dat2, funlist = funlist, thresh = matchList,
                              giveWeight = TRUE)
  }
  # If the incoming list contains numeric values, collect the thresholds and
  # convert the list to indices.
  if (!is.integer(unlist(matchList))){
    theThresholds <- matchList
    matchList     <- getIndexList(matchList)
  }

  # Grab the tested variables from dat1
  d1vars <- lapply(funlist, "[[", "d1vars")
  d1 <- lapply(d1vars, function(i) dat1[i])
  # Since multiple columns can be evaluated by a single function, we are going
  # to collapse these columns into one.
  d1 <- data.frame(lapply(d1, collapseValues))

  if (dat2exists){
    # Grab the tested variables from dat2 and rename them based off of dat1.
    # This allows us to bind the data together a lot easier.
    d2vars <- lapply(funlist, "[[", "d2vars")
    d2 <- lapply(d2vars, function(i) dat2[i])
    d2 <- data.frame(lapply(d2, collapseValues))
    names(d2) <- names(d1)
  }

  ## The output is a list of data frames subset in decreasing order of matching.
  outlist <- vector(length = length(matchList), mode = "list")
  for (i in seq(matchList)){
    indices <- matchList[[i]]
    if (dat2exists){
      outdat <- rbind(d1[indices$d1, ], d2[indices$d2, ])
    } else {
      outdat <- d1[indices$d1, ]
    }
    ## Give the indices within each data set ------------------------
    outdat$index <- unlist(indices, use.names = FALSE)

    ## Give the source of the data ----------------------------------
    outdat$source <- rep(c("d1", "d2"), lapply(indices, length))

    ## If the incoming list are thresholds, add them as a column ----
    if (exists("theThresholds", inherits = FALSE)){
      outdat$score <- unlist(theThresholds[[i]])
    }
    outlist[[i]] <- outdat
  }

  if (collapse){
    # This is where we make the data tidy by placing everything in a single data
    # frame and adding three extra columns that contain the information that was
    # in the result from `matchEpiData()`.

    ## Give the indices of the highest list -------------------------
    groups <- lapply(lapply(matchList, unlist), length)
    groups <- rep(seq_along(matchList), groups)

    ## Create the resulting tidy data frame and add information -----
    outlist <- do.call("rbind", outlist)
    outlist$groups  <- groups
  }

  return(outlist)
}
