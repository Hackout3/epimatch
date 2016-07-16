#' @importFrom stats dist
#' @importFrom utils adist
#' @importFrom utils read.csv
#' @importFrom stats setNames
clean_age <- function(dat, extra_column = list(mo = "month", yr = "year", day = "day")){
  if (ncol(dat) > 1 && !is.null(extra_column)){
    dat[[2]] <- trimws(tolower(dat[[2]]))
    ages <- ifelse(dat[[2]] %in% tolower(extra_column$mo), dat[[1]]*(1/12),
                   ifelse(dat[[2]] %in% tolower(extra_column$day), dat[[1]]*(1/365),
                          dat[[1]]))
  } else {
    ages <- dat[[1]]
  }
  return(ages)
}

loadTestSets <- function(){
  indata <- system.file("files", package = "epimatch")
  indata <- dir(indata, full.names = TRUE, pattern = "^test")
  x      <- lapply(indata, read.csv, stringsAsFactors = FALSE)
  names(x) <- c("test1", "test2")
  return(x)
}

# returns a vector of names
clean_names <- function(dat){
  if (ncol(dat) > 1){
    dat <- apply(dat, 1, paste, collapse = " ")
  } else {
    dat <- dat[[1]]
  }
  # replace all punctuation and spaces with a single space
  dat <- gsub("[[:punct:][:blank:]]+", "_", dat)
  dat <- tolower(dat)
  return(dat)
}



# This is duplicated - not how to share these functions in an R package?
# Is this done automatically?
cleanString <- function(dat)
{
  clean_dat <- apply(dat, 2, tolower)
  clean_dat <- apply(clean_dat, 2, trimws)
  return(data.frame(clean_dat, stringsAsFactors = FALSE))
}

# Split a single column with commas into a matrix with multiple columns
splitComma <- function(dat)
{
  if(ncol(dat) == 1)
  {
    new_dat <- strsplit(dat[,1], ",")
    new_dat <- do.call(rbind, new_dat)
    for(i in 1:nrow(new_dat))
    {
      if(any(duplicated(new_dat[i,])))
      {
        new_dat[i,which(duplicated(new_dat[i,]))] <- NA
      }
    }
  } else
  {
    return(dat)
  }
  return(new_dat)
}


#' return a list of indices from matchEpiData
#'
#' The output of matchEpiData can either be a list of indices or a named list of
#' weights. If it's a named list of weights, this function will return the list
#' of indices.
#'
#' @param matchList a list of lists of weights.
#'
#' @return an unnamed list of lists of indices
#' @keywords internal
#' @examples
#' ## Loading Data
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
#' res               # List of weights
#' getIndexList(res) # List of indices
getIndexList <- function(matchList){
  out <- lapply(matchList, lapply, function(i) as.integer(names(i)))
  names(out) <- NULL
  return(out)
}
