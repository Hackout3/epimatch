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

#' Collapse columns of a data frame into one
#'
#' @param df a data frame with one or more columns
#' @param sep a separator to collapse the columns with
#'
#' @return a single column data frame
#' @keywords internal
#' @noRd
#'
#' @examples
#' dat <- data.frame(a = letters, b = sample(100, 26), c = LETTERS)
#' collapseValues(dat)
collapseValues <- function(df, sep = "_"){
  if (!is.data.frame(df)){
    stop("a data frame is needed")
  }
  if (length(df) > 1){
    dfnames <- paste(names(df), collapse = sep)
    df <- data.frame(apply(df, 1, paste, collapse = sep), stringsAsFactors = FALSE)
    names(df) <- dfnames
  }
  return(df)
}
