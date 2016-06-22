#' Clean and match names column by removing punctuation and replacing
#' punctuation with an underscore
#'
#' @param dat1 data frame containing name information
#' @param dat2 data frame containing name information
#'
#' @return a distance matrix giving the number of mismatches in names.
#' @export
#'
#' @examples
#' set.seed(9)
#' x <- data.frame(x = letters, y = LETTERS, z = 1:26)
#' x <- sample(nrow(x), 10, replace = TRUE)
#' match_names(x)
match_names <- function(dat1, dat2 = NULL){
  if (!is.null(dat2))
  {
    combined <- c(clean_names(dat1), clean_names(dat2))
  }
  else
  {
    combined <- clean_names(dat1)
  }

  datdist <- adist(combined)
  res <- datdist/max(datdist)
  return(res)
}

# returns a vector of names
clean_names <- function(dat){
  if (ncol(dat) > 1){
    dat <- apply(x, 1, paste, collapse = " ")
  } else {
    dat <- dat[[1]]
  }
  # replace all punctuation and spaces with a single space
  dat <- gsub("[[:punct:][:blank:]]+", "_", dat)
  dat <- tolower(dat)
  return(dat)
}
