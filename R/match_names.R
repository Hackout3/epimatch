#' Clean and match names column by removing punctuation and replacing
#' punctuation with an underscore
#'
#' @param a data frame containing name information
#' @param b data frame containing name information
#'
#' @return a distance matrix giving the number of mismatches in names.
#' @export
#'
#' @examples
#' set.seed(9)
#' x <- data.frame(x = letters, y = LETTERS, z = 1:26)
#' x <- sample(nrow(x), 10, replace = TRUE)
#' match_names(x)
match_names <- function(a, b){
  combined <- rbind(clean_names(a), clean_names(b))
  datdist <- adist(combined)
  res <- datdist/max(datdist)
  return(res)
}

clean_names <- function(dat){
  if (!is.vector(dat)){
    dat <- apply(x, 1, paste, collapse = " ")
  }
  # replace all punctuation and spaces with a single space
  dat <- gsub("[[:punct:][:blank:]]+", "_", dat)
  dat <- tolower(dat)
}
