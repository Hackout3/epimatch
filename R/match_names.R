#' Clean and match names column by removing punctuation and replacing
#' punctuation with an underscore
#'
#' @param dat a vector or data frame
#'
#' @return a distance matrix giving the number of mismatches in names.
#' @export
#'
#' @examples
#' set.seed(9)
#' x <- data.frame(x = letters, y = LETTERS, z = 1:26)
#' x <- sample(nrow(x), 10, replace = TRUE)
#' match_names(x)
match_names <- function(dat){
  if (!is.vector(dat)){
    dat <- apply(x, 1, paste, collapse = " ")
  }
  # replace all punctuation and spaces with a single space
  dat <- gsub("[[:punct:][:blank:]]+", "_", dat)
  dat <- tolower(dat)
  return(adist(dat))
}
