#' Create distance from age column
#'
#' @param dat a vector containing ages in years, or, if a data frame, the first
#'   column contains the ages and the second contains the specifier of type of
#'   age (months or years).
#' @param extra_column a named vector specifying the type of age represented.
#'
#' @return a distance matrix of differences in age.
#' @export
#'
#' @examples
#' x <- rpois(5, 30)
#' y <- rpois(5, 18)
#' dat <- data.frame(age = c(x, y), age_class = rep(c("YEAR", "MONTH"), each = 5))
#' age_dist(x)
#' age_dist(dat, extra_column = c(mo = "MONTH", yr = "YEAR"))
age_dist <- function(dat, extra_column = c(mo = "months", yr = "years")){
  if (!is.null(extra_column) & !is.vector(dat)){
    ages <- ifelse(dat[[2]] == extra_column["mo"], dat[[1]]*(1/12), dat[[1]])
  } else {
    ages <- dat
  }
  return(dist(ages, method = "manhattan"))
}
