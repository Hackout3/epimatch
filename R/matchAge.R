#' Create distance from age column
#'
#' @param dat1 a data frame of ages
#' @param dat2 a data frame of ages
#' @param e the number of years difference allowed for two entries to be the same
#' @param extra_column a named vector specifying the type of age represented
#'
#' @description This function will take one or two data frames containing ages
#'   and create a distance matrix based off of the manhattan distance. Data can
#'   be entered as a single column data frame assuming years or a two column
#'   data frame with a specifier for months or year in the second column.
#' @return a distance matrix of differences in age.
#' @export
#'
#' @examples
#' set.seed(999)
#' x <- data.frame(age = rpois(5, 30), age_class = "YEAR")
#' y <- data.frame(age = rpois(5, 18), age_class = "MONTH")
#' dat <- rbind(x, y)
#' age_dist(dat[1])
#' age_dist(dat, extra_column = c(mo = "MONTH", yr = "YEAR"))
#' age_dist(x[1], y, extra_column = c(mo = "MONTH", yr = "YEAR"))
age_dist <- function(dat1, dat2 = NULL, e = 1,
                     extra_column = c(mo = "months", yr = "years")){
  dat <- clean_age(dat1, extra_column)
  if (!is.null(dat2)){
    dat2 <- clean_age(dat2, extra_column)
    dat <- c(dat, dat2)
  }
  age_diff <- dist(dat, method = "manhattan")
  age_diff[age_diff <= e] <- 0
  age_diff <- age_diff/max(age_diff)
  return(age_diff)
}

clean_age <- function(dat, extra_column = c(mo = "months", yr = "years")){
  if (ncol(dat) > 1 && !is.null(extra_column)){
    ages <- ifelse(dat[[2]] == extra_column["mo"], dat[[1]]*(1/12), dat[[1]])
  } else {
    ages <- dat[[1]]
  }
  return(ages)
}