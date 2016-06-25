#' Create distance from age column
#'
#' @param dat1,dat2 a data frame of ages
#' @param e the number of years difference allowed for two entries to be the
#'   same
#' @param extra_column a named list specifying the type of age represented
#'
#' @description This function will take one or two data frames containing ages
#'   and create a distance matrix based off of the manhattan distance. Data can
#'   be entered as a single column data frame assuming years or a two column
#'   data frame with a specifier for months or year in the second column.
#' @return a distance matrix of binary values where 0 indicates that two ages
#'   are within the given threshold of each other and 1 indicates not.
#' @export
#'
#' @examples
#' set.seed(999)
#' x <- data.frame(age = rpois(5, 30), age_class = "YEAR")
#' y <- data.frame(age = rpois(5, 18), age_class = "MONTH")
#' dat <- rbind(x, y)
#' # Assume everything is in years
#' ageDists(dat[1])
#'
#' # Specify year and month
#' ageDists(dat, extra_column = list(mo = "MONTH", yr = "YEAR"))
#'
#' # Input two data sets
#' ageDists(x[1], y, extra_column = list(mo = "MONTH", yr = "YEAR"))
#'
#' # allow ages within three years of each other to match
#' ageDists(dat, e = 3, extra_column = list(mo = "MONTH", yr = "YEAR"))
ageDists <- function(dat1, dat2 = NULL, e = 1,
                     extra_column = list(mo = c("months", "month", "mon", "m"),
                                      yr = c("years", "year", "yrs", "yr", "y"),
                                      day = c("days", "day", "d"))){
  dat <- clean_age(dat1, extra_column)
  if (!is.null(dat2)){
    dat2 <- clean_age(dat2, extra_column)
    dat <- c(dat, dat2)
  }
  age_diff <- dist(dat, method = "manhattan")
  age_diff[age_diff <= e] <- 0
  age_diff[age_diff > e]  <- 1
  # age_diff <- age_diff/max(age_diff, na.rm = TRUE)
  return(age_diff)
}
