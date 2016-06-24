#' Get all the distance functions in epimatch
#'
#' @return a named vector of distance functions in epimatch
#' @export
#'
#' @examples
#' distFuns()
distFuns <- function(){
  epidist_funs <- c(`Age` = 'ageDists',
                    `Date` = 'dateDists',
                    `Gender` = 'genderDists',
                    `Generic (text or numeric)` = 'genericDists',
                    `Location` = 'locationDists',
                    `Name (or ID)` = 'nameDists')
  return(epidist_funs)
}
