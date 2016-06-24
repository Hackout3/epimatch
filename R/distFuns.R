#' Get all the distance functions in epimatch
#'
#' @return a named vector of distance functions in epimatch
#' @export
#'
#' @examples
#' distFuns()
distFuns <- function(){
  epidist_funs <- c(age = 'ageDists',
                    date = 'dateDists',
                    gender = 'genderDists',
                    generic = 'genericDists',
                    location = 'locationDists',
                    name = 'nameDists')
  return(epidist_funs)
}
