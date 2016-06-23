#' Get all the distance functions in epimatch
#'
#' @return a named vector of distance functions in epimatch
#' @export
#'
#' @examples
#' distFuns()
distFuns <- function(){
  epidists      <- ls("package:epimatch")
  epidist_funs  <- grep("Dists", epidists, value = TRUE)
  epidist_names <- strsplit(epidist_funs, "Dists")
  epidist_names <- unlist(epidist_names)
  names(epidist_funs) <- epidist_names
  return(epidist_funs)
}
