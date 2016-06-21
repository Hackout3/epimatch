#' generic function for matching
#'
#' @param dat a vector of data to determine matching
#' @param ...condiditions to apply
#'
#' @return a pairwise matrix of scores from 0 (exact match) to 1 (no match)
#' @export
#'
#' @examples
#' generic_match(1:3)
generic_match <- function(dat, ...){
	if (!any(duplicated(dat))){
		return(0)
	}
}
