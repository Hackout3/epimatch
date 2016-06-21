#' Find duplicates in single data set
#'
#' @param dat a linelist
#' @param funlist a list of functions and arguments to use to evaluate columns of dat.
#'
#' @return
#' @export
#'
#' @examples
epi_self_match <- function(dat,
                           funlist = list(name = "match_names",
                                          age = list("age_list", extra_column = c(mo = "months", yr = "years")),
                                          CaseID = "exactMatchCaseIDIntraDataset")
                           ){
  datlist <- funlist
  for (i in seq(funlist)){
    fun <- funlist[[i]]
    fname <- names(funlist)[i]
    if (!is.list(fun)){
      datlist[[i]] <- do.call(fun, list(dat[[fname]]))
    } else {
      FUN <- fun[[1]]
      datlist[[i]] <- do.call(FUN, c(list(dat[[fname]]), fun[-1]))
    }
  }
  return(datlist)
}
