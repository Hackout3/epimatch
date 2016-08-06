context("tablesFromMatch tests")

indata   <- system.file("files", package = "epimatch")
indata   <- dir(indata, full.names = TRUE)
x        <- lapply(indata, read.csv, stringsAsFactors = FALSE)
names(x) <- basename(indata)

# We will use one data set from the case information and lab results
case <- x[["CaseInformationForm.csv"]]
lab <- x[["LaboratoryResultsForm7.csv"]]

funlist <- list(
             list(d1vars = "ID",
                  d2vars = "ID",
                  fun = "nameDists",
                  extraparams = NULL,
                  weight = 1),
             list(d1vars = c("Surname", "OtherNames"),
                  d2vars = c("SurnameLab", "OtherNameLab"),
                  fun = "nameDists",
                  extraparams = NULL,
                  weight = 0.5)
           )
res <- matchEpiData(dat1 = case,
                    dat2 = lab,
                    funlist = funlist,
                    thresh = 0.25,
                    giveWeight = TRUE)
l_res <- tablesFromMatch(case, lab, funlist, matchList = res, collapse = FALSE)
t_res <- tablesFromMatch(case, lab, funlist, matchList = 0.25)

## This not only tests the conversion, but since the input for the tests above
## are the same, this also tests to ensure that the matchList argument can take
## the result of matchEpiData AND a threshold.
test_that("list output and collapsed output are the same", {
  expect_equal(sum(vapply(l_res, nrow, 1L)), nrow(t_res))
  expect_equal(do.call("rbind", l_res), t_res[names(l_res[[1]])])
})

test_that("tablesFromMatch returns a list when collapse = FALSE", {
  expect_is(l_res, "list")
  for (i in seq_along(l_res)){
    dat <- l_res[[i]]
    expect_is(dat, "data.frame", info = paste("group", i))
    expect_equal(ncol(dat), ncol(l_res[[i]]))
  }
})

test_that("the data frame is tidy", {
  expect_is(t_res, "data.frame")
  expect_identical(names(t_res)[length(t_res)], "group")
  expect_equal(length(unique(t_res$group)), length(l_res))
  expect_equal(nrow(t_res), 20L)
  expect_equal(ncol(t_res), 6L)
})
