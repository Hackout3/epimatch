context("distFuns Test")

test_that("distFuns returns the distances", {
  d      <- distFuns()
  dnames <- vapply(strsplit(names(d), " "), function(i) i[1], character(1))
  expect_is(d, "character")
  expect_equal(length(d), 6)
  expect_equivalent(tolower(dnames), strtrim(d, nchar(dnames)))
})

context("ageDist Test")

x <- data.frame(age = c(28L, 22L, 23L, 28L, 26L), age_class = "YEAR")
y <- data.frame(age = c(27L, 21L, 21L, 11L, 18L), age_class = "MONTH")
xy <- rbind(x, y)


dist_x_y <- ageDists(x, y)
dist_xy  <- ageDists(xy)

test_that("ageDists returns expected values", {

  expect_is(dist_xy, "dist")
  expect_equal(unique(dist_xy), c(1, 0))
})

test_that("the combined data set and separate work", {
  expect_equal(dist_x_y, dist_xy)
})

test_that("month to year conversion works", {
  expect_equal(ageDists(data.frame(y$age*(1/12))), ageDists(y))
})

test_that("a zero year threshold returns the expected value", {
  expect_equal(45 - sum(ageDists(xy, e = 0)), sum(duplicated(xy$age)))
})

test_that("the conversion to month matters", {
  expect_gt(sum(ageDists(xy["age"])), sum(dist_xy))
})

test_that("extra_column can take any value", {
  x$age_class <- "Geordi"
  y$age_class <- "Laforge"
  starfleet   <- ageDists(rbind(x, y),
                          extra_column = list(yr = "geordi", mo = "laforge"))
  expect_equal(starfleet, dist_xy)
})




context("dateDist Test")

test1 <- data.frame(x = c("Jan-21-01", "01-25-02"), stringsAsFactors = FALSE)
test2 <- data.frame(x = c("01-Jan-21", "2001-Jan-31"), stringsAsFactors = FALSE)

eleven_days_combined <- dateDists(rbind(test1, test2),
                                  dat1Format = c("mdy", "ymd"),
                                  threshold = 11)

eleven_days <- dateDists(test1, test2, dat2Format = "ymd", threshold = 11)
zero_days   <- dateDists(test1, test2, dat2Format = "ymd")
rel_days    <- dateDists(test1, test2, dat2Format = "ymd", threshold = NULL)

test_that("the default distance for dateDists is binary", {
  expect_equal(sort(unique(zero_days[TRUE])), c(0L, 1L))
})

test_that("combined and separate data sets work", {
  expect_identical(eleven_days_combined, eleven_days)
})

test_that("the threshold works", {
  expect_equal(sum(eleven_days), 6)
})

test_that("exact matching works", {
  expect_equal(sum(zero_days), (2*nrow(test1))^2 - 6)
})

test_that("relative matching works", {
  expect_gt(sum(zero_days), sum(rel_days))
  expect_lt(sum(eleven_days), sum(rel_days))
  expect_gt(length(sort(unique(rel_days[TRUE]))), 2L)
})






test_that("genderDists returns expected values", {
  # genderDists()
})
test_that("genericDists returns expected values", {
  # genericDists()
})
test_that("locationDists returns expected values", {
  # locationDists()
})
test_that("nameDists returns expected values", {
  # nameDists()
})
