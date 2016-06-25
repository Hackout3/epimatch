context("Distance Tests")

test_that("distFuns returns the distances", {
  d      <- distFuns()
  dnames <- vapply(strsplit(names(d), " "), function(i) i[1], character(1))
  expect_is(d, "character")
  expect_equal(length(d), 6)
  expect_equivalent(tolower(dnames), strtrim(d, nchar(dnames)))
})

test_that("ageDists returns expected values", {
  x <- data.frame(age = c(28L, 22L, 23L, 28L, 26L), age_class = "YEAR")
  y <- data.frame(age = c(27L, 21L, 21L, 11L, 18L), age_class = "MONTH")
  xy <- rbind(x, y)
  expect_is(ageDists(x), "dist")

  dist_x_y <- ageDists(x, y)
  dist_xy  <- ageDists(xy)
  expect_equal(unique(dist_xy), c(1, 0))
  # Show that the combined data set and separate work
  expect_equal(dist_x_y, dist_xy)
  # Comparing year conversion
  expect_equal(ageDists(data.frame(y$age*(1/12))), ageDists(y))
  # Comparing threshold
  expect_equal(45 - sum(ageDists(xy, e = 0)), sum(duplicated(xy$age)))
  # Assess that the conversion to month matters
  expect_gt(sum(ageDists(xy["age"])), sum(dist_xy))
  # extra_column can take any value
  x$age_class <- "Geordi"
  y$age_class <- "Laforge"
  starfleet   <- ageDists(rbind(x, y),
                          extra_column = list(yr = "geordi", mo = "laforge"))
  expect_equal(starfleet, dist_xy)
})
test_that("dateDists returns expected values", {
  test <- c("Jan-21-01", "Jan-25-02", "Jan-21-01", "Jan-31-01")
  test <- data.frame(test, stringsAsFactors = FALSE)

  eleven_days <- dateDists(test, threshold = 11)
  zero_days   <- dateDists(test, threshold = 0)
  expect_equal(sum(eleven_days), 6)
  expect_equal(sum(zero_days), (nrow(test)*nrow(test)) - 6)

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
