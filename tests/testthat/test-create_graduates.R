library(williamsmetrics)

context("create_graduates")

test_that("Package data sensible", {

  data(graduates)

  x <- graduates

  ## These are the same checks that are at the bottom of create_graduates(). We
  ## should probably have a function that would encapsulate all this and test it
  ## multiple times, and on graduates_details too.

  stopifnot(all(unique(x$year) > 1999 & unique(x$year) < 2020))
  stopifnot(all(table(x$year) > 500 & table(x$year) < 600))

  stopifnot(sum(!is.na(x$major))   == sum(!is.na(x$honor)))

  stopifnot(length(unique(x$latin.honors)) == 4)
  stopifnot(length(unique(x$honor)) == 3)

})
