library(williamsmetrics)

context("create_graduates")

test_that("Package works", {
  x.1 <- create_graduates()
  x.2 <- create_graduates(complete = TRUE)

  expect_equal(x.1[c("first.name", "year", "latin.honors", "major", "gender", "race")],
               x.2[c("first.name", "year", "latin.honors", "major", "gender", "race")])

 data(graduates)

 expect_equal(x.1, graduates)
})
