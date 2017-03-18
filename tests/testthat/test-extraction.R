#' We will check for proper extraction of information of suma cum laude graduates here.
#' Test strategy:
#'            We create a dummy test catalog and test scrape_suma_cum_laude against the file. We know the
#'            true values that we should scrape. Our text function verifies that scrape_suma_cum_laude
#'            scrapes exactly the truth values.
testthat::test_that("Testing scraping information about summa cum laude grads from course catalogs", {
  truth <- c("DoHyun Tony Chung, with honors in Political Economy",
             "Rebecca Tamar Cover, with highest honors in Astrophysics",
             "Amanda Bouvier Edmonds",
             "Douglas Bertrand Marshall, with highest honors in Philosophy")
  data <- readFile("~karantibrewal/KT/williamsmetrics/tests/testdata/extraction-test")
  fact <- scrape_suma_cum_laude(data)
  testthat::expect_equal(truth, fact)
})

#' We will check for proper extraction of information of magna cum laude graduates here.
#' Test strategy:
#'            We create a dummy test catalog and test scrape_magna_cum_laude against the file. We know the
#'            true values that we should scrape. Our text function verifies that scrape_magna_cum_laude
#'            scrapes exactly the truth values.
testthat::test_that("Testing scraping information about summa cum laude grads from course catalogs", {
  truth <- c("David Scott Adams",
             "Julianne Humphrey Anderson",
             "Michael Zubrow Barkin",
             "Robert Charles Blackstone")
  data <- readFile("~karantibrewal/KT/williamsmetrics/tests/testdata/extraction-test")
  fact <- scrape_magna_cum_laude(data)
  testthat::expect_equal(truth, fact)
})

#' We will check for proper extraction of information of  cum laude graduates here.
#' Test strategy:
#'            We create a dummy test catalog and test scrape_cum_laude against the file. We know the
#'            true values that we should scrape. Our text function verifies that scrape_cum_laude
#'            scrapes exactly the truth values.
testthat::test_that("Testing scraping information about summa cum laude grads from course catalogs", {
  truth <- c("Katherine Osborn Abbott, with honors in English",
             "Khaleefah Ali Khaleefah Al-Sabah",
              "Andrew Carl Arbesman", "Jonathan Seth Asarch","Aida Avdic")
  data <- readFile("~karantibrewal/KT/williamsmetrics/tests/testdata/extraction-test")
  fact <- scrape_cum_laude(data)
  testthat::expect_equal(truth, fact)
})

#' We will check for proper extraction of information of graduates with no latin honors here.
#' Test strategy:
#'            We create a dummy test catalog and test scrape_graduates against the file. We know the
#'            true values that we should scrape. Our text function verifies that scrape_graduates
#'            scrapes exactly the truth values.
testthat::test_that("Testing scraping information about summa cum laude grads from course catalogs", {
  truth <- c("Moges Abebe", "Katherine Araminta Acton, with honors in English",
             "Mark Robert Acton, with highest honors in Physics",
             "Patrick Ramone Adair", "David Lincoln Adams")
  data <- readFile("~karantibrewal/KT/williamsmetrics/tests/testdata/extraction-test")
  fact <- scrape_graduates(data)
  testthat::expect_equal(truth, fact)
})


