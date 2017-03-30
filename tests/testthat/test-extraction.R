test_that("Testing scraping information about summa cum laude grads from course catalogs", {
  truth <- c("DoHyun Tony Chung, with honors in Political Economy",
             "Rebecca Tamar Cover, with highest honors in Astrophysics",
             "Amanda Bouvier Edmonds",
             "Douglas Bertrand Marshall, with highest honors in Philosophy")
  x <- readr::read_file("extraction-test")
  fact <- slice_and_delimit(x, "Bachelor of Arts, Summa Cum Laude\r\n", "Bachelor of Arts, Magna Cum Laude")
  expect_equal(truth, fact)
})

test_that("Testing scraping information about magna cum laude grads from course catalogs", {
  truth <- c("David Scott Adams",
             "Julianne Humphrey Anderson",
             "Michael Zubrow Barkin",
             "Robert Charles Blackstone")
  x <- readr::read_file("extraction-test")
  fact <- slice_and_delimit(x, "Bachelor of Arts, Magna Cum Laude\r\n", "Bachelor of Arts, Cum Laude\r\n")
  expect_equal(truth, fact)
})

test_that("Testing scraping information about cum laude grads from course catalogs", {
  truth <- c("Katherine Osborn Abbott, with honors in English",
             "Khaleefah Ali Khaleefah Al-Sabah",
              "Andrew Carl Arbesman", "Jonathan Seth Asarch","Aida Avdic")
  x <- readr::read_file("extraction-test")
  fact <- slice_and_delimit(x, "Bachelor of Arts, Cum Laude\r\n", "Bachelor of Arts\r\n")
  expect_equal(truth, fact)
})

test_that("Testing scraping information about non-honors grads from course catalogs", {
  truth <- c("Moges Abebe", "Katherine Araminta Acton, with honors in English",
             "Mark Robert Acton, with highest honors in Physics",
             "Patrick Ramone Adair", "David Lincoln Adams")
  x <- readr::read_file("extraction-test")
  fact <- slice_and_delimit(x, "Bachelor of Arts\r\n")
  expect_equal(truth, fact)
})
