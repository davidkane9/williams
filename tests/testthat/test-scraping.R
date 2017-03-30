#' We will check for proper extraction of first names here.
#' Test cases:
#'            TEST 1: "Karan Tibrewal" should yield "Karan"
#'            TEST 2: "Karan Tibrewal, with honors in Computer Science" should yield "Karan"
#'            TEST 3: "Karan Tibrewal, with honors in Computer Science, with honors in Mathematics"
#'                    should yield "Karan"
#'            TEST 4: "\n" should yield an error
#'            TEST 5: "KaranTibrewal, with honors in Computer Science" should yield an error

test_that("Testing scraping first name from graduation details", {

  expect_equal(scrape_first_name("Karan Tibrewal"), "Karan")
  expect_equal(scrape_first_name("Karan Tibrewal, with honors in Computer Science"),
                         "Karan"
                           )
  expect_equal(
    scrape_first_name("Karan Tibrewal, with honors in Computer Science, with honors in Mathematics"),
    "Karan"
    )
  expect_error(scrape_first_name("\n"))
  expect_error(scrape_first_name("KaranTibrewal, with honors in Computer Science"))
  expect_error(scrape_first_name(" KaranTibrewal"))
})

#' We will check for proper extraction of middle names here.
#' Test cases:
#'            TEST 1: "Karan Tibrewal" should yield ""
#'            TEST 2: "Karan Tibrewal, with honors in Computer Science" should yield ""
#'            TEST 3: "Karan Tibrewal, with honors in Computer Science, with honors in Mathematics"
#'                    should yield ""
#'            TEST 4: "\n" should yield ""
#'            TEST 5: "KaranTibrewal, with honors in Computer Science" should yield ""
#'            TEST 6: "Chris James Washington" should yield "James"
#'            TEST 7: "Chris James Washington, with honors in Economics" should yield "James"
#'            TEST 8: "James Hugh Calum Laurie, with honors in Economics" should yield "Hugh Calum"

test_that("Testing scraping middle name from graduation details", {

  expect_equal(scrape_middle_name("Karan Tibrewal"), "")
  expect_equal(scrape_middle_name("Karan Tibrewal, with honors in Computer Science"),
                         ""
  )
  expect_equal(
    scrape_middle_name("Karan Tibrewal, with honors in Computer Science, with honors in Mathematics"),
    ""
  )
  expect_equal(scrape_middle_name("\n"), "")
  expect_equal(scrape_middle_name("KaranTibrewal, with honors in Computer Science"), "")
  expect_equal(scrape_middle_name(" KaranTibrewal"), "")
  expect_equal(scrape_middle_name("Chris James Washington"), "James")
  expect_equal(scrape_middle_name("Chris James Washington, with honors in Economics"), "James")
  expect_equal(scrape_middle_name("James Hugh Calum Laurie, with honors in Economics"),
                         "Hugh Calum")
})

#' We will check for proper extraction of last names here.
#' Test cases:
#'            TEST 1: "Karan Tibrewal" should yield "Tibrewal"
#'            TEST 2: "Karan Tibrewal, with honors in Computer Science" should yield "Tibrewal"
#'            TEST 3: "Karan Tibrewal, with honors in Computer Science, with honors in Mathematics"
#'                    should yield "Tibrewal"
#'            TEST 4: "\n" should yield an error
#'            TEST 5: "KaranTibrewal, with honors in Computer Science" should yield an error
#'            TEST 6: "Chris James Washington" should yield "Washington"
#'            TEST 7: "Chris James Washington, with honors in Economics" should yield "Washington"
#'            TEST 8: "James Hugh Calum Laurie, with honors in Economics" should yield "Laurie"

test_that("Testing scraping middle name from graduation details", {

  expect_equal(scrape_last_name("Karan Tibrewal"), "Tibrewal")
  expect_equal(scrape_last_name("Karan Tibrewal, with honors in Computer Science"),
                         "Tibrewal"
  )
  expect_equal(
    scrape_last_name("Karan Tibrewal, with honors in Computer Science, with honors in Mathematics"),
    "Tibrewal"
  )
  expect_error(scrape_last_name("\n"))
  expect_error(scrape_last_name("KaranTibrewal, with honors in Computer Science"))
  expect_error(scrape_last_name(" KaranTibrewal"))
  expect_equal(scrape_last_name("Chris James Washington"), "Washington")
  expect_equal(scrape_last_name("Chris James Washington, with honors in Economics"), "Washington")
  expect_equal(scrape_last_name("James Hugh Calum Laurie, with honors in Economics"),
                         "Laurie")
})


#' We will check for extraction of primary thesis distinction level here
#' Test cases:
#'            TEST 1: "Karan Tibrewal" should yield "NONE"
#'            TEST 2: "Karan Tibrewal, with honors in Computer Science" should yield "HONORS"
#'            TEST 3: "Karan Tibrewal, with highest honors in Computer Science, with honors in Mathematics"
#'                    should yield "HIGHEST"
#'            TEST 4: "\n" should yield "NONE"
#'            TEST 5: "Karan Tibrewal, with highest honors in Computer Science" should yield "HIGHEST"
#'            TEST 7: "Chris James Washington, with honors in Economics" should yield "HONORS"
#'            TEST 8: "James Hugh Calum Laurie" should yield "NONE"

test_that("Testing scraping primary honors distinction level graduation details", {

  expect_equal(scrape_primary_thesis_distinction("Karan Tibrewal"), "NONE")
  expect_equal(scrape_primary_thesis_distinction("Karan Tibrewal, with honors in Computer Science"),
                         "HONORS"
  )
  expect_equal(
    scrape_primary_thesis_distinction("Karan Tibrewal, with highest honors in Computer Science, with honors in Mathematics"),
    "HIGHEST"
  )
  expect_equal(scrape_primary_thesis_distinction("\n"), "NONE")
  expect_equal(scrape_primary_thesis_distinction("Karan Tibrewal, with honors in Computer Science"), "HONORS")
  expect_equal(scrape_primary_thesis_distinction("Chris James Washington, with honors in Economics"), "HONORS")
  expect_equal(scrape_primary_thesis_distinction("James Hugh Calum Laurie"), "NONE")
})


#' We will check for extraction of major for primary thesis here
#' Test cases:
#'            TEST 1: "Karan Tibrewal" should yield "NONE"
#'            TEST 2: "Karan Tibrewal, with honors in Computer Science" should yield "Computer Science"
#'            TEST 3: "Karan Tibrewal, with highest honors in Computer Science, with honors in Mathematics"
#'                    should yield "Computer Science"
#'            TEST 4: "\n" should yield "NONE"
#'            TEST 5: "Karan Tibrewal, with highest honors in Computer Science" should yield "Computer Science"
#'            TEST 7: "Chris James Washington, with honors in Economics" should yield "Economics"
#'            TEST 8: "James Hugh Calum Laurie" should yield "NONE"
#'            TEST 9: "Karan Tibrewal, with highest honors in Contract Major: Dance" should yield
#'                    "Contract Major: Dance"

test_that("Testing scraping primary honors major from graduation details", {

  expect_equal(scrape_primary_thesis_major("Karan Tibrewal"), "NONE")
  expect_equal(scrape_primary_thesis_major("Karan Tibrewal, with honors in Computer Science"),
                         "Computer Science"
  )
  expect_equal(
    scrape_primary_thesis_major("Karan Tibrewal, with highest honors in Computer Science,
                                with honors in Mathematics"),
    "Computer Science"
  )
  expect_equal(scrape_primary_thesis_major("\n"), "NONE")
  expect_equal(scrape_primary_thesis_major("Karan Tibrewal, with honors in Computer Science"),
                         "Computer Science")
  expect_equal(scrape_primary_thesis_major("Chris James Washington, with honors in Economics"),
                         "Economics")
  expect_equal(scrape_primary_thesis_major("James Hugh Calum Laurie"),
                         "NONE")
  expect_equal(scrape_primary_thesis_major("Karan Tibrewal, with highest honors in Contract Major: Dance"),
                         "Contract Major: Dance")
})

#' We will check for extraction of secondary thesis distinction level here
#' Test cases:
#'            TEST 1: "Karan Tibrewal" should yield "NONE"
#'            TEST 2: "Karan Tibrewal, with honors in Computer Science" should yield "NONE"
#'            TEST 3: "Karan Tibrewal, with highest honors in Computer Science, with honors in Mathematics"
#'                    should yield "HONORS"
#'            TEST 4:  "Karan Tibrewal, with highest honors in Computer Science, with highest honors in Mathematics"
#'                    should yield "HIGHEST"
#'            TEST 5: "Karan Tibrewal, with highest honors in Computer Science" should yield "NONE"
#'            TEST 7: "Chris James Washington, with honors in Economics" should yield "NONE"
#'            TEST 8: "James Hugh Calum Laurie" should yield "NONE"

test_that("Testing scraping secondary honors distinction level graduation details", {

  expect_equal(scrape_secondary_thesis_distinction("Karan Tibrewal"), "NONE")
  expect_equal(scrape_secondary_thesis_distinction("Karan Tibrewal, with honors in Computer Science"),
                         "NONE"
  )
  expect_equal(
    scrape_secondary_thesis_distinction("Karan Tibrewal, with highest honors in Computer Science, with honors in Mathematics"),
    "HONORS"
  )
  expect_equal(
    scrape_secondary_thesis_distinction("Karan Tibrewal, with highest honors in Computer Science, with highest honors in Mathematics"),
    "HIGHEST")
  expect_equal(scrape_secondary_thesis_distinction("Karan Tibrewal, with honors in Computer Science"), "NONE")
  expect_equal(scrape_secondary_thesis_distinction("Chris James Washington, with honors in Economics"), "NONE")
  expect_equal(scrape_secondary_thesis_distinction("James Hugh Calum Laurie"),
                         "NONE")
})


#' We will check for extraction of major for secondary thesis here
#' Test cases:
#'            TEST 1: "Karan Tibrewal" should yield "NONE"
#'            TEST 2: "Karan Tibrewal, with honors in Computer Science" should yield "Computer Science"
#'            TEST 3: "Karan Tibrewal, with highest honors in Computer Science, with honors in Mathematics"
#'                    should yield "Computer Science"
#'            TEST 4: "\n" should yield "NONE"
#'            TEST 5: "Karan Tibrewal, with highest honors in Computer Science" should yield "Computer Science"
#'            TEST 7: "Chris James Washington, with honors in Economics" should yield "Economics"
#'            TEST 8: "James Hugh Calum Laurie" should yield "NONE"
#'            TEST 9: "Karan Tibrewal, with highest honors in Contract Major: Dance" should yield
#'                    "Contract Major: Dance"

test_that("Testing scraping primary honors distinction level graduation details", {

  expect_equal(scrape_secondary_thesis_major("Karan Tibrewal"), "NONE")
  expect_equal(scrape_secondary_thesis_major("Karan Tibrewal, with honors in Computer Science"),
                         "NONE"
  )
  expect_equal(
    scrape_secondary_thesis_major("Karan Tibrewal, with highest honors in Computer Science, with honors in Mathematics"),
    "Mathematics"
  )
  expect_equal(
    scrape_secondary_thesis_major("Karan Tibrewal, with highest honors in Art, with highest honors in Computer Science"),
    "Computer Science")
  expect_equal(scrape_secondary_thesis_major("Karan Tibrewal, with honors in Computer Science"), "NONE")
  expect_equal(scrape_secondary_thesis_major("Chris James Washington, with honors in Economics"), "NONE")
  expect_equal(scrape_secondary_thesis_major("James Hugh Calum Laurie"),
                         "NONE")
})
