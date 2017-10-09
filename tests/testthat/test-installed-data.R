library(williams)

context("installed data is correct")

test_that("graduate data sensible", {

  data(graduates)

  ## Some of these are the same checks that are at the bottom of
  ## create_graduates(). We should probably have a function that would
  ## encapsulate all this and test it multiple times, and on graduates_details
  ## too. 

  stopifnot(all(unique(graduates$year) > 1999 & unique(graduates$year) < 2020))
  stopifnot(all(table(graduates$year) > 500 & table(graduates$year) < 600))

  stopifnot(sum(!is.na(graduates$major))   == sum(!is.na(graduates$honor)))

  stopifnot(length(unique(graduates$latin.honors)) == 4)
  stopifnot(length(unique(graduates$honor)) == 3)


  ## Are we handling III correctly?

  stopifnot(all(as.data.frame(graduates[c(8584, 8642), "last.name"])[,1] == c("Capute", "Finnie")))

  ## How about Jr.?

  stopifnot(all(as.data.frame(graduates[c(8406, 8436, 8740, 8866), "last.name"])[,1] ==
                  c("McDonald", "Worthington", "Martin", "Vargas" )))
  
  ## No empty cells.
  
  stopifnot(! any(graduates == "", na.rm = TRUE))

  ## Need more tests here. 
  
  
})

test_that("faculty data sensible", {
  
  data(faculty)
  
  ## There should not be "" entries. They should be explicitly NA instead.
  
  ## stopifnot(! any(faculty == "", na.rm = TRUE))
  
  stopifnot(nrow(faculty) == 428)
  
  stopifnot(all(faculty$status %in% c("Athletic", "Tenure-track", "Tenured", "Visiting", NA)))
  stopifnot(all(faculty$leave %in% c("Academic Year", "First Semester", "None", "Second Semester", "Calendar Year")))
  
  ## Must be a better way to handle ranges and comparisons which have NA values
  ## sometime.
  
  stopifnot(nrow(dplyr::filter(faculty, ! dplyr::between(birth.year, 1900, 2000))) == 0)
  stopifnot(nrow(dplyr::filter(faculty, ! birth.year < first.degree.year)) == 0)

  ## Some professors only have one degree, so we list that as both first and 
  ## last degree (with same year). Not sure if that makes sense. Other
  ## professors have a masters as a last degree, awarded in same year as BA.
  
  stopifnot(nrow(dplyr::filter(faculty, ! first.degree.year <= last.degree.year)) == 0)
  
})
