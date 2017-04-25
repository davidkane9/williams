add_faculty_department <- function(x) {
  stopifnot(is.data.frame(x))
  stopifnot("title" %in% names(x))
  stopifnot(is.character(x$title))

  # Read in master list of departments
  filename <- system.file("extdata/departments.txt", package = "williamsmetrics")
  departments <- readr::read_lines(filename)

  ## Handle edge cases:
  ## Absurdly many ways to say WGES!!
  x$raw <- x$raw.text
  x$raw <- stringr::str_replace_all(x$raw, "Women's, Gender and Sexuality Studies|Women's and Gender Studies|Women’s and Gender Studies|
                                    Women’s, Gender and Sexuality Studies|Women’s, Gender and Sexuality Studies|Women’s, Gender, and Sexuality Studies
                                    Womens’, Gender, and Sexuality Studies|Womens’, Gender, and Sexuality Studies|Women’s, Gender, and Sexuality Studies|
                                    Women s and Gender Studies", "WGES")

  ## "Economics" mispelled elsewehere
  x$raw <- stringr::str_replace_all(x$raw, "Economic", "Economics")


  x$department <- NA
  for(department in departments){
    x$department[which(stringr::str_detect(x$raw, department) & is.na(x$department))] <- department
  }

}

"Steven P. Souza # Senior Lecturer in Astronomy #"
