#' @title Add Faculty Department
#' @description The function accepts a data frame with raw.text column, and adds a column for the faculty's department
#'
#' @param x data frame with raw.text column
#'@return the input data frame along with a new columns.
#'
#' @format \describe{
#'  \item{department}{the faculty member's department}
#'  }
#'
#' @importFrom dplyr %>%
#'
#' @export
#'

add_faculty_department <- function(x) {
  stopifnot(is.data.frame(x))
  stopifnot("title" %in% names(x))
  stopifnot("first.name" %in% names(x))
  stopifnot("last.name" %in% names(x))
  stopifnot(is.character(x$title))
  stopifnot(is.character(x$first.name))
  stopifnot(is.character(x$last.name))


  # Read in master list of departments
  filename <- system.file("extdata/departments.txt", package = "williamsmetrics")
  departments <- readr::read_lines(filename)

  ## Handle edge cases:
  ## Absurdly many ways to say WGES!!
  x$raw <- x$title
  x$raw <- stringr::str_replace_all(x$raw, "Women's, Gender and Sexuality Studies|Women's and Gender Studies|Women’s and Gender Studies|
                                    Women’s, Gender and Sexuality Studies|Women’s, Gender and Sexuality Studies|Women’s, Gender, and Sexuality Studies
                                    Womens’, Gender, and Sexuality Studies|Womens’, Gender, and Sexuality Studies|Women’s, Gender, and Sexuality Studies|
                                    Women s and Gender Studies", "WGES")

  ## "Economics" mispelled
  x$raw <- stringr::str_replace_all(x$raw, "Economic", "Economics")

  ## It seems like there are too many edge cases to design a simple munging criteria like we did for majors in graduates.
  ## Instead, we've used a manually prepared master list of departments and str_detect to get the departments.
  ## For example, for Daniel P. Aalberts #  Associate Professor of Physics # B.S. (1989) M.I.T.; Ph.D. (1994) M.I.T.
  ## we would infer deopartment as "Phsyics"
  x$department <- NA
  for(department in departments){
    x$department[which(stringr::str_detect(x$raw, department) & is.na(x$department))] <- department
  }

  ## But there are some annoying titles: for example, Bernadette Brooten # Croghan Bicentennial Visiting Professor in Biblical
  ## and Early Christian Studies, Spring Semester # B.A. (1971) University of Portland; Ph.D. (1982) Harvard”. From above, we
  ## should probably infer department as "Religion". However, this cannot be achieved with the the above str_detect mechansism
  ## and has to be explicitly handled here if required.

  ## x$department[which(x$first.name == "Bernadette" & x$last.name == "Brooten")] <- "Religion"

  ## Several more to complete here...


  x$raw <- NULL
  x

}
