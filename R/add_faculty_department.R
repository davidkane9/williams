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
  stopifnot("raw.text" %in% names(x))
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

  ## "Economics" mispelled
  x$raw <- stringr::str_replace_all(x$raw, "Economic", "Economics")

  ## It seems like there are too many edge cases to design a simple munging criteria like we did for majors in graduates.
  ## Instead, we've used a manually prepared master list of departments and str_detect to get the departments
  x$department <- NA
  for(department in departments){
    x$department[which(stringr::str_detect(x$raw, department) & is.na(x$department))] <- department
  }

  ## But there are some annoying titles: for example, Bernadette Brooten # Croghan Bicentennial Visiting Professor in Biblical
  ## and Early Christian Studies, Spring Semester # B.A. (1971) University of Portland; Ph.D. (1982) Harvard”. From above, we
  ## infer department as "Biblical and Early Christian Studies", but really, according to how Williams would define department,
  ## this should probably be "Religion". We will explicitly handle these edge cases here

  ## Start with some easy ones:
  x$department[which(x$department == "Geology")] <- "Geosciences"
  x$department[which(x$department == "Law")] <- "Justice and Law"
  x$department[which(x$department == "International Studies")] <- "Global Studies"
  x$department[which(x$department == "International Relations")] <- "Global Studies"
  x$department[which(x$department == "Environmental Issues")] <- "Environmental Science"
  x$department[which(x$department == "Public Affairs")] <- "Political Science"
  x$department[which(x$department == "Biblical and Early Christian Studies")] <- "Religion"
  x$department[which(x$department == "Legal Studies")] <- "Justice and Law"
  x$department[which(x$department == "American Civilization")] <- "American Studies"
  x$department[which(x$department == "Politics")] <- "Political Science"
  x$department[which(x$department == "Romance Language")] <- "Spanish"
  x$department[which(x$department == "Natural Sciences")] <- "Environmental Studies"
  x$department[which(x$department == "Humanities")] <- "Art"
  x$department[which(x$department == "Ancient Languages")] <- "Classics"
  ## Several more to complete here...


  x$raw <- NULL
  x

}
