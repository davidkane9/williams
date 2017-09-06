#' @title Add Faculty Department
#' @description The function accepts a data frame with raw.text column, and adds 
#'  a column for the faculty's department
#'
#' @param x data frame with raw.text column
#' @return the input data frame along with a new columns.
#'
#' @format \describe{
#'  \item{department}{the faculty member's department}
#'  }
#'
#' @importFrom dplyr %>%
#'
#' @export

add_faculty_department <- function(x) {

  stopifnot(is.data.frame(x))
  stopifnot("title" %in% names(x))
  stopifnot(is.character(x$title))
  stopifnot(is.character(x$last.name))

  # Read in master list of departments
  
  filename <- system.file("extdata/departments.txt", package = "williams")
  departments <- readr::read_lines(filename)

  ## Handle edge cases:
 
  ## Absurdly many ways to say WGES! Basic idea is that, during the time period,
  ## WGES had two main names: first, Women's and Gender Studies; second, Women's
  ## Gender and Sexuality Studies. And dealing with the different versions of 
  ## the apostraphe in Women's is not simple. Unicode is confusing! Fortunately,
  ## a simple regexp seems to do the job.
  
  ## Note that this is not that important since most professors who have an 
  ## affiliation to WGES have it as a second affiliation. We use the first
  ## affiliation to assign a department.
  
  x$raw <- x$title
  x$raw <- stringr::str_replace(x$raw, "Women(.*)Studies", "WGES")

  ## "Economics" mispelled
  
  x$raw <- stringr::str_replace_all(x$raw, "Economic", "Economics")

  ## It seems like there are too many edge cases to design a simple munging
  ## criteria like we did for majors in graduates. Instead, we've used a
  ## manually prepared master list of departments and str_detect to get the
  ## departments. For example, for Daniel P. Aalberts #  Associate Professor of
  ## Physics # B.S. (1989) M.I.T.; Ph.D. (1994) M.I.T. we would infer
  ## deopartment as "Phsyics"
  
  x$department <- NA
  
  for(department in departments){
    x$department[which(stringr::str_detect(x$raw, department) & is.na(x$department))] <- department
  }

  ## But there are some annoying titles: for example, Bernadette Brooten # Croghan Bicentennial Visiting Professor in Biblical
  ## and Early Christian Studies, Spring Semester # B.A. (1971) University of Portland; Ph.D. (1982) Harvard". From above, we
  ## should probably infer department as "Religion". However, this cannot be achieved with the the above str_detect mechansism
  ## and has to be explicitly handled here if required.

  ## First some specific departments which have been named otherwise in titles
  
  x$department[which(stringr::str_detect(x$title, "Natural Science") & is.na(x$department))] <- "Geosciences"
  x$department[which(stringr::str_detect(x$title, "Geology") & is.na(x$department))] <- "Geosciences"
  x$department[which(stringr::str_detect(x$title, "Librarian") & is.na(x$department))] <- "Library"
  x$department[which(stringr::str_detect(x$title, "Mystic") & is.na(x$department))] <- "Williams Mystic"
  x$department[which(stringr::str_detect(x$title, "Legal") & is.na(x$department))] <- "Justice & Law"
  x$department[which(stringr::str_detect(x$title, "Latina") & is.na(x$department))] <- "Laitna/o Studies"

  ## Now, some specific ones for which department was obvious after a google search
  
  x$department[which(x$first.name == "Bernadette" & x$last.name == "Brooten")] <- "Religion"
  x$department[which(x$first.name == "Jennifer" & x$last.name == "Austin")] <- "Spanish"
  x$department[which(x$first.name == "Gene" & x$last.name == "Bell-Villada")] <- "Spanish"
  x$department[which(x$first.name == "Nicole" & x$last.name == "Desrosiers")] <- "French"
  x$department[which(x$first.name == "Charles" & x$last.name == "Dew")] <- "History"
  x$department[which(x$first.name == "Peter" & x$last.name == "Frost")] <- "History"
  x$department[which(x$first.name == "Charles" & x$last.name == "Fuqua")] <- "Classics"
  x$department[which(x$first.name == "Antonio" & str_detect(x$last.name, "^Gim"))] <- "Spanish"
  x$department[which(x$first.name == "Gary" & x$last.name == "Jacobsohn")] <- "Political Science"
  x$department[which(x$first.name == "Glyn" & x$last.name == "Norton")] <- "French"
  x$department[which(x$first.name == "Ronald" & x$last.name == "Nigh")] <- "Environmental Studies"
  x$department[which(x$first.name == "Lawrence" & x$last.name == "Raab")] <- "English"
  x$department[which(x$first.name == "Gail" & x$last.name == "Newman")] <- "German"
  x$department[which(x$first.name == "Michael" & x$last.name == "MacDonald")] <- "Political Science"
  x$department[which(x$first.name == "Ralph" & x$last.name == "Bradburd")] <- "Economics"
  x$department[which(x$first.name == "Kerry" & x$last.name == "Christensen")] <- "Classics"
  x$department[which(x$first.name == "Robert" & x$last.name == "Dalzell")] <- "History"
  x$department[which(x$first.name == "Stephen" & x$last.name == "Sheppard")] <- "Economics"
  x$department[which(x$first.name == "Susan" & x$last.name == "Dunn")] <- "Leadership Studies"
  x$department[which(x$first.name == "Soledad" & x$last.name == "Fox")] <- "Spanish"
  x$department[which(x$first.name == "Isabel" & x$last.name == "Roche")] <- "French"
  x$department[which(x$first.name == "Jana" & x$last.name == "Sawicki")] <- "Philosophy"
  x$department[which(x$first.name == "James" & x$last.name == "Pethica")] <- "English"
  x$department[which(x$first.name == "Wayne" & x$last.name == "Meeks")] <- "Religion"
  x$department[which(x$first.name == "Jorge" & x$last.name == "Marcone")] <- "Environmental Studies"
  x$department[which(x$first.name == "Brenna" & x$last.name == "Munro")] <- "Africana Studies"
  x$department[which(x$first.name == "Luke" & x$last.name == "Bouvier")] <- "French"
  x$department[which(x$first.name == "April" & x$last.name == "Overstreet")] <- "Spanish"
  x$department[which(x$first.name == "Anthony" & x$last.name == "Nicastro")] <- "Italian"
  x$department[which(x$first.name == "Barbara" & x$last.name == "Bell")] <- "Theatre"
  x$department[which(x$first.name == "David" & x$last.name == "Kaiser")] <- "History"
  x$department[which(x$first.name == "Leslie" & x$last.name == "Wingard")] <- "English"
  x$department[which(x$first.name == "Youngshik" & x$last.name == "Bong")] <- "Asian Studies"
  x$department[which(x$first.name == "Peter" & x$last.name == "Erickson")] <- "English"
  x$department[which(x$first.name == "Leyla" & x$last.name == "Rouhi")] <- "English"
  x$department[which(x$first.name == "William" & x$last.name == "Darrow")] <- "Religion"
  x$department[which(x$first.name == "Barry" & x$last.name == "Goldstein")] <- "Art"
  x$department[which(x$first.name == "Mark" & x$last.name == "Taylor")] <- "Religion"

  ## Several more to complete here...

  x$raw <- NULL
  x
}
