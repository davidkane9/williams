#' Williams College Faculty
#'
#' A tibble containing information for the 2016-2017 members 
#' of the Williams College faculty using information extracted from the 
#' course catalog. Created by running \code{create_faculty()}.
#'
#' @source \url{https://catalog.williams.edu/archive/}
#' @format A tibble with 428 rows and 17 variables.
#' \describe{
#'     \item{year}{catalog start year from which information is extracted}
#'     \item{first.name}{first name of faculty member}
#'     \item{last.name}{last name of faculty member}
#'     \item{title}{title of faculty member}
#'     \item{department}{department of faculty member}
#'     \item{rank}{rank of faculty member: one of "Instructor", 
#'         "Fellow", "Lecturer", "Visiting Professor", "Artist-in-Residence",
#'         "Assistant Professor", "Associate Professor", "Professor".
#'         "Librarian" is a rank sometimes used in previous years.} 
#'     \item{status}{teaching status of faculty member: one of  
#'          "Tenure-track", "Tenured", "Athletic" or "Visiting". 
#'          "Part-time" has been used in previous years.}
#'     \item{leave}{faculty member's leave status. One of "Academic Year",   
#'           "First Semester", "None", or "Second Semester". "Calendar Year" has
#'           been used in previous years.} 
#'     \item{birth.year}{estimated year of birth, based on the assumption 
#'            that the first degree was awarded at age 22.}            
#'     \item{first.degree}{first degree held by faculty. This is generally 
#'            a bachelors degree}
#'     \item{first.degree.year}{year the first degree was conferred}
#'     \item{first.degree.school}{school that conferred first degree}
#'     \item{last.degree}{last degree held by faculty; This is generally the highest degree held by the faculty.}
#'     \item{last.degree.year}{year the last degree was conferred}
#'     \item{last.degree.school}{school that conferred last degree}
#'     \item{gender}{character, based on \code{first.name}, using the gender package.}
#'     \item{race}{character, based on \code{last.name}, mainly using the wru package. 
#'         Values are "Asian", "Black", "Hispanic", "White"}
#'     }
"faculty"
