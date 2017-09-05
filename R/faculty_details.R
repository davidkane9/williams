#' Williams College Faculty Details
#'
#' A dataset containing information for all members of the 
#' Williams faculty since 2000, contrudcted using information extracted 
#' from the Williams College course catalog. See also the \code{faculty} 
#' data frame.
#'
#' @source \url{http://web.williams.edu/admin/registrar/catalog/archive.html}
#' @format
#' \describe{
#'     \item{year}{Catalog year from which information is extracted}
#'     \item{first.name}{First name of faculty member}
#'     \item{last.name}{Last name of faculty member}
#'     \item{title}{Title of faculty member}
#'     \item{department}{Department of faculty member}
#'     \item{rank}{Rank of faculty member: one of "Librarian", "Instructor", 
#'         "Fellow", "Lecturer", "Visiting Professor",
#'         "Assistant Professor", "Associate Professor", "Professor". Otherwise, blank.} 
#'     \item{status}{Teaching status of faculty member: one of "Part-time", 
#'          "Tenure-track", "Tenured", or "Visiting". Otherwise, blank.}
#'     \item{leave}{Faculty member's leave status. One of "Academic Year",   
#'           "Calendar Year", "First Semester", "None", or "Second Semester".} 
#'     \item{birth.year}{Estimated year of birth, based on the assumption 
#'            that the first degree was awarded at age 22.}            
#'     \item{first.degree}{First degree held by faculty. This is generally 
#'            a bachelors degree}
#'     \item{first.degree.year}{Year the first degree was conferred}
#'     \item{first.degree.school}{School that conferred first degree}
#'     \item{last.degree}{Last degree held by faculty; This is generally the highest degree held by the faculty.}
#'     \item{last.degree.year}{Year the last degree was conferred}
#'     \item{last.degree.school}{School that conferred last degree.}
#'     \item{gender}{Estimated gender.}
#'     \item{race}{Estimated race: "Asian", "Black", "Hispanic", "Other", "White"}
#'     }
"faculty_details"

