#' Williams College Faculty
#'
#' A dataset containing graduation information for Williams faculty from 2001 to 2016, extracted from the
#' Williams College course catalog.
#'
#' @source \url{http://web.williams.edu/admin/registrar/catalog/archive.html}
#' @format
#' \describe{
#'     \item{first.name}{First name of faculty}
#'     \item{last.name}{Last name of faculty}
#'     \item{year}{catalog year from which information is extracted}
#'     \item{leave.full.year}{Is faculty on leave for full academic year?}
#'     \item{leave.first.sem}{Is faculty on leave for only first semester?}
#'     \item{leave.first.sem}{Is faculty on leave for only second semester?}
#'     \item{leave.first.sem}{Is faculty on leave for full calendar year?}
#'     \item{first.degree}{First degree held by faculty; This is generally a bachelors degree}
#'     \item{first.degree.year}{Year the first degree was conferred}
#'     \item{first.degree.school}{School that conferred first degree}
#'     \item{last.degree}{First degree held by faculty; This is generally the highest degree held by the faculty.}
#'     \item{last.degree.year}{Year the last degree was conferred}
#'     \item{last.degree.school}{School that conferred last degree.}
#'     \item{title}{title of faculty}
#'     \item{rank}{rank of facutly; one of "Librarian", "Instructor", "Fellow", "Lecturer", "Visiting Professor",
#'     "Assistant Professor", "Associate Professor", "Professor". Otherwise, blank.}
#'     }
"faculty"
