#' Sample of Students Admitted to Williams College
#'
#' A tibble containing information for 2,110 high school students admitted to
#' Williams College for class years ranging from 2011 through 2020. Note that
#' the raw source for this data was submitted anonymously to EphBlog. We can not
#' guarantee its accuracy. The data has been modified to make it impossible to
#' identify specific applicants, something that was possible in the raw data.
#' 
#' The data is, obviously, not a complete set of all the applicants admitted to
#' Williams over this time period. For example, it includes no information from
#' the classes of 2012 and 2018. The largest number of applicants in any year is
#' 391 for the class of 2017, but that is only a small portion of the more than
#' 1,200 applicants that Williams accepts in most years.
#' 
#' However, the data seems reasonable. For example, the mean SAT score for
#' Black/White enrolled students is 1274/1480. \emph{Race and Class Matters at an
#' Elite College} by Elizabeth Aries reports (p. 22) a similar gap 1284/1488 for
#' an older sample at Amherst College, a school with a very similar profile to
#' Williams.
#' 
#' @source \url{http://www.ephblog.com}
#'
#' @format A tibble with 2,110 rows and 7 variables:
#' \describe{
#'     \item{class}{year of graduating class to which the applicant was 
#'        accepted. For example, 2019 means a student who applied, in the fall
#'        of 2014 for enrollment in the fall of 2015 and with a projected
#'        graduation in the spring of 2019.}
#'     \item{enrolled}{logical value indicating whether or not the student 
#'        enrolled at Williams}
#'     \item{nationality}{either "foreign" or "USA." Orginally data included 
#'        specific countries.}
#'     \item{race}{one of "Asian", "Black", "Hispanic", "Native American" 
#'        or "White." Values of "Non-US" and "Unidentified" in the original
#'        data were set to NA.}    
#'     \item{sex}{either "male" or "female." We believe that this is the birth 
#'        sex, as provided in the Common Ap.}
#'     \item{ACT}{composite ACT score}
#'     \item{SAT}{sum of the score on the math and reading portions of the SAT}
#' }
#'
"admits"
