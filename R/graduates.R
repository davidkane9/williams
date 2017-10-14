#' Williams College Graduates
#'
#' A tibble containing graduation information for 8,893 ephs (classes of 2001 to
#' 2016), extracted from the Williams College course catalog. 
#'
#' @source \url{https://catalog.williams.edu/archive/}
#'
#' @format A tibble with 8,893 rows and 10 variables:
#' \describe{
#'     \item{year}{year of graduation}
#'     \item{first.name}{first name of graduate}
#'     \item{last.name}{last name of graduate}
#'     \item{gender}{based on \code{first.name}, using the gender package.}
#'     \item{race}{based on \code{last.name}, using the wru package.}
#'     \item{latin.honors}{character, Latin Honors recieved by graduate, if any}
#'     \item{Phi.Beta.Kappa}{logical, indicates Phi Beta Kappa membership}
#'     \item{Sigma.Xi}{logical, indicates Sigma Xi membership}
#'     \item{honor}{first department honor awarded, if any. A handful
#'       of graduates receive honors in two departments. We provide only the 
#'       first. Values are "honors" or "highest honors"}
#'     \item{major}{character, academic field in which graduate won primary honor,
#'        if any}
#' }
#'
"graduates"
