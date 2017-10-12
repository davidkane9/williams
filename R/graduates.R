#' Williams College Graduates
#'
#' A dataset containing graduation information for 8,893 ephs (classes of 2001 to 2016), extracted from the
#' Williams College course catalog. graduates_details is a similar data frame, with the same
#' number of rows and all the same variables. It also includes a dozen or so other variables.
#'
#' @source \url{https://catalog.williams.edu/archive/}
#'
#' @format A data frame with 8,893 rows and 10 variables:
#' \describe{
#'     \item{year}{numeric, year of graduation}
#'     \item{first.name}{character, first name of graduate}
#'     \item{last.name}{character, last name of graduate}
#'     \item{latin.honors}{character, Latin Honors recieved by graduate (if any)}
#'     \item{Phi.Beta.Kappa}{lgical, indicates Phi Beta Kappa membership}
#'     \item{Sigma.Xi}{lgical, indicates Sigma Xi membership}
#'     \item{honor}{character, first department honor awarded, if any.}
#'     \item{major}{character, academic field in which graduate won primary honor.}
#'     \item{gender}{character, based on \code{first.name}, using the gender package.}
#'     \item{race}{character, based on \code{last.name}, using the wru package.}
#' }
#'
"graduates"
