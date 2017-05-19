#' Williams College Graduates
#'
#' A dataset containing graduation information for 8,357 ephs (classes of 2001 to 2016), extracted from the
#' Williams College course catalog. This dataset supplements the graduates dataset by adding a dozen or so more
#' columns.
#'
#' @source \url{http://web.williams.edu/admin/registrar/catalog/archive.html}
#' @source \url{https://cran.r-project.org/web/packages/gender/README.html}
#' @source \url{https://cran.r-project.org/web/packages/wru/index.html}
#'
#' @aliases graduates_details
#'
#' @format A data frame with 8,357 rows and 10 variables:
#' \describe{
#'     \item{year}{numeric, year of graduation}
#'     \item{first.name}{character, first name of graduate}
#'     \item{last.name}{character, last name of graduate}
#'     \item{latin.honors}{character, Latin Honors recieved by graduate (if any)}
#'     \item{Phi.Beta.Kappa}{lgical, indicates Phi Beta Kappa membership}
#'     \item{Sigma.Xi}{lgical, indicates Sigma Xi membership}
#'     \item{honor.1}{character, details of distinction of primary thesis honor (if any)
#'                    We define the "primary" thesis honor as the first honor
#'                    appearing alongside grad's name}
#'     \item{major.1}{character, major in which graduate completed primary honor}
#'     \item{honor.2}{character, details of distinction of secondary thesis honor (if any)
#'                   We define the "secondary" thesis honor as the second honor
#'                  appearing alongside grad's name}
#'     \item{major.2}{character, major in which graduate completed secondary honor}
#'     \item{honor}{character, first department honor awarded, if any.}
#'     \item{major}{character, academic field in which graduate won primary honor.}
#'     \item{gender}{character, as determined by \code{gender/genderizeR} package.}
#'     \item{p_male.first}{numeric, probability that the individual is a male given their first name, as determined by the
#'     \code{gender} packege}
#'     \item{p_female.first}{numeric, probability that the individual is a female given their first name, as determined by the
#'     \code{gender} packege}
#'     \item{gender.first}{character,gender of an individual given their first name, as determined by the  \code{gender} packege}
#'     \item{p_male.middle}{numeric, probability that the individual is a male given their middle name, as determined by the
#'     \code{gender} packege}
#'     \item{p_female.middle}{numeric, probability that the individual is a female given their middle name, as determined by the
#'     \code{gender} packege}
#'     \item{gender.middle}{character,gender of an individual given their middle name, as determined by the  \code{gender} packege}
#'     \item{gender.g}{character, gender of an individual, as determined by \code{genderizeR} package}
#'     \item{race}{character, based on \code{last.name}, using the wru package.}
#'     \item{p_whi}{numeric, prosterior probability that the individual's race is White, as determined by the \code{wru} package}
#'     \item{p_bla}{numeric, prosterior probability that the individual's race is Black, as determined by the \code{wru} package}
#'     \item{p_asi}{numeric, prosterior probability that the individual's race is Asian, as determined by the \code{wru} package}
#'     \item{p_his}{numeric, prosterior probability that the individual's race is Hispanic, as determined by the \code{wru} package}
#'     \item{p_oth}{numeric, prosterior probability that the individual's race is none of the above, as determined by the \code{wru} package}
#'
#' }
#'
"graduates_details"
