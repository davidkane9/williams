#' @title Build Dataframe for all Available Years
#' @description This function builds independent dataframes for supplied years using the
#'              \code{build_annual_dataframe} and binds it together using row join operations.
#'
#' @param start_year (<YYYY>) starting year to build dataframes for (inclusive).
#'                   Default: 2000 (as of March 29, 2017)
#'
#' @param end_year (<YYYY>) end year to build dataframes for (inclusive)
#'                 Default: 2015 (as of March 29, 2017)
#'
#' @return dataframe with # rows = # graduates for the year,
#'                        # variabels = 10
#' @format
#' \describe{
#'     \item{firstname}{firstname of graduate}
#'     \item{middlename}{middlename(s) of graduate}
#'     \item{lastname}{lastname of graduate}
#'     \item{year}{year of graduation}
#'     \item{latin.honor}{latin honor recieved by graduate (if any)}
#'     \item{honor}{details of distinction of primary thesis honor (if any)
#'                    We define the "primary" thesis honor as the first honor
#'                    appearing alongside grad's name}
#'     \item{major}{major in which graduate completed primary honor}
#'     \item{honor.2}{details of distinction of secondary thesis honor (if any)
#'                   We define the "secondary" thesis honor as the second honor
#'                  appearing alongside grad's name}
#'     \item{major.2}{major in which graduate completed secondary honor}
#'     \item{raw.text}{graduation details as appearing in course catalog}
#' }
build_dataframes <- function(start_year = 2000, end_year = 2015) {
  years <- start_year:end_year
  do.call(rbind, lapply(years, build_annual_dataframe) )
}
