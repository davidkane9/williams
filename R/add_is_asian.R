#' @title Add Is Asian? Column
#' @description This function loades in the "graduates" data, and adds a flag for asian lastnames.
#'              We flag a lastname as "asian" if a majority of people with the given lastname self-identified
#'              as Asian according to the U.S. Bureau of the Census
#' @return "graduates" dataframe with new column "is.asian" which is "TRUE" if we flag the lastname as asian,
#'         "FALSE" if we don't flag as asian (majority doesn't identify as Asian), or "NA" if we don't have
#'         data to make a guess.
add_is_asian <- function(){
  load("~karantibrewal/KT/williamsmetrics/inst/extdata/AsianLastnames.RData")
  load("~karantibrewal/KT/williamsmetrics/data/Graduates.RData")
  is.asian <- sapply(graduates$lastname, is_asian, asian_lastnames)
  graduates$is.asian <- is.asian
  return(graduates)
}
