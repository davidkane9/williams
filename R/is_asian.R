#' @title Is Asian?
#' @description Given a lastname, this function will flag it as "TRUE" (if asian), "FALSE" (if not asian),
#'              "NA" (if no data available). This is in accordance with data compiled by www.mongabay.com
#'              from the U.S. Bureau of the Census.
#' @param lastname lastname to flag
#' @param asian_lastnames data frame of the percentage of people with a given last name who identify as Asian
#' @return TRUE if majority of people with given lastname self identify as ASIAN
#'         FALSE if majority of people with given lastname do not self identify as ASIAN
#'         NA if no data is available
#' @export

is_asian <- function(lastname, asian_lastnames){
  lastname <- toupper(lastname)
  index <- which(asian_lastnames$lastname == lastname)
  if(length(index) == 0) {
    return(NA)
  }
  self_identifying_percent <- asian_lastnames$percent.asian[index[1]]
  return(ifelse(self_identifying_percent >= 50, TRUE, FALSE))
}
