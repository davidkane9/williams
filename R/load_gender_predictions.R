#' @title Load Gender Predictions
#' @description This function extracts the unique first names from the \code{graduates} dataset, and
#'              uses the \code{gender} package to predict the gender for each first name. The result is
#'              compiled as a table with relevant columns \code{proportion_female} and
#'              code{proportion_male} and returned to the calling function.
#' @return A table containing gender prediction probabilities for each unique first name in the
#'         \code{graduates} dataset.
#'
#' @export

load_gender_predictions <- function(){
  load("~karantibrewal/KT/williamsmetrics/data/graduates.RData")
  unique_first_names <- unique(as.character(graduates$firstname))
  predict_gender <- gender::gender(unique_first_names)
  return(predict_gender)
}
