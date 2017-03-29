#' @title Get Female Probability
#' @description This function returns the probability that a given name belongs to a female student according
#'              to the \code{gender} package.
#' @param name name to predict gender for
#' @param predict_gender prediction table generated for all unique names in \code{graduates} dataset with
#'                       \code{gender} package
#' @return probability that given name belongs to a female student. If information for given name is not
#'         present in \code{gender} package, we will return NA
get_female_probability <- function(name, predict_gender){
  prediction <- predict_gender[which(predict_gender$name == name), ]$proportion_female
  if(length(prediction) == 0){
    return(NA)
  }else{
    return(prediction)
  }
}
