#' @title Build Dataframe with Gender
#' @description This function adds \code{male_prob} and \code{female_prob} columns to the \code{graduate}
#'              dataset, and returns the new dataframe
#' @return new \code{graduates} dataframe with  \code{male_prob} and \code{female_prob} columns
#' @export

build_gender_df <- function(){
  predict_gender <- load_gender_predictions()
  male_prob <- sapply(graduates$firstname, get_male_probability, predict_gender = predict_gender)
  female_prob <- sapply(graduates$firstname, get_female_probability, predict_gender = predict_gender)
  graduates$male_prob <- male_prob
  graduates$female_prob <- female_prob
  graduates$gender <- ifelse(graduates$male_prob > 0.50, "MALE", "FEMALE")
  return(graduates)
}
