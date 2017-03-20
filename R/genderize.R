
#' @title Load Gender Predictions
#' @description This function extracts the unique first names from the \code{graduates} dataset, and
#'              uses the \code{gender} package to predict the gender for each first name. The result is
#'              compiled as a table with relevant columns \code{proportion_female} and
#'              code{proportion_male} and returned to the calling function.
#' @return A table containing gender prediction probabilities for each unique first name in the
#'         \code{graduates} dataset.
#'
load_gender_predictions <- function(){
  unique_first_names <- unique(as.character(graduates$firstname))
  predict_gender <- gender::gender(unique_first_names)
  return(predict_gender)
}

#' @title Get Male Probability
#' @description This function returns the probability that a given name belongs to a male student according
#'              to the \code{gender} package.
#' @param name name to predict gender for
#' @param predict_gender prediction table generated for all unique names in \code{graduates} dataset with
#'                       \code{gender} package
#' @return probability that given name belongs to a male student. If information for given name is not
#'         present in \code{gender} package, we will return NA
get_male_probability <- function(name, predict_gender){
  prediction <- predict_gender[which(predict_gender$name == name), ]$proportion_male
  if(length(prediction) == 0){
    return(NA)
  }else{
    return(prediction)
  }
}

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

#' @title Build Dataframe with Gender
#' @description This function adds \code{male_prob} and \code{female_prob} columns to the \code{graduate}
#'              dataset, and returns the new dataframe
#' @return new \code{graduates} dataframe with  \code{male_prob} and \code{female_prob} columns
build_gender_df <- function(){
  load("~karantibrewal/KT/williamsmetrics/data/graduates.RData")
  predict_gender <- load_gender_predictions()
  male_prob <- sapply(graduates$firstname, get_male_probability, predict_gender = predict_gender)
  female_prob <- sapply(graduates$firstname, get_female_probability, predict_gender = predict_gender)
  graduates$male_prob <- male_prob
  graduates$female_prob <- female_prob
  return(graduates)
}

