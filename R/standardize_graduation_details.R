#' @title Standardize Graduation Details
#' @description This function standardizes a student's graduation details to account for changes
#'              in the catalogs over the years.
#' @param grad_details A student's graduation details
#' @return Standardizes discrepencies in graduation_details (if any)
#' @export

standardize_graduation_details <- function(grad_details){

  #edge case (1) for WGES
  grad_details <- gsub(pattern = "Women’s, Gender and Sexuality Studies",
                         x = grad_details, replacement = "WGES")
  grad_details <- gsub(pattern = "Women’s, Gender & Sexuality Studies",
                         x = grad_details, replacement = "WGES")
}
