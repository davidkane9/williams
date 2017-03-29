#' @title Standardize Graduation Details
#' @description This function standardizes a student's graduation details to account for discrepancies
#'              in the catalogs over the years. Each edge case is described in this description in detail.
#'
#'             (1) "Women's, Gender and Sexuality", "Women's Gender & Sexuality" are treated as one
#'                  and replaced as "WGES".
#' @param grad_details A student's graduation details
#' @return Standardizes deiscrepencies in graduation_details (if any)
standardize_graduation_details <- function(grad_details){

  #edge case (1) for WGES
  grad_details <- gsub(pattern = "Women’s, Gender and Sexuality Studies",
                         x = grad_details, replacement = "WGES")
  grad_details <- gsub(pattern = "Women’s, Gender & Sexuality Studies",
                         x = grad_details, replacement = "WGES")
}
