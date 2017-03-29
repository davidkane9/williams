#' @title Scrape Last Name
#' @description A student's graduation details is provided in the catalog in the following format:
#'              (1) <firstname> <midlename(s)> <lastname>, <theses details>
#'              or, (2) <firstname> <midlename(s)> <lastname>
#'
#'              Here, we define lastname as the aphabetic sequence appearing in a student's
#'              name after (but not including) the last white space. For example, given graduation
#'              details, "DoHyun Tony Chung, with honors in Political Economy", we infer "Chung" as
#'              the student's lastname.
#'
#'@param grad_details Student's graduation details from catalog.
#'@return student's lastname as infered by our definition:  we define lastname as the aphabetic sequence
#'        appearing in a student's name after (but not including) the last white space. For example,
#'        given graduation details, "DoHyun Tony Chung, with honors in Political Economy", we infer "Chung" as
#'        the student's lastname.
scrape_last_name <- function(grad_details){
  # delete honors details
  honors_index <- regexpr(",", grad_details)
  if(honors_index > 0){
    grad_details <- substr(grad_details, 1, honors_index - 1)
  }

  # now, substring
  space_indices <- gregexpr(" ", grad_details)[[1]]
  last_space_index <- space_indices[length(space_indices)]
  if(last_space_index <= 1 || last_space_index == nchar(grad_details)){
    stop(paste("No last name in graduation details...\n", grad_details, sep = ""))
  }

  return(substr(grad_details, last_space_index + 1, nchar(grad_details)))
}
