#' @title Scrape Middle Name(s)
#' @description A student's graduation details is provided in the catalog in the following format:
#'              (1) <firstname> <midlename(s)> <lastname>, <theses details>
#'              or, (2) <firstname> <midlename(s)> <lastname>
#'
#'              Here, we define middle as the aphabetic sequence appearing in a student's
#'              name after (but not including) the first white space, and before (but not including)
#'              the last white space.
#'              For example, given graduation details, "DoHyun Tony Chung, with honors in Political Economy",
#'              we infer "Tony" as the student's middle
#'
#'@param grad_details Student's graduation details from catalog.
#'@return student's middle as infered by our definition:  we define middlename as the aphabetic sequence
#'        appearing in a student's name after (but not including) the first white space, and before
#'        (but not including) the last white space.
#'        For example, given graduation details, "DoHyun Tony Chung, with honors in Political Economy",
#'        we infer "Tony" as the student's middlename.
#' @export

scrape_middle_name <- function(grad_details){
  # delete honors details
  honors_index <- regexpr(",", grad_details)
  if(honors_index > 0){
    grad_details <- substr(grad_details, 1, honors_index - 1)
  }

  # now, substring
  space_indices <- gregexpr(" ", grad_details)[[1]]
  first_space_index <- space_indices[1]
  last_space_index <- space_indices[length(space_indices)]

  return(substr(grad_details, first_space_index + 1, last_space_index - 1))
}
