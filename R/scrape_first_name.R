#' @title Scrape First Name
#' @description A student's graduation details is provided in the catalog in the following format:
#'              (1) <firstname> <midlename(s)> <lastname>, <theses details>
#'              or, (2) <firstname> <midlename(s)> <lastname>
#'
#'              Here, we define firstname as the first aphabetic sequence appearing in a student's
#'              name uptill (but not including) the first white space. For example, given graduation
#'              details, "DoHyun Tony Chung, with honors in Political Economy", we infer "DoHyun" as
#'              the student's firstname.
#'
#'@param grad_details Student's graduation details from catalog.
#'@return student's firstname as infered by our definition: we define firstname as the first aphabetic sequence appearing in a student's
#'              name uptill (but not including) the first white space. For example, given graduation
#'              details, "DoHyun Tony Chung, with honors in Political Economy", we infer "DoHyun" as
#'              the student's firstname.
scrape_first_name <- function(grad_details){
  # delete theses details
  if(regexpr(",", grad_details) > 0) {
    grad_details <- substr(grad_details, 1, regexpr(",", grad_details) - 1)
  }
  start_pos <- 1
  end_pos <- regexpr(' ', grad_details) - 1
  if(end_pos <= 1) {
    stop(paste("No first name in graduation details...\n", grad_details, sep = ""))
  }
  return(substr(grad_details, start_pos, end_pos))
}
