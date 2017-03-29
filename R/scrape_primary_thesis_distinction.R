#' @title Scrape Primary Honor Distinction
#' @description A student's graduation details is provided in the catalog in the following format:
#'              (1) <firstname> <midlename(s)> <lastname>, <theses details>
#'              or, (2) <firstname> <midlename(s)> <lastname>
#'
#'              This function concerns itself with the <theses details>. In turn, <theses details> are
#'              provided in the following format:
#'              <thesis 1 details>, <thesis 2 details>
#'              where each <thesis N detail> is in the format "with <distinction> honors in <major>"
#'
#'              Here we define the first thesis as the "Primary Thesis". This function will scrape the
#'              distinction for the Primary Thesis which maybe one of the following: "HIGHEST" (for highest
#'              level of honors), "HONORS" (for thesis completion), "NONE" (if no thesis was written).
#'@param grad_details A student's graduation details as appearing in the catalog.
#'@return Student's primary honors distinction level (i.e. "HIGHEST", "HONORS", or "NONE")
scrape_primary_thesis_distinction <- function(grad_details){
  prim_honors_start_index <- regexpr(",", grad_details) + 1
  if(prim_honors_start_index <= 0){ # format (1) ?? If yes, student didn't write a thesis
    return("NONE")
  }
  #  => format (2) --> extract thesis distinction
  grad_details <- substr(grad_details, prim_honors_start_index, nchar(grad_details))
  prim_honors_end_index <- regexpr(",", grad_details)
  # remove secondary thesis details (if any)
  if(prim_honors_end_index > 0){
    grad_details <- substr(grad_details, 1, prim_honors_end_index - 1)
  }
  # check for distinction
  if(regexpr("highest", grad_details) > 0){
    return("HIGHEST")
  } else {
    return("HONORS")
  }
}
