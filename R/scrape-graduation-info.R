#' @title First Name Scraper
#' @description Scrapes graduates first name for honor details
#'
#'              As per the course catalog, graduation details are presented as:
#'              (1) <firstname> <midlename(s)> <lastname>, <honors details>
#'              or, (2) <firstname> <midlename(s)> <lastname>
#'
#'              This function will scrape the first name from the details. We do so by extracting
#'              the substring from the begining of the provided string, to just before the first
#'              occurance of a blankspace (" ").
#'
#' @param grad_details a graduates graduation details
#'                     supplied as: (1) <firstname> <midlename(s)> <lastname>, <honors details>
#'                              or, (2) <firstname> <midlename(s)> <lastname>
#' @return graduate's first name
scrape_first_name <- function(grad_details){
  #edge case for WGES
  grad_details <-   gsub(pattern = "Women’s, Gender and Sexuality Studies", x = grad_details, replacement = "WGES")
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

#' @title Last Name Scraper
#' @description Scrapes graduates last name for honor details
#'
#'               As per the course catalog, graduation details are presented as:
#'              (1) <firstname> <midlename(s)> <lastname>, <honors details>
#'              or, (2) <firstname> <midlename(s)> <lastname>
#'
#'              This function first checks if the details are in format (1) or (2) from above. If
#'              in format (2), we change it to format (1) by deleting the honor details. Hence,
#'              the grad_details are in <firstname> <midlename(s)> <lastname> format now.
#'
#'              Now, to extract the last name, we substring from the last occurance of blankspace (" ")
#'              to the end of the String.
#'
#' @param grad_details a graduates graduation details
#'                     supplied as: (1) <firstname> <midlename(s)> <lastname>, <honors details>
#'                              or, (2) <firstname> <midlename(s)> <lastname>
#' @return graduate's last name
scrape_last_name <- function(grad_details){
  #edge case for WGES
  grad_details <-   gsub(pattern = "Women’s, Gender and Sexuality Studies", x = grad_details, replacement = "WGES")
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

#' @title Middle Name Scraper
#' @description Scrapes graduates middle name for honor details
#'
#'              As per the course catalog, graduation details are presented as:
#'              (1) <firstname> <midlename(s)> <lastname>, <honors details>
#'              or, (2) <firstname> <midlename(s)> <lastname>
#'
#'              This function first checks if the details are in format (1) or (2) from above. If
#'              in format (2), we change it to format (1) by deleting the honor details. Hence,
#'              the grad_details are in <firstname> <midlename(s)> <lastname> format now.
#'
#'              Now, to extract the middle names, we substring from the first occurance of blankspace (" ")
#'              + 1 to the last occurance of blankspace - 1.
#'
#' @param grad_details a graduates graduation details
#'                     supplied as: (1) <firstname> <midlename(s)> <lastname>, <honors details>
#'                              or, (2) <firstname> <midlename(s)> <lastname>
#' @return graduate's middle name
scrape_middle_name <- function(grad_details){
  #edge case for WGES
  grad_details <-   gsub(pattern = "Women’s, Gender and Sexuality Studies", x = grad_details, replacement = "WGES")
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

#' @title Primary Honors Scraper
#' @description Scrapes primary honor details
#'              We define the "primary" honor as the first honor appearing alongside grad's name
#'
#'              This function extracts the distinction level ("NONE", "HONORS", or "HIGHEST") for the
#'              primary honor of a graduating student. As per the catalog's format, a grad_detail input
#'              is in the format (1) <firstname> <midlename(s)> <lastname>, <honors details>
#'                              or, (2) <firstname> <midlename(s)> <lastname>.
#'
#'              If \code{grad_details} are in format (1), then the student did not do a thesis,
#'              in which case, we return "NONE". Alternatively, if \code{grad_details} are in format (2),
#'              we first make sure to check if the student wrote a second thesis, and if so, we delete that
#'              information since it is not relevant to this function.
#'
#'              Finally, we check if the word "highest" occurs in the truncated string, and if so, the
#'              student has received highest honors (and we return "HIGHEST"). Else, we return "HONORS".
#'
#' @param grad_details a graduates graduation details
#'                     supplied as: (1) <firstname> <midlename(s)> <lastname>, <honors details>
#'                              or, (2) <firstname> <midlename(s)> <lastname>
#' @return (1) "NONE" if no honors
#'         (2) "HIGHEST" if grad recieved (highest) honors for thesis
#'         (3) "HONORS" if grad received (ordinary) honors for thesis
scrape_primary_honors <- function(grad_details) {
  #edge case for WGES
  grad_details <-   gsub(pattern = "Women’s, Gender and Sexuality Studies", x = grad_details, replacement = "WGES")
  prim_honors_start_index <- regexpr(",", grad_details) + 1

  if(prim_honors_start_index <= 0){ # format (1) ?? If yes, student didn't write a thesis
    return("NONE")
  }

  #  => format (2) --> extract thesis distinction
  grad_details <- substr(grad_details, prim_honors_start_index, nchar(grad_details))
  prim_honors_end_index <- regexpr(",", grad_details)
  if(prim_honors_end_index > 0){
    grad_details <- substr(grad_details, 1, prim_honors_end_index - 1)
  }
  if(regexpr("highest", grad_details) > 0){
    return("HIGHEST")
  } else {
    return("HONORS")
  }
}

#' @title Primary Honors Major Scraper
#' @description Scrapes primary honor details
#'              We define the "primary" honor as the first honor appearing alongside grad's name
#'
#'              This function extracts the major for the primary honor of a graduating student.
#'              As per the catalog's format, a grad_detail input is in the format
#'              (1) <firstname> <midlename(s)> <lastname>, <honors details>
#'              or, (2) <firstname> <midlename(s)> <lastname>.
#'
#'              If \code{grad_details} are in format (1), then the student did not do a thesis,
#'              in which case, we return "NONE". Alternatively, if \code{grad_details} are in format (2),
#'              we first make sure to check if the student wrote a second thesis, and if so, we delete that
#'              information since it is not relevant to this function.
#'
#'              Finally, we find the major by extracting the substring from after the occurance
#'              of " in ", till the end of the string.
#'
#' @param grad_details a graduates graduation details
#'                     supplied as: (1) <firstname> <midlename(s)> <lastname>, <honors details>
#'                              or, (2) <firstname> <midlename(s)> <lastname>
#' @return (1) "NONE" if no honors
#'         (2) <major> if grad did a thesis
scrape_primary_honors_major <- function(grad_details) {
  #edge case for WGES
  grad_details <-   gsub(pattern = "Women’s, Gender and Sexuality Studies", x = grad_details, replacement = "WGES")
  prim_honors_start_index <- regexpr(",", grad_details) + 1
  if(prim_honors_start_index <= 0){ # format (1) ?? If yes, student didn't write a thesis
    return("NONE")
  }
  #  => format (2) --> extract thesis major
  grad_details <- substr(grad_details, prim_honors_start_index, nchar(grad_details))
  prim_honors_end_index <- regexpr(",", grad_details)
  if(prim_honors_end_index > 0){
    grad_details <- substr(grad_details, 1, prim_honors_end_index - 1)
  }
  major_start_index <- regexpr(" in ", grad_details) + nchar(" in ")
  return(substr(grad_details, major_start_index, nchar(grad_details)))
}

#' @title Secondary Honors Scraper
#' @description Scrapes secondary honor details
#'              We define the "secondary" honor as the second honor appearing alongside grad's name
#'
#'              This function extracts the distinction level ("NONE", "HONORS", or "HIGHEST") for the
#'              secondary honor of a graduating student. As per the catalog's format, a grad_detail input
#'              is in the format (1) <firstname> <midlename(s)> <lastname>, <honors details>
#'                              or, (2) <firstname> <midlename(s)> <lastname>.
#'
#'              If \code{grad_details} are in format (1), then the student did not do a thesis,
#'              in which case, we return "NONE". Alternatively, if \code{grad_details} are in format (2),
#'              we first make sure to check if the student wrote a second thesis, and if not,
#'              we should once again, return "NONE".
#'
#'              If yes, we delete the information about the primary honor, so that our string just has
#'              details about the secondary honor. We now, check the resultant string for the
#'              occurance of "highest" and return "HIGHEST" or "HONORS" appropriately.
#'
#' @param grad_details a graduates graduation details
#'                     supplied as: (1) <firstname> <midlename(s)> <lastname>, <honors details>
#'                              or, (2) <firstname> <midlename(s)> <lastname>
#' @return (1) "NONE" if no honors
#'         (2) "HIGHEST" if grad recieved (highest) honors for thesis
#'         (3) "HONORS" if grad received (ordinary) honors for thesis
scrape_secondary_honors <- function(grad_details) {
  #edge case for WGES
  grad_details <-   gsub(pattern = "Women’s, Gender and Sexuality Studies", x = grad_details, replacement = "WGES")
  comma_indices <- gregexpr(",", grad_details)[[1]]
  if(length(comma_indices) < 2){ # format (1) or no secondary thesis??
    return("NONE")
  }

  # => student wrote a secondary thesis --> extract distinction level
  secondary_honors_start_index <- comma_indices[2] + 1
  secondary_honors_end_index <- nchar(grad_details)
  grad_details <- substr(grad_details, secondary_honors_start_index, secondary_honors_end_index)
  if(regexpr("highest", grad_details) > 0){
    return("HIGHEST")
  } else {
    return("HONORS")
  }
}

#' @title Secondary Honors Major Scraper
#' @description Scrapes secondary honor details
#'              We define the "secondary" honor as the second honor appearing alongside grad's name.
#'
#'              This function extracts the major for the secondary honor of a graduating student.
#'              As per the catalog's format, a grad_detail input is in the format
#'              (1) <firstname> <midlename(s)> <lastname>, <honors details>
#'              or, (2) <firstname> <midlename(s)> <lastname>.
#'
#'              If \code{grad_details} are in format (1), then the student did not do a thesis,
#'              in which case, we return "NONE". Alternatively, if \code{grad_details} are in format (2),
#'              we first make sure to check if the student wrote a second thesis, and if not,
#'              we should once again, return "NONE".
#'
#'              If yes, we delete the information about the primary honor, so that our string just has
#'              details about the secondary honor. We find the major by extracting the substring from after the occurance
#'               of " in ", till the end of the string.
#'
#' @param grad_details a graduates graduation details
#'                     supplied as: (1) <firstname> <midlename(s)> <lastname>, <honors details>
#'                              or, (2) <firstname> <midlename(s)> <lastname>
#' @return (1) "NONE" if no honors
#'         (2) <major> if grad did a second thesis
scrape_secondary_honors_major <- function(grad_details) {
  #edge case for WGES
  grad_details <-   gsub(pattern = "Women’s, Gender and Sexuality Studies", x = grad_details, replacement = "WGES")
  comma_indices <- gregexpr(",", grad_details)[[1]]
  if(length(comma_indices) < 2){ # format (1) or no secondary thesis??
    return("NONE")
  }
  # => student wrote a secondary thesis --> extract major
  secondary_honors_start_index <- comma_indices[2] + 1
  secondary_honors_end_index <- nchar(grad_details)
  grad_details <- substr(grad_details, secondary_honors_start_index, secondary_honors_end_index)
  major_start_index <- regexpr(" in ", grad_details) + nchar(" in ")
  return(substr(grad_details, major_start_index, nchar(grad_details)))
}
