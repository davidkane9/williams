#' Sample of Students Admitted to Williams College
#'
#' A tibble containing information for 2,110 high school students admitted to
#' Williams College for class years ranging from 2011 through 2020. Note that
#' the raw source for this data was submitted anonymously to EphBlog. We can not
#' guarantee its accuracy. The data has been modified to make it impossible to
#' identify specific applicants, something that was possible in the raw data.
#' 
#' Original command to go from raw data to what we have here is:
#' 
#' read_csv("inst/admissions.csv") %>% mutate(enrolled = as.logical(enrolled)) %>% mutate(sex = recode(sex, "M" = "male", "F" = "female")) %>% mutate(country = recode(country, "United States" = "USA", .default = "foreign")) %>% mutate(SAT = reading + math) %>% rename(ACT = act, race = ethnicity, nationality = country) %>% select(-state, -math, -reading, -writing) %>% mutate(race = recode(race, "Asian American" = "Asian", "Hispanic/Latino"= "Hispanic", "Non-US" = NA_character_, "Unidentified" = NA_character_)) -> admits
#'
#' @source \url{https://www.ephblog.com}
#'
#' @format A tibble with 2,110 rows and 7 variables:
#' \describe{
#'     \item{class}{year of graduating class to which the applicant was 
#'        accepted. For example, 2019 means a student who applied, in the fall
#'        of 2014 for enrollment in the fall of 2015 and with a projected
#'        graduation in the spring of 2019.}
#'     \item{enrolled}{logical value indicating whether or not the student 
#'        enrolled at Williams}
#'     \item{nationality}{either "foreign" or "USA." Orginally data included 
#'        specific countries.}
#'     \item{race}{one of "Asian", "Black", "Hispanic", "Native American" 
#'        or "White." Values of "Non-US" and "Unidentified" in the original
#'        data were set to NA.}    
#'     \item{sex}{either "male" or "female." We believe that this is the birth 
#'        sex, as provided in the Common Ap.}
#'     \item{ACT}{composite ACT score}
#'     \item{SAT}{sum of the score on the math and reading portions of the SAT}
#' }
#'
"admits"
