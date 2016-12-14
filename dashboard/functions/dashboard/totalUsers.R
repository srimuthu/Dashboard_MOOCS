#' Query new enrollers
#' 
#' Definition: Number of sign-ups in at least 1 of the courses
#'
#' @param con Postgresql connection object returned by the \seealso{psql} function.
#' @param from Date from which you want to query (inclusive). Defaults to '1970-01-01'. Must be passed in YYYY-MM-DD format. 
#' @param to Date to which you want to query (inclusive). Defaults to date of today. Must be passed in YYYY-MM-DD format. 
#' 
#' @return Numeric vector of length 1 containing the number of enrollers.
#' 
#' @author Jasper Ginn
#'
#' @importFrom dplyr %>%, tbl, summarize, collect, filter, n()
#' 

newEnrollers <- function(con, from = "1970-01-01", to = as.character(Sys.Date())) {
  
  # From/to date --> check if right format ie. YYYY-MM-DD
  if(is.na(try(as.Date(from, format = "%Y-%m-%d")))) stop("Please enter a date in the following format: YYYY-MM-DD")
  if(is.na(try(as.Date(to, format = "%Y-%m-%d")))) stop("Please enter a date in the following format: YYYY-MM-DD")

  # Query users table
  r <- tbl(con, "course_memberships") %>%
    # Filter from / to date
    filter(course_membership_ts >= from,
           course_membership_ts <= to,
           course_membership_role == "LEARNER") %>%
    summarize(n()) %>%
    collect()
  
  # Select count and return
  return(r$count)
    
}
