#' Average grade for all passed users
#' 
#' @param con Postgresql connection object returned by the \seealso{psql} function.
#' @param from Date from which you want to query (inclusive). Defaults to '1970-01-01'. Must be passed in YYYY-MM-DD format. 
#' @param to Date to which you want to query (inclusive). Defaults to date of today. Must be passed in YYYY-MM-DD format. 
#' @param digits Number of digits used to round return value. Defaults to 3.
#' 
#' @return Numeric vector of length 1 containing the average grade rounded to three decimals.
#' 
#' @author Jasper Ginn
#'
#' @importFrom dplyr %>%, tbl, summarize, collect, filter, n()
#' 

averageGrade <- function(con, from = "1970-01-01", to = as.character(Sys.Date()), digits = 3) {
  
  # From/to date --> check if right format ie. YYYY-MM-DD
  if(is.na(try(as.Date(from, format = "%Y-%m-%d")))) stop("Please enter a date in the following format: YYYY-MM-DD")
  if(is.na(try(as.Date(to, format = "%Y-%m-%d")))) stop("Please enter a date in the following format: YYYY-MM-DD")
  
  # Query average grades
  r <- tbl(con, "course_grades") %>%
    # Filter for dates & students who have passed
    filter(course_grade_ts >= from,
           course_grade_ts <= to,
           !course_passing_state_id %in% c(0,3)) %>%
    # Take average of grade
    summarize(avg = mean(course_grade_overall)) %>%
    collect()
  
  # Return
  return(round(r$avg, digits = digits))
  
}
