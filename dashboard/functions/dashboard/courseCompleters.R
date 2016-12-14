#' Calculate the number of course completers
#' 
#' Definition: Number of course completers  in at least 1 of the courses
#' 
#' @param con Postgresql connection object returned by the \seealso{psql} function.
#' @param from Date from which you want to query (inclusive). Must be passed in YYYY-MM-DD format. 
#' @param to Date to which you want to query (inclusive). Defaults to date of today. Must be passed in YYYY-MM-DD format. 
#' 
#' @return Numeric vector of length 1 containing the number of course completers.
#' 
#' @author Jasper Ginn
#'
#' @importFrom dplyr %>%, tbl, summarize, collect, filter, n(), select, distinct
#' 

courseCompleters <- function(con, from = as.character(Sys.Date() - 30), to = as.character(Sys.Date())) {
  
  # From/to date --> check if right format ie. YYYY-MM-DD
  if(is.na(try(as.Date(from, format = "%Y-%m-%d")))) stop("Please enter a date in the following format: YYYY-MM-DD")
  if(is.na(try(as.Date(to, format = "%Y-%m-%d")))) stop("Please enter a date in the following format: YYYY-MM-DD")

  # Calculate the number of people who passed course
  r <- tbl(con, "course_grades") %>%
    # Drop all columns we won't need
    select(-course_id, -course_grade_overall_passed_items, -course_grade_overall, -course_grade_verified_passed_items, -course_grade_verified) %>%
    # Filter for dates --> course_grade_ts: 'The timestamp for when a learner's course grade has changed. This occurs everytime a grading event takes place.'
    # Filter for passing states 1 + 2
    filter(course_grade_ts >= from,
           course_grade_ts <= to,
           course_passing_state_id %in% c(1,2)) %>%
    # Drop columns
    select(-course_grade_ts, -course_passing_state_id) %>%
    # Distinct users
    distinct() %>%
    # Count rows
    summarize(n()) %>%
    collect()
  
  # Return
  return(r$count)
  
}