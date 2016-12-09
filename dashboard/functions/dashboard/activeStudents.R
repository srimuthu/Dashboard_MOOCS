#' Active students in past week
#' 
#' Definition: Number of students who watched a video, started/completed an assignment, downloaded an assignment etc.  in at least 1 of the courses
#' 
#' @param con Postgresql connection object returned by the \seealso{psql} function.
#' @param from Date from which you want to query (inclusive). Must be passed in YYYY-MM-DD format. 
#' @param to Date to which you want to query (inclusive). Defaults to date of today. Must be passed in YYYY-MM-DD format. 
#' 
#' @author Jasper Ginn
#'
#' @importFrom dplyr %>%, tbl, summarize, collect, filter, n(), select, distinct
#' 

activeStudents <- function(con, from = as.character(Sys.Date() - 30), to = as.character(Sys.Date())) {
  
  # From/to date --> check if right format ie. YYYY-MM-DD
  if(is.na(try(as.Date(from, format = "%Y-%m-%d")))) stop("Please enter a date in the following format: YYYY-MM-DD")
  if(is.na(try(as.Date(to, format = "%Y-%m-%d")))) stop("Please enter a date in the following format: YYYY-MM-DD")
  
  # Query table for course progress
  r <- tbl(con, "course_progress") %>%
    # Drop columns we don't need
    select(-course_id, -course_progress_state_type_id, -course_item_id) %>%
    # Filter for from/to dates
    filter(course_progress_ts >= from,
           course_progress_ts <= to) %>%
    # This gives us a data frame with all actions for all users
    # The return value is simply the number of unique students
    # Note that instead of selecting the users, we simply take out all other variables.
    # This is necessary because the name of the user_id is preceded by the institution name
    # As such, it is different for each institution
    select(-course_progress_ts) %>%
    # Count number of unique students
    distinct() %>%
    # Number of rows
    summarise(n()) %>%
    collect()
  
  # Return
  return(r$count)
  
}

