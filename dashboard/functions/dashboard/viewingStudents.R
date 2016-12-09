#' Calculate the number of 'viewers'
#' 
#' Definition: Number of people who engage with content except (graded) assignments  in at least 1 of the courses
#' 
#' @param con Postgresql connection object returned by the \seealso{psql} function.
#' @param from Date from which you want to query (inclusive). Must be passed in YYYY-MM-DD format. 
#' @param to Date to which you want to query (inclusive). Defaults to date of today. Must be passed in YYYY-MM-DD format. 
#' 
#' @author Jasper Ginn
#'
#' @importFrom dplyr %>%, tbl, summarize, collect, filter, n(), select, distinct
#' 

viewingStudents <- function(con, from = as.character(Sys.Date() - 30), to = as.character(Sys.Date())) {
  
  # From/to date --> check if right format ie. YYYY-MM-DD
  if(is.na(try(as.Date(from, format = "%Y-%m-%d")))) stop("Please enter a date in the following format: YYYY-MM-DD")
  if(is.na(try(as.Date(to, format = "%Y-%m-%d")))) stop("Please enter a date in the following format: YYYY-MM-DD")
  
  # Query results
  r <- tbl(con, "course_item_types") %>%
    # Join with course_items table
    inner_join(tbl(con, "course_items"), by="course_item_type_id") %>%
    # Select course_item_id
    select(., course_item_id, course_item_type_graded) %>%
    # Join with course_progress table
    inner_join(tbl(con, "course_progress") %>% 
                 filter(course_progress_ts >= from, 
                        course_progress_ts <= to), 
               by = "course_item_id") %>%
    # Rename user id
    select(course_item_id, 
           course_item_type_graded,
           user_id = ends_with("user_id")) %>%
    # Filter where graded information is NA
    filter(!is.na(course_item_type_graded)) %>%
    # Mutate TRUE/FALSE to 1/0
    mutate(course_item_type_graded = ifelse(course_item_type_graded, 1, 0)) %>%
    # Group by user id
    group_by(user_id) %>%
    # Per user, count the number of graded assignments
    summarize(number_graded_assignments = sum(course_item_type_graded)) %>%
    # Filter any student who has more than 0 graded assignments
    filter(number_graded_assignments == 0) %>%
    # Count rows
    summarize(n()) %>%
    collect()

  # Return value
  return(r$count)
  
}
