#' Get the number of unique active forum post initiators
#' 
#' @param con Postgresql connection object returned by the \seealso{psql} function.
#' @param post_length_threshold Integer value detailing whether posts with a length below that value should be filtered from the results. Defaults to 50 characters.
#' @param from Date from which you want to query (inclusive). Must be passed in YYYY-MM-DD format. 
#' @param to Date to which you want to query (inclusive). Defaults to date of today. Must be passed in YYYY-MM-DD format. 
#' 
#' @author Jasper Ginn
#'
#' @return Numeric vector of length 1 containing the number of active forum post initiators
#'
#' @importFrom dplyr %>%, tbl, summarize, collect, filter, n(), select, distinct
#' 

activeForumPostInitiators <- function(con, post_length_threshold = 50, from = as.character(Sys.Date() - 30), to = as.character(Sys.Date())) {
  
  # From/to date --> check if right format ie. YYYY-MM-DD
  if(is.na(try(as.Date(from, format = "%Y-%m-%d")))) stop("Please enter a date in the following format: YYYY-MM-DD")
  if(is.na(try(as.Date(to, format = "%Y-%m-%d")))) stop("Please enter a date in the following format: YYYY-MM-DD")
  
  # Get data
  dq <- tbl(con, "discussion_questions") %>%
    # Filter everything with course item id to keep posts initiated by moderators or students
    filter(is.na(course_item_id)) %>%
    # Filter for dates
    filter(discussion_question_created_ts >= from,
           discussion_question_created_ts <= to) %>%
    # Filter for posts below threshold
    filter(nchar(discussion_question_details) >= post_length_threshold) %>%
    # Select variables of interest
    select(user_id = ends_with("user_id")) %>%
    summarize(count = n_distinct(user_id)) %>%
    collect()
  
  # Return
  return(dq$count)
  
}
