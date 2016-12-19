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

activeForumPostResponders <- function(con, post_length_threshold = 50, from = as.character(Sys.Date() - 30), to = as.character(Sys.Date())) {
  
  # From/to date --> check if right format ie. YYYY-MM-DD
  if(is.na(try(as.Date(from, format = "%Y-%m-%d")))) stop("Please enter a date in the following format: YYYY-MM-DD")
  if(is.na(try(as.Date(to, format = "%Y-%m-%d")))) stop("Please enter a date in the following format: YYYY-MM-DD")
  
  # Get data
  dq <- tbl(con, "discussion_questions") %>% 
    # Filter everything with course item id to keep posts initiated by moderators or students
    filter(is.na(course_item_id)) %>%
    # Select variables of interest
    select(discussion_question_id) %>%
    # Join with discussion answers
    inner_join(tbl(con, "discussion_answers") %>%
                 # Filter for posts below threshold
                 filter(nchar(discussion_answer_content) >= post_length_threshold) %>%
                # Filter for dates
                filter(discussion_answer_created_ts >= from,
                       discussion_answer_created_ts <= to) %>%
                # Select user id
                select(discussion_question_id,
                       user_id = ends_with("user_id")),
              by = "discussion_question_id") %>%
    # Get number of unique users
    summarize(count = n_distinct(user_id)) %>%
    collect()
  
  # Return
  return(dq$count)
  
}
