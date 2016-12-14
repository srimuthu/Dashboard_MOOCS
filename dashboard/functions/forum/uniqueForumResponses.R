#' Get the number of unique forum responses
#' 
#' @param con Postgresql connection object returned by the \seealso{psql} function.
#' @param includePrompts Whether or not to include discussion prompts as forum posts. Defaults to TRUE.
#' @param post_length_threshold Integer value detailing whether posts with a length below that value should be filtered from the results. Defaults to 50 characters.
#' @param from Date from which you want to query (inclusive). Must be passed in YYYY-MM-DD format. 
#' @param to Date to which you want to query (inclusive). Defaults to date of today. Must be passed in YYYY-MM-DD format. 
#' 
#' @author Jasper Ginn
#'
#' @return Numeric vector of length 1 containing the number of unique forum responses
#'
#' @importFrom dplyr %>%, tbl, summarize, collect, filter, n(), select, distinct
#' 

forumResponses <- function(con, includePrompts = TRUE, post_length_threshold = 50, from = as.character(Sys.Date() - 30), to = as.character(Sys.Date())) {
  
  # From/to date --> check if right format ie. YYYY-MM-DD
  if(is.na(try(as.Date(from, format = "%Y-%m-%d")))) stop("Please enter a date in the following format: YYYY-MM-DD")
  if(is.na(try(as.Date(to, format = "%Y-%m-%d")))) stop("Please enter a date in the following format: YYYY-MM-DD")
  
  # If includePrompts is TRUE
  if(includePrompts) {
    dq <- tbl(con, "discussion_questions")
  } else {
    dq <- tbl(con, "discussion_questions") %>%
      # Filter everything with course item id to keep posts initiated by moderators or students
      filter(is.na(course_item_id)) 
  }
  
  # Continue query
  dq2 <- dq %>% 
    # Combine with discussion forums data
    left_join(., tbl(con, "discussion_course_forums"),
              by = "discussion_forum_id") %>% 
    # Filter for dates
    filter(discussion_question_created_ts >= from,
           discussion_question_created_ts <= to) %>%
    # Count number of distinct forum ids and return
    summarize(count = n_distinct(discussion_forum_id)) %>%
    collect()
  
  # Return
  return(dq2$count)
  
}
