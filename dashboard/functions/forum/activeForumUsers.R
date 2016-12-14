#' Get the number of unique active forum users per module.
#' 
#' @param con Postgresql connection object returned by the \seealso{psql} function.
#' @param includePrompts Whether or not to include discussion prompts as forum posts. Defaults to TRUE.
#' @param post_length_threshold Integer value detailing whether posts with a length below that value should be filtered from the results. Defaults to 50 characters.
#' @param from Date from which you want to query (inclusive). Must be passed in YYYY-MM-DD format. 
#' @param to Date to which you want to query (inclusive). Defaults to date of today. Must be passed in YYYY-MM-DD format. 
#' 
#' @author Jasper Ginn
#'
#' @return Numeric vector of length 1 containing the number of active forum users.
#'
#' @importFrom dplyr %>%, tbl, summarize, collect, filter, n(), select, distinct
#' 

activeForumUsers <- function(con, includePrompts = TRUE, post_length_threshold = 50, from = as.character(Sys.Date() - 30), to = as.character(Sys.Date())) {
  
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
  
  # Unique question ids
  f <- dq %>% 
    select(discussion_question_id) %>%
    distinct() %>%
    collect() %>%
    unlist() %>%
    unname()
  
  # Continue query
  dq2 <- dq %>%
    # Filter for posts below threshold
    filter(nchar(discussion_question_details) >= post_length_threshold) %>%
    # Filter for dates
    filter(discussion_question_created_ts >= from,
           discussion_question_created_ts <= to) %>%
    # Select variables of interest
    select(user_id = ends_with("user_id"),
           discussion_question_id) %>%
    # Filter for discussion ID
    filter(discussion_question_id %in% f) %>%
    select(user_id) %>%
    collect() %>%
    unlist() %>%
    unname()
  
  # Get unique users in responses
  da <- tbl(con, "discussion_answers") %>%
    # Filter for posts below threshold
    filter(nchar(discussion_answer_content) >= post_length_threshold) %>%
    # Filter for dates
    filter(discussion_answer_created_ts >= from,
           discussion_answer_created_ts <= to) %>%
    # Select variables
    select(user_id = ends_with("user_id"),
           discussion_question_id) %>%
    # Filter for discussion question id
    filter(discussion_question_id %in% f) %>%
    # Get unique students
    select(user_id) %>%
    collect() %>%
    unlist() %>%
    unname()
  
  # Bind and get unique values
  return(length(unique(c(dq2, da))))
  
}
