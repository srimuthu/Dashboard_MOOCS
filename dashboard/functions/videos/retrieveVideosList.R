#' Get a dataframe with all videos for a course.
#' 
#' Graded quizzes are further split in branches (when you have different versions of the same course) and versions.
#' 
#' @param con Postgresql connection object returned by the \seealso{psql} function.
#' @param from Date from which you want to query (inclusive). Must be passed in YYYY-MM-DD format. 
#' @param to Date to which you want to query (inclusive). Defaults to date of today. Must be passed in YYYY-MM-DD format. 
#' 
#' @author S.M.N. Balasubramanian
#'
#' @return 
#'
#' @importFrom dplyr %>%, tbl, summarize, collect, filter, n(), select, distinct

retrieveVideosList <- function(con, from = as.character(Sys.Date() - 30), 
                                to = as.character(Sys.Date())) {
  
  # From/to date --> check if right format ie. YYYY-MM-DD
  if(is.na(try(as.Date(from, format = "%Y-%m-%d")))) stop("Please enter a date in the following format: YYYY-MM-DD")
  if(is.na(try(as.Date(to, format = "%Y-%m-%d")))) stop("Please enter a date in the following format: YYYY-MM-DD")
  
  # Retrieve data frame of Videos
  vl <- tbl(con, "course_items") %>%
    # Select course_item_id and course_item_name
    select(., course_item_id, course_item_name, course_item_type_id) %>%
    # Filter by item type "video" type = 1
    filter(course_item_type_id == 1) %>%
    # Join with course_progress tables
    inner_join(tbl(con, "course_progress"), by="course_item_id") %>%
    # Drop variables
    select(-course_item_type_id) %>%
    
    collect()
}
    