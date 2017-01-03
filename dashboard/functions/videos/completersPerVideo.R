#' get number of completers for a video
#' 
#' Definition: Number of completers for a video
#'
#' @param con Postgresql connection object returned by the \seealso{psql} function.
#' 
#' @return Numeric vector of length 1 containing the number of completers for a video
#' 
#' @author S.M.N. Balasubramanian
#'
#' @importFrom dplyr %>%, tbl, summarize, collect, filter, n()
#' 

completersPerVideo <- function(con, vid, from = as.character(Sys.Date() - 30), 
                                to = as.character(Sys.Date())) {
  
  # From/to date --> check if right format ie. YYYY-MM-DD
  if(is.na(try(as.Date(from, format = "%Y-%m-%d")))) stop("Please enter a date in the following format: YYYY-MM-DD")
  if(is.na(try(as.Date(to, format = "%Y-%m-%d")))) stop("Please enter a date in the following format: YYYY-MM-DD")

  # Retrieve data frame of Videos
  cpv <- tbl(con, "course_items") %>%
    # Select course_item_id and course_item_name
    select(., course_item_id, course_item_name, course_item_type_id) %>%
    # Filter by item type "video" type = 1
    filter(course_item_type_id == 1) %>%
    # Join with course_progress tables
    inner_join(tbl(con, "course_progress"), by="course_item_id") %>%
    # Filter for completers of a specific video
    filter(course_item_name == vid,
               course_progress_state_type_id == 2) %>%
    
    summarize(n()) %>% 

    collect()
  
  # Select count and return
  return(cpv$count)
  
}