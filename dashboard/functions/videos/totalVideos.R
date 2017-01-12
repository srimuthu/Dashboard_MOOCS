#' Query total videos
#' 
#' Definition: Total number of videos in the course
#'
#' @param con Postgresql connection object returned by the \seealso{psql} function.
#' 
#' @return Numeric vector of length 1 containing the number of videos
#' 
#' @author S.M.N. Balasubramanian
#'
#' @importFrom dplyr %>%, tbl, summarize, collect, filter, n()
#' 

totalVideos <- function(con) {
  
  # Query course_items table
  tv <- tbl(con, "course_items") %>%
    # Filter all items of type video (id =1)
    filter(course_item_type_id == 1) %>%
    summarize(n()) %>%
    collect()
  
  # Select count and return
  return(tv$count)
  
}