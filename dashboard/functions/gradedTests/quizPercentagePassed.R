#' Calculate the percentage of learners who pass a quiz
#' 
#' @param con Postgresql connection object returned by the \seealso{psql} function.
#' @param assessment_id_p Each quiz is assigned a unique id that is a combination of a base id and version. (from Coursera README)
#' @param course_item_id_p 5 character alphanumeric string identifying an individual item within a course. Items that have the same id that are in different branches of the same course are considered to be the same for the purposes of progress and grade computations. For example, if you complete item xxxxx in branch A, then you have completed it in branch B even if item xxxxx in branch B is very different from item xxxxx in branch A. (from Coursera README)
#' 
#' @author Jasper Ginn
#' 
#' @seealso Add documentation link
#'
#' @return percentage of learners who passed the test
#'
#' @importFrom dplyr %>%, tbl, summarize, collect, filter, n(), select, distinct
#' 

quizPercentagePassed <- function(con, assessment_id_p, course_item_id_p) {
  
  i2 <- tbl(con, "course_item_grades") %>%
    collect()
  # Get
  i <- tbl(con, "assessment_actions") %>% head() %>% collect()
    # Filter 
    filter(assessment_id == assessment_id_p) %>%
    # Drop guest user id
    select(-guest_user_id) %>%
    # Select variables of interest
    select(assessment_id, 
           user_id = ends_with("user_id")) %>%
    collect()
  
  i <- tbl(con, "course_item_grades") %>%
    collect()

}