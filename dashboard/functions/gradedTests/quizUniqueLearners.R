#' Query the number of unique learners who have taken a quiz
#' 
#' @param con Postgresql connection object returned by the \seealso{psql} function.
#' @param assessment_id_p Each quiz is assigned a unique id that is a combination of a base id and version. (from Coursera README)
#' @param course_item_id_p 5 character alphanumeric string identifying an individual item within a course. Items that have the same id that are in different branches of the same course are considered to be the same for the purposes of progress and grade computations. For example, if you complete item xxxxx in branch A, then you have completed it in branch B even if item xxxxx in branch B is very different from item xxxxx in branch A. (from Coursera README)
#' 
#' @author Jasper Ginn
#'
#' @return Numeric vector of length 1 containing the number of unique learners who have taken the quiz.
#'
#' @importFrom dplyr %>%, tbl, summarize, collect, filter, n(), select, distinct
#'

quizUniqueUsers <- function(con, assessment_id_p, course_item_id_p) {
  # Get unique learners for assignment
  aa <- tbl(con, "assessment_actions") %>%
    # Filter
    filter(assessment_id == assessment_id_p) %>%
    # Gemove guest_user_id
    select(-guest_user_id) %>%
    select(user_id = ends_with("user_id")) %>%
    # Summarize
    summarize(count = n_distinct(user_id)) %>%
    collect()
  
  # return
  return(aa$count)
}