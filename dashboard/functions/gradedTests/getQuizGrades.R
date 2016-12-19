#' Query grades for a quiz
#' 
#' @param con Postgresql connection object returned by the \seealso{psql} function.
#' @param assessment_id_p Each quiz is assigned a unique id that is a combination of a base id and version. (from Coursera README)
#' @param course_item_id_p 5 character alphanumeric string identifying an individual item within a course. Items that have the same id that are in different branches of the same course are considered to be the same for the purposes of progress and grade computations. For example, if you complete item xxxxx in branch A, then you have completed it in branch B even if item xxxxx in branch B is very different from item xxxxx in branch A. (from Coursera README)
#' 
#' @author Jasper Ginn
#'
#' @return Tibble with 1 column containing the results of the 'course_item_grade_overall' column
#'
#' @importFrom dplyr %>%, tbl, summarize, collect, filter, n(), select, distinct
#' 

quizGetGrades <- function(con, assessment_id_p, course_item_id_p) {
  # Get users who filled out particular assessment
  aa <- tbl(con, "assessment_actions") %>% 
    # Gemove guest_user_id
    select(-guest_user_id) %>%
    select(user_id = ends_with("user_id"),
           assessment_id) %>%
    filter(assessment_id == assessment_id_p) %>%
    # Drop assessment_id
    select(-assessment_id) %>%
    # Take distinct users
    distinct() %>%
    # Join with item grades
    inner_join(tbl(con, "course_item_grades") %>%
                 select(user_id = ends_with("user_id"),
                        course_item_id,
                        course_item_grade_overall) %>%
                 filter(course_item_id == course_item_id_p),
               by = "user_id") %>%
    # Summarize
    select(course_item_grade_overall) %>%
    collect()
  
  # Return
  return(aa)
}
