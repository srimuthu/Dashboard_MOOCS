#' Query average grade for a quiz
#' 
#' @param con Postgresql connection object returned by the \seealso{psql} function.
#' @param assessment_id_p Each quiz is assigned a unique id that is a combination of a base id and version. (from Coursera README)
#' @param course_item_id_p 5 character alphanumeric string identifying an individual item within a course. Items that have the same id that are in different branches of the same course are considered to be the same for the purposes of progress and grade computations. For example, if you complete item xxxxx in branch A, then you have completed it in branch B even if item xxxxx in branch B is very different from item xxxxx in branch A. (from Coursera README)
#' @param attempt Numeric value indicating whether you want to query data for the first, second or third attempt that a user did the quiz.
#' 
#' @author Jasper Ginn
#'
#' @return Numeric vector of length 1 containing the average grade for a quiz. Returns NULL if no learners have done the quiz.
#'
#' @importFrom dplyr %>%, tbl, summarize, collect, filter, n(), select, distinct
#'

quizAvgGrade <- function(con, assessment_id_p, course_item_id_p, attempt = c(1,2,3)) {
  attempt <- match.arg(attempt)
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
                 # Select variables
                 select(user_id = ends_with("user_id"),
                        course_item_id,
                        course_item_grade_overall) %>%
                 # Filter for course item
                 filter(course_item_id == course_item_id_p) ,
               by = "user_id") %>%
    # Summarize
    summarize(avg_grade = mean(course_item_grade_overall)) %>%
    collect()
  
  # IF NA THEN RETURN NULL AND VALIDATE
  if(is.na(aa$avg_grade)) {
    return(NULL)
  } else {
    # Return
    return(round(aa$avg_grade, digits =2))
  }
}