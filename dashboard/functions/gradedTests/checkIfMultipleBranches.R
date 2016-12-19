#' Check if "course_branch_id" column in 'course_item_types' table contains multiple branches
#' 
#' @param con Postgresql connection object returned by the \seealso{psql} function.
#' 
#' @author Jasper Ginn
#'
#' @return TRUE / FALSE value. Defaults to TRUE
#'
#' @importFrom dplyr %>%, tbl, summarize, collect, filter, n(), select, distinct
#' 

checkIfMultipleBranches <- function(con) {
  
  # Count the number of branches
  t <- tbl(con, "course_item_types") %>%
    # Join with course_items table
    inner_join(tbl(con, "course_items"), by="course_item_type_id") %>%
    # Select course_item_id
    select(., course_item_id, course_item_type_graded, course_item_type_desc, course_item_name) %>%
    # Filter where items are graded and filter where course_item_type_desc == exam
    filter(., course_item_type_graded,
           course_item_type_desc == "exam") %>%
    # Drop variables
    select(-course_item_type_graded, -course_item_type_desc) %>%
    # Merge with course branch item assessments to get assessment ids
    inner_join(tbl(con, "course_branch_item_assessments"), by ="course_item_id") %>%
    summarize(count = n_distinct(course_branch_id)) %>%
    collect()
  
  # Return
  return(ifelse(t$count > 1, TRUE, FALSE))
  
}
