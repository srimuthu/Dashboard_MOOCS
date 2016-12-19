#' Get a dataframe with all graded quizzes for a course.
#' 
#' Graded quizzes are further split in branches (when you have different versions of the same course) and versions.
#' 
#' @param con Postgresql connection object returned by the \seealso{psql} function.
#' @param from Date from which you want to query (inclusive). Must be passed in YYYY-MM-DD format. 
#' @param to Date to which you want to query (inclusive). Defaults to date of today. Must be passed in YYYY-MM-DD format. 
#' 
#' @author Jasper Ginn
#'
#' @return tibble with 4 columns and n rows for the number of assignments present. Column 1 is the item_id of the quiz, column 2 is the course_item_name, column 3 is the course_branch_id, and column 4 is the assessment_id. Note that an assignment may appear multiple times IF the course is branched (i.e. is running multiple versions of the same course for the purposes of e.g. an A/B test or to split groups).
#'
#' @importFrom dplyr %>%, tbl, summarize, collect, filter, n(), select, distinct
#' 

retrieveGradedTests <- function(con, from = as.character(Sys.Date() - 30), 
                                to = as.character(Sys.Date())) {
  
  # From/to date --> check if right format ie. YYYY-MM-DD
  if(is.na(try(as.Date(from, format = "%Y-%m-%d")))) stop("Please enter a date in the following format: YYYY-MM-DD")
  if(is.na(try(as.Date(to, format = "%Y-%m-%d")))) stop("Please enter a date in the following format: YYYY-MM-DD")
  
  # Retrieve data frame of graded tests
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
    # Arrange by course item name
    arrange(desc(course_item_name)) %>%
    collect()
  
}

