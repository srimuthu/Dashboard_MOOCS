#' Query p-value & RIT-scores for a quiz
#' 
#' This function queries results on a quiz-question level (score per question). It then performs some calculations and calculates the p-value and RIT-score 
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

queryTestStats <- function(con, assessment_id_p, course_item_id_p, attempt = c(1,2,3)) {
  
  # Source function to strip HTML
  source("functions/forum/removeHTML.R")
  
  # Query unique assessment_action_id ordered by the timestamp
  # This means that we can differentiate between a first, second, third attempt and so on.
  orderedResponses <- tbl(con, "assessment_actions") %>% 
    # Filter for assessment id
    filter(assessment_id == assessment_id_p) %>%
    # Drop guest user id
    select(-guest_user_id) %>%
    # Select variables of interest
    select(user_id = ends_with("user_id"),
           assessment_action_id, 
           assessment_action_ts) %>%
    # Right join with assessment responses
    right_join(tbl(con, "assessment_responses") %>%
                 filter(assessment_id == assessment_id_p) %>%
                 select(assessment_action_id) %>%
                 distinct(),
               by = "assessment_action_id") %>%
    # Group by user
    group_by(user_id) %>%
    # Arrange attempts by timestamp
    arrange(assessment_action_ts) %>%
    # Get action number
    mutate(attempt_number = row_number()) %>%
    # Drop
    ungroup() %>%
    select(assessment_action_id,
           attempt_number) 
  
  # Get quiz responses for quiz, branch & version
  r3 <- tbl(con, "assessment_actions") %>%
    # Drop guest user id
    select(-guest_user_id) %>%
    # Select variables of interest
    select(assessment_id, 
           assessment_action_id, 
           assessment_action_start_ts, 
           user_id = ends_with("user_id")) %>%
    # Filter for assessment id
    filter(assessment_id == assessment_id_p) %>%
    # Join with assessment responses to get the response ids of interest
    inner_join(tbl(con, "assessment_responses") %>%
                 # Select variables of interest
                 select(assessment_action_id, assessment_question_id, 
                        assessment_response_score, assessment_response_weighted_score), 
               by = "assessment_action_id") %>%
    # Make sure that assessment_response_score is either 0 or 1
    mutate(assessment_response_score = ifelse(assessment_response_score < 1, 
                                              0, assessment_response_score)) %>%
    # Mutate wrong assessment_response_score so we can sum over them
    mutate(is_incorrect = ifelse(assessment_response_score == 0, 1, 0)) %>%
    # Join to get attempt numbers
    inner_join(orderedResponses, by="assessment_action_id") %>%
    # Filter for attempt number
    filter(attempt_number == attempt) %>%
    # Drop
    select(-attempt_number,
           -assessment_id,
           -assessment_action_start_ts,
           -assessment_response_weighted_score) %>%
    # Group group user
    group_by(user_id) %>%
    # Calculate score
    mutate(course_item_grade_overall = sum(assessment_response_score) / n()) %>%
    ungroup() %>%
    collect()
  
  # If no assessment_question_id return NULL. This line is not an elegant solution
  # but otherwise the user gets to see error messages between selecting questions.
  # This most likely happens because when a user switches the question, the question
  # version does not immediately change (we reset it when the question is changed by
  # the user, but this does not happen fast enough). Hence, the function tries to 
  # pull in data that does not exist.
  if(!"assessment_question_id" %in% names(r3)) return(NULL)
  # Create summary stats
  sumStats <- r3 %>%
    # Group by quiz item
    group_by(assessment_question_id,
             assessment_response_score) %>%
    # Summarize average score
    summarize(avg_score = mean(course_item_grade_overall),
              number_responses = n()) %>%
    # Arrange
    arrange(assessment_question_id,
            assessment_response_score) %>%
    # Mutate 1/0 to right/wrong
    mutate(is_correct = ifelse(assessment_response_score == 1, "right", "wrong")) %>%
    # Select
    select(assessment_question_id,
           is_correct,
           avg_score,
           number_responses) %>%
    ungroup() %>%
    # Get question prompt
    left_join(., tbl(con, "assessment_questions") %>%
                select(assessment_question_id, 
                       assessment_question_prompt) %>%
                collect(),
                by = "assessment_question_id") %>%
    collect() %>%
    # Check if question is in json
    mutate(is_json = startsWith(assessment_question_prompt, "{"))
  
  # Add labels (question prompts)
  sumStats$label <- unlist(map2(sumStats$assessment_question_prompt, 
                                sumStats$is_json, function(x,y) {
    # If is_json == TRUE, then get the label value, else just return the original value
    if(y) {
      return(strtrim(removeHTML(jsonlite::fromJSON(x)$definition$value), 40))
    } else {
      return(strtrim(x, 40))
    }
  }))
  
  # Calculate standard deviation
  sd <- sd(r3$course_item_grade_overall)
  
  # Return list with values
  return(
    list(
      "sd" = sd,
      "sumStats" = sumStats %>% select(-assessment_question_prompt, -is_json)
    )
  )
  
}
