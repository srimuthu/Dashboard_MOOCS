#' Calculate RIT scores
#' 
#' Definition: The RIT score is a measure of correlation of an item vis-a-vis the test. If an item has a RIT score > 0, then students who scored high on the test as a whole tend to make that question well. If the RIT score < 0, then students who tend to score badly on the test as a whole tend to make that question well. If the RIT score == 0, then it has no impact.
#' 
#' @param data Dataset returned by sumStats (file queryTestStats.R line 86)
#' @param sd Standard deviation of final test scores
#' 
#' @author Jasper Ginn
#'
#' @return Numeric vector of length n containing pvalues for each question.
#'
#' @importFrom dplyr %>%, tbl, summarize, collect, filter, n(), select, distinct
#'

# Calculate Rit-score
ritScore <- function(data, sd) {
  # Return if uneven number of rows
  if((nrow(data) %% 2) != 0) return(NULL)
  # Pieces
  avg_score_right <- data %>% filter(is_correct == "right") %>% select(avg_score)
  avg_score_wrong <- data %>% filter(is_correct == "wrong") %>% select(avg_score)
  pValItem <- data %>% filter(is_correct == "right") %>% select(number_responses) / 
    (data %>% filter(is_correct == "right") %>% select(number_responses) + data %>% filter(is_correct == "wrong") %>% select(number_responses))
  q <- (1-pValItem)
  # RIT formula
  ritForm <- function(avg_score_right, avg_score_wrong, sd_test, pValItem, q) {
    ((avg_score_right - avg_score_wrong) / sd_test) * sqrt(pValItem * q)
  }
  ritscore <- ritForm(avg_score_right, avg_score_wrong, sd, pValItem, q)
  ritscore <- cbind(unique(data$assessment_question_id), ritscore)
  names(ritscore) <- c("assessment_question_id", "ritscore")
  return(ritscore)
}