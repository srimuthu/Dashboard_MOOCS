#' Calculate p-value
#' 
#' Definition: p-value is the percentage of people who answered question correctly. This function should not be called directly.
#' 
#' @param data Dataset returned by sumStats (file queryTestStats.R line 86)
#' 
#' @author Jasper Ginn
#'
#' @return Numeric vector of length n containing pvalues for each question.
#'
#' @importFrom dplyr %>%, tbl, summarize, collect, filter, n(), select, distinct
#'

calculatePValue <- function(data) {
  # If wrong does not exist, return NULL
  if(!any("wrong" %in% data$is_correct)) return(NULL)
  # Spread good v bad answers
  data %>%
    select(is_correct, number_responses, assessment_question_id) %>%
    spread(., is_correct, number_responses) %>%
    # Calculate pval
    mutate(., pval = right / (right + wrong)) %>%
    select(-right, -wrong)
}
