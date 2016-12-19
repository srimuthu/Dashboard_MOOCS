#' Classify a combination of a p-value and a RIT score.
#' 
#' Classify a combination of a p-value and a RIT score in either one of three categories (OK, Review and Possible Review). 
#' 
#' @param pval Data returned by 'calculatePValue.R'
#' @param ritscores Data returned by 'ritScore.R'
#' @param attempt Whether the p-values and rit scores were based on a first, second or third quiz attempt. 
#' 
#' @author Jasper Ginn
#' 
#' @seealso Add documentation link
#'
#' @return Character vector of length n (== length of pval and ritscores) containing class labels.
#'
#' @importFrom dplyr %>%, tbl, summarize, collect, filter, n(), select, distinct
#'

classifyQuizItems <- function(pval, ritscores) {
  
  # Series of TRUE/FALSE statements to determine boundries
  # Pvalue: between 0.3 & 0.8
  # ritscore: above 0.3
  
  pvalcheck <- pval < 0.3 | pval > 0.8
  ritscorecheck <- ritscores < 0.3
  
  # Classify
  class <- c()
  for(n in 1:length(pvalcheck)) {
    pvalcheck1 <- pvalcheck[n]
    ritscorecheck1 <- ritscorecheck[n]
    if(pvalcheck1 & ritscorecheck1) {
      i <- "Review"
    } else if((pvalcheck1 & !ritscorecheck1) | (ritscorecheck1 & !pvalcheck1)) {
      i <- "Possible review"
    } else {
      i <- "OK"
    }
    class <- c(class, i)
  }
  
  # Return
  return(class)
}