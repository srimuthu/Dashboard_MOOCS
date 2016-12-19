#' Function to split assessment version from assessment_id
#' 
#' @param split_me String with assessment_id to split
#' 
#' @author Jasper Ginn
#'
#' @return dplyr postgres lazy evaluation for the query. Can be passed to other functions to reduce computation.
#'
#' @importFrom dplyr %>%, tbl, summarize, collect, filter, n(), select, distinct
#' 

getIds <- function(split_me) {
  sm <- sapply(split_me, function(x) strsplit(x, "@")[[1]][2])
  as.numeric(unname(sm))
}