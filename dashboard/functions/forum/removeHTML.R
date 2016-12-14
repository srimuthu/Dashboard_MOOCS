#' Remove HTML tags from a string.
#' 
#' @param con Postgresql connection object returned by the \seealso{psql} function.
#' @param from Date from which you want to query (inclusive). Must be passed in YYYY-MM-DD format. 
#' @param to Date to which you want to query (inclusive). Defaults to date of today. Must be passed in YYYY-MM-DD format. 
#' 
#' @author Jasper Ginn
#'
#' @return vector of one or more cleaned strings.
#'
#' @importFrom dplyr %>%, tbl, summarize, collect, filter, n(), select, distinct
#' 