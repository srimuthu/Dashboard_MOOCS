#' Number of payments
#' 
#' Definition: Number of users who paid for certificate  in at least 1 of the courses
#' 
#' @param con Postgresql connection object returned by the \seealso{psql} function.
#' @param from Date from which you want to query (inclusive). Must be passed in YYYY-MM-DD format. 
#' @param to Date to which you want to query (inclusive). Defaults to date of today. Must be passed in YYYY-MM-DD format. 
#' 
#' @return Numeric vector of length 1 containing the number of students who bought a certificate.
#' 
#' @author Jasper Ginn
#'
#' @importFrom dplyr %>%, tbl, summarize, collect, filter, n(), select, distinct
#' 

numberPayments <- function(con, from = "1970-01-01", to = as.character(Sys.Date())) {
  
  # From/to date --> check if right format ie. YYYY-MM-DD
  if(is.na(try(as.Date(from, format = "%Y-%m-%d")))) stop("Please enter a date in the following format: YYYY-MM-DD")
  if(is.na(try(as.Date(to, format = "%Y-%m-%d")))) stop("Please enter a date in the following format: YYYY-MM-DD")
  
  # Query number of payments in date range
  src_tbls(con)
  r <- tbl(con, "users") %>%
    # select column with user ids. Rename institution-specific user id to generic user id
    select(user_id = ends_with("user_id"), user_join_ts) %>%
    # Filter for dates
    filter(user_join_ts >= from,
           user_join_ts <= to) %>%
    # Join with certificate payments
    inner_join(tbl(con, "users_courses__certificate_payments") %>%
                 select(., user_id = ends_with("user_id"),
                        was_payment), 
               by = "user_id") %>%
    # Filter by TRUE
    filter(was_payment) %>%
    summarize(n()) %>%
    collect()
  
  # Return
  return(r$count)
  
}