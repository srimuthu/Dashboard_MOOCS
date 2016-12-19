#' Query when users joined over time
#' 
#' This function takes the two-letter country-code for each student and converts it to country name. Then plot on a world map using leaflet.
#'
#' @param con Postgresql connection object returned by the \seealso{psql} function.
#' 
#' @return A tibble with at most 120 rows and 3 columns (column names: date, Enrollers, and SMA) --> Simple moving average 
#' 
#' @author Jasper Ginn
#'
#' @importFrom dplyr %>%, select, tbl, group_by, summarize, arrange, collect, mutate, filter
#' @importFrom countrycode countrycode
#' 

usersOverTime <- function(con, from = as.character(Sys.Date() - 120), to = Sys.Date()) {
  
  source("functions/dashboard/movingAverage.R")
  
  # From/to date --> check if right format ie. YYYY-MM-DD
  if(is.na(try(as.Date(from, format = "%Y-%m-%d")))) stop("Please enter a date in the following format: YYYY-MM-DD")
  if(is.na(try(as.Date(to, format = "%Y-%m-%d")))) stop("Please enter a date in the following format: YYYY-MM-DD")
  
  # Get data for last 90 days
  tbl(con, "course_memberships") %>%
    # Filter from / to date
    filter(course_membership_ts >= from,
           course_membership_ts <= to,
           course_membership_role == "LEARNER") %>%
    # Mutate timestamp to date
    mutate(date = date(course_membership_ts)) %>%
    # Count by date
    group_by(date) %>%
    summarize(Enrollers = n()) %>%
    # Arrange by date order
    arrange(date) %>%
    collect() %>%
    # Calculate moving average
    mutate(SMA = movingAverage(Enrollers, n=5))
  
}
