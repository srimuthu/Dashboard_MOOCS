#' Query country of origin for each user
#' 
#' This function takes the two-letter country-code for each student and converts it to country name. Then plot on a world map using leaflet.
#'
#' @param con Postgresql connection object returned by the \seealso{psql} function.
#' 
#' @author Jasper Ginn
#'
#' @importFrom dplyr %>%, select, tbl, group_by, summarize, arrange, collect, mutate, filter
#' @importFrom countrycode countrycode

countryOfOrigin <- function(con, from = "1970-01-01", to = as.character(Sys.Date())) {
  
  # From/to date --> check if right format ie. YYYY-MM-DD
  if(is.na(try(as.Date(from, format = "%Y-%m-%d")))) stop("Please enter a date in the following format: YYYY-MM-DD")
  if(is.na(try(as.Date(to, format = "%Y-%m-%d")))) stop("Please enter a date in the following format: YYYY-MM-DD")
  
  # Get users table
  i <- tbl(con, "users") %>%
    # Select for country code and rename user id
    select(user_id = ends_with("user_id"), country_cd) %>%
    # Join with table containing info when students joined
    inner_join(
      tbl(con, "course_memberships") %>%
        # Filter from / to date
        filter(course_membership_ts >= from,
               course_membership_ts <= to,
               course_membership_role == "LEARNER") %>%
        # Rename user_id
        select(user_id = ends_with("user_id"))
    , by = "user_id") 
    
  # Check if number of rows == 0. If yes, return empty tibble
  check <- i %>% summarize(count = n()) %>% collect()
  if(check$count == 0) return(tbl_df(data.frame("country_cd" = character(), 
                                     "n" = numeric(),
                                     "iso3c" = character(),
                                     stringsAsFactors = FALSE)))
  
  # Else, return countries
  i %>%
    # Group by country_cd
    group_by(country_cd) %>%
    # Sum by countrycode
    summarize(n = n()) %>%
    # Arrange in descending order
    arrange(desc(n)) %>%
    # Collect from database
    collect() %>%
    # Mutate such that we get full country names
    mutate("iso3c" = countrycode::countrycode(country_cd, "iso2c", "iso3c")) %>%
    # Remove any NA values
    filter(!is.na(iso3c))
  
}
