# Helper functions for shiny app

library("dplyr") # Useful for data manipulation
library("lubridate") # Dates suck
library("reshape2") # Melting and casting, casting and melting.

# Function 1: Connect to postgresql -----

psql <- function(host, port, user, password, database) {
  # Load postgres library
  library("RPostgreSQL")
  # Connect
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, 
                   dbname=database,
                   port = port,
                   host = host,
                   user = user,
                   password = password)
  # Return
  return(con)
}

# Function 2: Get data about where people come from -----

mapData <- function(connection) {
  # Load countrycode library (used to convert between country codes and names/regions etc.)
  library("countrycode")
  # SQL statement
  STAT <- "SELECT country_cd FROM users"
  # Send query to postgres
  data <- dbSendQuery(connection, STAT)
  # Countries
  CO <- as.data.frame(fetch(data, n=-1), # Fetch data & wrap in data frame
                      stringsAsFactors=F) %>% # Chaining command (%>%) comes from dplyr package
    group_by(.,country_cd) %>% # Group by country
    tally(.) %>% # Counts per country
    mutate(., country_name = countrycode(country_cd, "iso2c", "country.name")) %>% # Create new variable. Convert ISO2 to country name
    arrange(., desc(n)) %>% 
    na.omit() %>% # Omit NA values
    as.data.frame(., stringsAsFactors = F)
  # Clear result
  dbClearResult(data)
  # Return data frame
  return(CO)
}

# UserJoin? ----

userJoinData <- function(connection) {
  # Get user joins
  STAT <- "SELECT user_join_ts FROM users;"
  # Fetch
  data <- dbSendQuery(connection, STAT)
  # Countries
  CO <- fetch(data, n=-1) %>% 
    mutate(., date = format(as.Date(user_join_ts),format="%Y-%m-%d")) %>%
    group_by(., date) %>%
    tally(.) %>%
    arrange(., date) %>%
    filter(., date > "2015-01-01") # Why are there dates in 2012/2013 etc.? Standard accounts?
  # Clear result
  dbClearResult(data)
  # Return data frame
  return(CO)
}

# Get video completion data ----- (UNFINISHED)

videoCompletion <- function(connection) {
  # Select items. Course_type_item_id == 1 -----> VIDEO
  course_items <- dbReadTable(connection, "course_items") %>%
    filter(., course_item_type_id == 1)
  # Get state types
  stat <- paste0("SELECT course_item_id, course_progress_state_type_id FROM course_progress WHERE course_item_id IN ",
                 "(",
                 paste0("'",unique(course_items$course_item_id),"'", collapse=","),
                 ")")
  # Create join table
  JT <- course_items[,c("course_item_id", "course_item_name")]
  # Query
  data <- dbSendQuery(connection, stat)
  # Fetch
  CO <- as.data.frame(fetch(data, n=-1), 
                      stringsAsFactors=F) %>%
    group_by(., course_item_id, course_progress_state_type_id) %>%
    summarise(., count = n())
  # Clear result
  dbClearResult(data)
  # Join & add completed
  CO2 <- merge(CO, JT, by = "course_item_id") %>%
    mutate(., completed = ifelse(course_progress_state_type_id == 2, "Completed", "Not Completed")) %>%
    as.data.frame(.) %>%
    select(., c(course_item_name, count, completed)) 
  # Reshape
  data.frame("lecture_name" = unique(CO2$course_item_name),
             "completed" = CO2[CO2$completed == "Completed",]$count,
             stringsAsFactors = F)
  
  chi <- dcast(CO2, course_item_name ~ completed, value.var = "course_item_name")
  # Return
  return(CO2)
}

# Passed, not passed and verified passed -----
# 1 == passed, 2 == verified passed, 0 == not passed.

passingGr <- function(connection) {
  # Get user joins
  STAT <- "SELECT course_grade_ts, course_passing_state_id, 
  course_grade_overall_passed_items, course_grade_overall FROM course_grades"
  # Fetch
  data <- dbSendQuery(connection, STAT)
  # Countries
  CO <- as.data.frame(fetch(data, n=-1), 
                      stringsAsFactors=F)
  # Clear results
  dbClearResult(data)
  # Send query to get total number of users
  query <- "SELECT COUNT(*) FROM users;"
  r <- dbSendQuery(connection, query)
  cnt <- fetch(r)
  # Clear result
  dbClearResult(r)
  # Process & return
  results <- list("total_users" = cnt,
                  "passing_data" = CO %>%
                    group_by(., course_passing_state_id) %>%
                    summarize(count = n()) %>%
                    mutate(., course_passing_state_id = ifelse(course_passing_state_id == 0, "Others",
                                                                ifelse(course_passing_state_id == 1, 
                                                                       "Passed Without Verification", 
                                                                       "Passed With Verification"))) %>%
                    as.data.frame(.),
                  "number_items_passed" = CO %>%
                    group_by(., course_grade_overall_passed_items) %>%
                    summarize(count = n()) %>%
                    as.data.frame(.),
                  "course_grade_overall" = CO %>%
                    select(., course_passing_state_id, course_grade_overall) %>%
                    as.data.frame(.),
                  "completed_time" = CO %>%
                    filter(., course_passing_state_id != 0) %>%
                    select(., course_passing_state_id,course_grade_ts) %>%
                    mutate(., ts_conv = parse_date_time(course_grade_ts,"%y-%m-%d %H:%M:%S")) %>%
                    mutate(., date = format(ts_conv,format="%Y-%m-%d")) %>%
                    as.data.frame(.))
  return(results)
}

# Forum texts -----

'
forTex <- function(connection)

# Testing


drv <- dbDriver("PostgreSQL")
connection <- dbConnect(drv, dbname="terrorism_ondemand",host = "127.0.0.1",user = "jasper",password = "root")

dbListTables(connection)

CG <- dbReadTable(connection, "discussion_questions")
CG2 <- dbReadTable(connection, "discussion_answers")
CGPG <- dbReadTable(connection, "course_item_passing_states")

dbDisconnect(con)


# Course_type_item_id == 1 -----> VIDEO
course_items <- course_items[course_items$course_item_type_id == 1, ]
# Course progress
course_progress <- dbReadTable(connection, "course_progress")
# State types
course_progress_state_types <- dbReadTable(connection, "course_progress_state_types")
'

