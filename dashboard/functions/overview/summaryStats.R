#' Calculate summary statistics over all courses for main panel
#' 
#' This function calculates several summary statistics to compare them across courses. This function is not called in the shiny app. Instead, it stores the results in a local sqlite database and can be called e.g. once a week to update the data.
#'
#' @param con Postgresql connection object returned by the \seealso{psql} function.
#' 
#' @return Returns TRUE if operation is successful
#' 
#' @author Jasper Ginn
#'
#' @importFrom dplyr %>%, select, tbl, group_by, summarize, arrange, collect, mutate, filter
#' @importFrom countrycode countrycode
#' 

# Libraries
library(dplyr)
library(tidyr)
library(RPostgreSQL)

# Functions
source("functions/dashboard/totalUsers.R")
source("functions/dashboard/activeStudents.R")
source("functions/dashboard/viewingStudents.R")
source("functions/dashboard/courseCompleters.R")
source("functions/dashboard/numberPayments.R")
source("functions/dashboard/numberFinancialAid.R")
source("functions/dashboard/usersOverTime.R")
source("functions/psql.R")

summaryStats <- function(from = "1970-01-01", to = as.character(Sys.Date())) {
  
  # -------
  # CHECKS
  # -------
  
  sqlite_exists <- "summary_stats.sqlite" %in% list.files("data/")
  
  # ----------
  # LOAD DATA
  # ----------
  
  # Get postgres defaults
  pgd <- jsonlite::fromJSON("settings/postgres_defaults.json")
  # Get courses
  courses <- jsonlite::fromJSON("settings/course_list.json")

  # ----------------
  # CALCULATE STATS
  # ----------------
  
  # Active students
  AS <- lapply(courses, function(x) {
    # Connect to db
    con <- psql(host = pgd$hostname, port = pgd$port, user = pgd$user, password = pgd$password, database = x)
    # Get data
    d <- activeStudents(con, from, to)
    # Disconnect
    dbDisconnect(con$con)
    # Return data
    d
  }) %>%
    tbl_df() %>%
    gather(., course, active_students)
  
  # New enrolments
  NE <- lapply(courses, function(x) {
    # Connect to db
    con <- psql(host = pgd$hostname, port = pgd$port, user = pgd$user, password = pgd$password, database = x)
    # Get data
    d <- newEnrollers(con, from, to)
    # Disconnect
    dbDisconnect(con$con)
    # Return data
    d
  }) %>%
    tbl_df() %>%
    gather(., course, new_enrolments)
  
  # viewing students
  VS <- lapply(courses, function(x) {
    # Connect to db
    con <- psql(host = pgd$hostname, port = pgd$port, user = pgd$user, password = pgd$password, database = x)
    # Get data
    d <- viewingStudents(con, from, to)
    # Disconnect
    dbDisconnect(con$con)
    # Return data
    d
  }) %>%
    tbl_df() %>%
    gather(., course, viewing_students)
  
  # Course completers
  CC <- lapply(courses, function(x) {
    # Connect to db
    con <- psql(host = pgd$hostname, port = pgd$port, user = pgd$user, password = pgd$password, database = x)
    # Get data
    d <- courseCompleters(con, from, to)
    # Disconnect
    dbDisconnect(con$con)
    # Return data
    d
  }) %>%
    tbl_df() %>%
    gather(., course, course_completers)
  
  # Payments
  PAY <- lapply(courses, function(x) {
    # Connect to db
    con <- psql(host = pgd$hostname, port = pgd$port, user = pgd$user, password = pgd$password, database = x)
    # Get data
    d <- numberPayments(con, from, to)
    # Disconnect
    dbDisconnect(con$con)
    # Return data
    d
  }) %>%
    tbl_df() %>%
    gather(., course, payments)
  
  # Financial aid
  FINAID <- lapply(courses, function(x) {
    # Connect to db
    con <- psql(host = pgd$hostname, port = pgd$port, user = pgd$user, password = pgd$password, database = x)
    # Get data
    d <- numberFinancialAid(con, from, to)
    # Disconnect
    dbDisconnect(con$con)
    # Return data
    d
  }) %>%
    tbl_df() %>%
    gather(., course, financial_aid)
  
  # Bind together
  final_ds <- NE %>%
    inner_join(AS, by = "course") %>%
    inner_join(VS, by = "course") %>%
    inner_join(CC, by = "course") %>%
    inner_join(PAY, by = "course") %>%
    inner_join(FINAID, by = "course")
  
  # --------------
  # STORE RESULTS
  # --------------
  
  # Open connection to sqlite
  sql_con <- src_sqlite(path = "data/summary_stats.sqlite", create = TRUE)
  
  # If db exists, check if table exists. If so, drop
  if(sqlite_exists) {
    tabs <- dbListTables(sql_con$con)
    if("summary_stats" %in% tabs) {
      q <- dbSendQuery(sql_con$con, "DROP TABLE IF EXISTS summary_stats;")
      cr <- dbClearResult(q)
    }
  }
  
  # Store
  ct <- copy_to(sql_con, final_ds, "summary_stats", temporary = FALSE)
  
  # Disconnect
  dbDisconnect(sql_con$con)
  
}

summaryStats()
