#' Convenience function to quickly connect to postgresql
#'
#' @param host IP address or localhost. Depends on where postgresql database is running. Default values should be '127.0.0.1' or 'localhost'
#' @param port Port the postgresql database is listening on
#' @param user User connecting to database
#' @param password Password belonging to user who is connecting
#' @param database Name of the database you want to connect to
#' 
#' @author Jasper Ginn
#'
#' @importFrom dplyr src_postgres

psql <- function(host, port, user, password, database) {
  
  # Connect
  con <- dplyr::src_postgres(dbname = database,
                             host = host,
                             port = port,
                             user = user,
                             password = password)
  
  # Return
  return(con)
  
}
