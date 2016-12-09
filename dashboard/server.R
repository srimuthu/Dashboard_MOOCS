
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(ggplot2)
library(plotly)

# Shiny server
function(input, output, session) {
  
  # ---------------------------------------
  # USER SETTINGS
  # ---------------------------------------
  
  #postgresql setup
  psql_host <- reactive({
    input$psqlhostname
  })
  psql_user <- reactive({
    input$psqlusername
  })
  psql_pwd <- reactive({
    input$psqlpassword
  })
  psql_db <- reactive({
    input$selectCourse
  })
  psql_port <- reactive({
    input$psqlport
  })
  
  # Listen for click to save settings as default
  observeEvent(input$saveSettingsButton, {
    postgres_defaults <- list()
    # Set defaults
    postgres_defaults$hostname <- psql_host()
    postgres_defaults$port <- as.numeric(psql_port())
    postgres_defaults$user <- psql_user()
    postgres_defaults$password <- psql_pwd()
    postgres_defaults$database <- psql_db()
    # Save
    setts <- jsonlite::toJSON(postgres_defaults, pretty = TRUE)
    write(setts, paste0(getwd(), "/postgres_defaults.json"))
  })
  
  # Listen for click to save courses settings as default
  observeEvent(input$saveCoursesButton, {
    courses_defaults <- list()
    # Set defaults
    v <- input$courses
    # Split at ", \n"
    vsplit1 <- strsplit(v, ",\n")[[1]]
    # Split at '='
    vsplit2 <- strsplit(vsplit1, " = ")
    # To list & JSON
    names.courses <- c()
    for(co in 1:length(vsplit2)) {
      names.courses <- c(names.courses, vsplit2[[co]][1])
      # Remove from vsplit2
      vsplit2[[co]] <- vsplit2[[co]][-1]
    }
    # Add names
    names(vsplit2) <- names.courses
    # To json
    courses <- jsonlite::toJSON(vsplit2, pretty = TRUE)
    write(courses, "settings/course_list.json")
  })
  
  # Get date range
  dates <- reactive({
    input$daterange
  })

  # ---------------------------------------
  # TAB DASHBOARD OUTPUT
  # ---------------------------------------  
  
  # New enrollers
  output$valueBoxTotalStudents <- renderValueBox({
    d<-dates()
    # Connect to postgres
    con <- psql(psql_host(), psql_port(), psql_user(), psql_pwd(), psql_db())
    # calculate total students
    TS <- newEnrollers(con, from = d[1], to = d[2])
    # Valuebox
    valueBox(
      TS, "New enrollers", icon = icon("users"), color = "purple"
    )
  })
  
  # Active
  output$valueBoxActiveStudents <- renderValueBox({
    d<-dates()
    # Connect to postgres
    con <- psql(psql_host(), psql_port(), psql_user(), psql_pwd(), psql_db())
    # calculate total students
    TS <- activeStudents(con, from = d[1], to = d[2])
    # Valuebox
    valueBox(
      TS, "Active students", icon = icon("bolt"), color = "purple"
    )
  })
  
  # Browsers
  output$valueBoxBrowsingStudents <- renderValueBox({
    d<-dates()
    # Connect to postgres
    con <- psql(psql_host(), psql_port(), psql_user(), psql_pwd(), psql_db())
    # calculate total students
    TS <- viewingStudents(con, from = d[1], to = d[2])
    # Valuebox
    valueBox(
      TS, "Browsing students", icon = icon("mobile"), color = "purple"
    )
  })
  
  # Course completers
  output$valueBoxCourseCompleters <- renderValueBox({
    d<-dates()
    # Connect to postgres
    con <- psql(psql_host(), psql_port(), psql_user(), psql_pwd(), psql_db())
    # calculate total students
    TS <- courseCompleters(con, from = d[1], to = d[2])
    # Valuebox
    valueBox(
      TS, "Course completers", icon = icon("graduation-cap"), color = "blue"
    )
  })
  
  # Number payments
  output$valueBoxPayments <- renderValueBox({
    d<-dates()
    # Connect to postgres
    con <- psql(psql_host(), psql_port(), psql_user(), psql_pwd(), psql_db())
    # calculate total students
    TS <- numberPayments(con, from = d[1], to = d[2])
    # Valuebox
    valueBox(
      TS, "Payments", icon = icon("credit-card"), color = "blue"
    )
  })
  
  # Number financial aid
  output$valueBoxFinancialAid <- renderValueBox({
    d<-dates()
    # Connect to postgres
    con <- psql(psql_host(), psql_port(), psql_user(), psql_pwd(), psql_db())
    # calculate total students
    TS <- numberFinancialAid(con, from = d[1], to = d[2])
    # Valuebox
    valueBox(
      TS, "Students receiving financial aid", icon = icon("medkit"), color = "blue"
    )
  })
  
  # Users over time
  output$chartUsersOverTime <- renderPlotly({
    # Fetch data for last 90 days
    io <- usersOverTime(con)
    # Create an area chart
    a <- ggplot(io, aes(x=date, y=Enrollers)) +
      geom_area(fill = "red", alpha = 0.6) +
      geom_line(aes(x=date, y=SMA), color = "#808080", size = 1) +
      theme_cfi_scientific() +
      scale_x_date(name = "") +
      scale_y_continuous(name = "Number of enrollers")
    # Push through plotly to make interactive
    ggplotly(a)
  })
  
  # ---------------------------------------
  # TAB GEOGRAPHY OUTPUT
  # ---------------------------------------  
  
  # Map with number of users
  output$leafletMapCountryOfOrigin <- renderLeaflet({
    d<-dates()
    # Connect to postgres
    con <- psql(psql_host(), psql_port(), psql_user(), psql_pwd(), psql_db())
    # Dataframe with countries of origin
    io <- countryOfOrigin(con, from = d[1], to = d[2])
    # Validate that length of io > 0
    validate(
      need(nrow(io) != 0, "There are no learners who joined in this period.")
    )
    # Join to map data
    countries@data <- countries@data %>%
      # Join with coursera data
      left_join(io, by="iso3c") %>%
      # Replace NA values
      mutate(n=ifelse(is.na(n), 0, n))
    # Create color palette
    pal <- colorQuantile(RColorBrewer::brewer.pal(5, "Blues"), countries@data$n)
    # Create popup based on values
    popup <- paste0(
      "<strong>", countries@data$country, ": </strong>", countries@data$n, " learners (", 
      round((countries@data$n / sum(countries@data$n)) * 100,digits=2), "%)"
    )
    # Create leaflet map
    leaflet(data = countries) %>%
      # Add polygons
      addPolygons(fillColor = ~pal(n), 
                  fillOpacity = 1, 
                  color = "#505051",
                  popup = popup,
                  weight = 2.5) %>%
      # Set view on europe
      setView(lng = -27.5097656, lat = 29.0801758, zoom = 1)
    
  })
  
}
