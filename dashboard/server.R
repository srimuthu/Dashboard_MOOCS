
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
library(scales)

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
  # TAB OVERVIEW OUTPUT
  # ---------------------------------------
  
  # Value box for total number of students
  output$valueBoxSummaryTotalStudents <- renderValueBox({
    # Connect to sqlite with summary stats
    con <- src_sqlite("data/summary_stats.sqlite")
    # Fetch summary stats
    SS <- tbl(con, "summary_stats") %>% 
      summarize(count = sum(new_enrolments)) %>%
      collect()
    # Disconnect
    dbDisconnect(con$con)
    # Output value box
    valueBox(
      SS$count, "Total number of enrollers", icon = icon("users"), color = "purple"
    )
  })
  
  # Value box for total number of graduated students
  output$valueBoxSummaryCompleted <- renderValueBox({
    # Connect to sqlite with summary stats
    con <- src_sqlite("data/summary_stats.sqlite")
    # Fetch summary stats
    SS <- tbl(con, "summary_stats") %>%
      summarize(count = sum(course_completers)) %>%
      collect()
    # Disconnect
    dbDisconnect(con$con)
    # Output value box
    valueBox(
      SS$count, "Students who have completed a course", icon = icon("graduation-cap"), color = "purple"
    )
  })
  
  # Value box for total number of payments (placeholder)
  output$valueBoxSummaryPayments <- renderValueBox({
    # Connect to sqlite with summary stats
    con <- src_sqlite("data/summary_stats.sqlite")
    # Fetch summary stats
    SS <- tbl(con, "summary_stats") %>%
    summarize(count = sum(payments)) %>%
      collect()
    # Disconnect
    dbDisconnect(con$con)
    # Output value box
    valueBox(
      SS$count, "Total number of payments", icon = icon("credit-card"), color = "purple"
    )
  })
  
  # Conversion rates
  output$chartSummaryConversion <- renderPlotly({
    # Connect to sqlite with summary stats
    con <- src_sqlite("data/summary_stats.sqlite")
    # Fetch summary stats
    SS <- tbl(con, "summary_stats") %>%
      select(course, new_enrolments, payments) %>%
      mutate(conversion = payments / new_enrolments) %>%
      arrange(desc(payments)) %>%
      # Add label for plot
      mutate(label = "Conversion") %>%
      collect()
    # Disconnect
    dbDisconnect(con$con)
    # Plot
    p <- ggplot(SS, aes(x=reorder(course, -conversion), 
                        y=conversion, label = label, fill = course)) +
      geom_bar(stat= "identity") +
      theme_cfi_scientific() +
      scale_fill_manual(values = CFI_palette(),
                        guide = guide_legend(reverse = TRUE)) +
      scale_x_discrete(name = "",
                       expand = c(0.01,0))  +
      scale_y_continuous(labels=percent,
                         expand = c(0.01,0))
    # Plotly
    ggplotly(p)
  })
  
  # Active learners
  output$chartSummaryGraduation <- renderPlotly({
    # Connect to sqlite with summary stats
    con <- src_sqlite("data/summary_stats.sqlite")
    # Fetch summary stats
    SS <- tbl(con, "summary_stats") %>%
      select(course, active_students, course_completers) %>%
      mutate(completion = course_completers / active_students) %>%
      arrange(desc(completion)) %>%
      # Add label for plot
      mutate(label = "Course completers") %>%
      collect()
    # Disconnect
    dbDisconnect(con$con)
    # Plot
    p <- ggplot(SS, aes(x=reorder(course, -completion),
                        y=completion, label = label, fill = course)) +
      geom_bar(stat= "identity") +
      theme_cfi_scientific() +
      scale_fill_manual(values = CFI_palette(),
                        guide = guide_legend(reverse = TRUE)) +
      scale_x_discrete(name = "",
                       expand = c(0.01,0))  +
      scale_y_continuous(labels=percent,
                         expand = c(0.01,0))
    # Plotly
    ggplotly(p)
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
    # Close connection
    dbDisconnect(con$con)
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
    # Close connection
    dbDisconnect(con$con)
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
    # Close connection
    dbDisconnect(con$con)
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
    # Close connection
    dbDisconnect(con$con)
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
    # Close connection
    dbDisconnect(con$con)
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
    # Close connection
    dbDisconnect(con$con)
    # Valuebox
    valueBox(
      TS, "Students receiving financial aid", icon = icon("medkit"), color = "blue"
    )
  })
  
  # Users over time
  output$chartUsersOverTime <- renderPlotly({
    # Connect to postgres
    con <- psql(psql_host(), psql_port(), psql_user(), psql_pwd(), psql_db())    
    # Fetch data for last 90 days
    io <- usersOverTime(con)
    # Create an area chart
    a <- ggplot(io, aes(x=date, y=Enrollers)) +
      geom_area(fill = "red", alpha = 0.6) +
      geom_line(aes(x=date, y=SMA), color = "#808080", size = 1) +
      theme_cfi_scientific() +
      scale_x_date(name = "",
                   expand = c(0.01,0)) +
      scale_y_continuous(name = "Number of enrollers",
                         expand = c(0.03,0)) +
      theme(
        axis.ticks.x = element_line(),
        axis.text.x = element_text()
      )
    # Close connection
    dbDisconnect(con$con)
    # Push through plotly to make interactive
    ggplotly(a)
  })
  
  # ---------------------------------------
  # TAB GEOGRAPHY OUTPUT
  # ---------------------------------------  
  
  # Map with number of users
  output$leafletMapCountryOfOrigin <- renderLeaflet({
    d<-dates()
    # Get map settings input
    msi <- input$mapSettingsInput
    # Connect to postgres
    con <- psql(psql_host(), psql_port(), psql_user(), psql_pwd(), psql_db())
    # Dataframe with countries of origin
    io <- countryOfOrigin(con, from = d[1], to = d[2])
    # Close connection
    dbDisconnect(con$con)
    # Validate that length of io > 0
    validate(
      need(nrow(io) != 0, "There are no learners who joined in this period.")
    )
    # Join to map data
    countries@data <- countries@data %>%
      # Join with coursera data
      left_join(io, by="iso3c") %>%
      mutate(
        Population = as.numeric(as.character(Population)),
        Internet_users = as.numeric(as.character(Internet_users)),
        n = ifelse(is.na(n), 0, n),
        Population = ifelse(is.na(Population), 0, Population),
        Internet_users = ifelse(is.na(Internet_users), 0, Internet_users)
      )
    # If normalize
    if(msi == "Population") {
      countries@data <- countries@data %>%
        mutate(n = round((n / Population) * 1000000, digits=0))
      # Create popup
      popup <- paste0(
        "<strong>", countries@data$country, ": </strong><br>", 
        countries@data$n, " learners per 1.000.000 inhabitants"
      )
    } else if(msi == "Internet users") {
      countries@data <- countries@data %>%
        mutate(n = round((n / Internet_users) * 1000000, digits=0))
      # Create popup
      popup <- paste0(
        "<strong>", countries@data$country, ": </strong><br>", 
        countries@data$n, " learners per 1.000.000 internet users"
      )
    } else {
      # Create popup
      popup <- paste0(
        "<strong>", countries@data$country, ": </strong>", countries@data$n, " learners (", 
        round((countries@data$n / sum(countries@data$n)) * 100,digits=2), "%)"
      )
    }
    # Get tmp values
    tmpvalues <- countries@data %>%
      select(n) %>%
      filter(!is.na(n),
             !is.infinite(n))
    # Create color palette
    pal <- colorQuantile(RColorBrewer::brewer.pal(5, "Blues"), 
                         domain = tmpvalues$n, n=5)
    # Validate that quantiles are unique
    validate(
      need(
        length(unique(quantile(tmpvalues$n))) == 5, "Not enough data to produce unique breaks for quantile values. Please extend the data range."
      )
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
