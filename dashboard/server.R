
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(googleVis)

function(input, output) {
  #Import helper script
  source(paste0(getwd(),"/functions/helpers.R"))
  
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
    # Split at ', \n'
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
  
  #-------------------------------------TEST------------------------------------------
  # GET COMPLETION DATA OVERVIEW-----
  
  compDataOverview <- reactive({
    
    for (course in course_list){
      con <- psql(psql_host(), psql_port(), psql_user(), psql_pwd(), course)
      # Get data
      compDataOverview <- passingGr(con)
      # Disconnect
      t <- dbDisconnect(con)  
    }
    
    # Return
    return(compDataOverview)
    
  })  
  # Create value box (average grade)
  output$completersOverview <- renderValueBox({
    # Data
    t <- compDataOverview()$course_grade_overall %>%
      filter(., course_passing_state_id != 0)
    # Value box
    valueBox(
      format(round(mean(t$course_grade_overall), digits = 2),format="d",big.mark=","),
      "Average Grade (of completers)", icon = icon("area-chart"), color = "yellow")
  })
  #-------------------------------------END TEST------------------------------------------
  
  
  # Dates on which users join ----

  usrJoin <- reactive({
    con <- psql(psql_host(), psql_port(), psql_user(), psql_pwd(), psql_db())
    # Get user join
    uJD <- userJoinData(con) # This function can be found in 'helpers.R'
    # Disconnect
    t <- dbDisconnect(con)
    return(uJD)
  })

  # Plot
  output$joinLine <- renderGvis({
    # data
    t <- usrJoin()
    names(t) <- c("date", "count")
    # Return chart
    gvisLineChart(t#,
                  #options=list(legend = "none",
                  #            series="[{targetAxisIndex: 0},
                  #           {targetAxisIndex:1}]",
                  #          vAxes="[{title:'Number of participants'}, {title:'Date'}]"
                  #)
    )
  })

  # GET COMPLETION DATA -----

  compData <- reactive({
    con <- psql(psql_host(), psql_port(), psql_user(), psql_pwd(), psql_db())
    # Get data
    compData <- passingGr(con)
    # Disconnect
    t <- dbDisconnect(con)
    # Return
    return(compData)
  })

  # Visuals completion data

  # Create value box (completion rate)
  output$compRate <- renderValueBox({
    # Data
    t <- compData()$passing_data
    # Numb
    y <- t[t$course_passing_state_id != "Others",]
    # Num
    tnum <- round((sum(y$count) / compData()$total_users$count) * 100, digits=2)
    # Value box
    valueBox(
      format(paste0(tnum,"%"),format="d",big.mark=","),
      "Overall Completion Rate", icon = icon("area-chart"), color = "green")
  })
  # Create value box (average grade)
  output$avgGr <- renderValueBox({
    # Data
    t <- compData()$course_grade_overall %>%
      filter(., course_passing_state_id != 0)
    # Value box
    valueBox(
      format(round(mean(t$course_grade_overall), digits = 2),format="d",big.mark=","),
      "Average Grade (of completers)", icon = icon("area-chart"), color = "yellow")
  })
  # Create value box (completed this month)
  output$compTM <- renderValueBox({
    thisMonth <- format(Sys.Date(), "%B")
    thisYear <- format(Sys.Date(), "%Y")
    # Data
    t <- compData()$completed_time %>%
      mutate(., month = format(ts_conv, "%B")) %>%
      mutate(., year = format(ts_conv, "%Y")) %>%
      filter(., month == thisMonth & year == thisYear)
    # Value box
    valueBox(
      format(nrow(t),format="d",big.mark=","),
      paste0("# Completers in ",thisMonth, " ", thisYear),
      icon = icon("area-chart"), color = "orange")
  })
  # Pie chart (completers)
  output$barChartComp <- renderGvis({
    t <- compData()$passing_data
    # Plot
    gvisPieChart(t,
                 options = list(title="Overview of Completion Rates")
    )
  })
  # Histogram of Grade Distribution
  output$histGrades <- renderGvis({
    t <- compData()$course_grade_overall %>%
      filter(., course_passing_state_id != 0) %>%
      select(., course_grade_overall)
    # Plot
    gvisHistogram(t,
                  options = list(legend = "none",
                                 title="Distribution of Course Grades (for completers)")
    )
  })
  # Graph of when completed
  output$completersPM <- renderGvis({
    t <- compData()$completed_time %>%
      filter(., course_passing_state_id != 0) %>%
      mutate(., month = format(as.Date(ts_conv), "%B"),
             month_num = format(as.Date(ts_conv), "%m"),
             year = format(as.Date(ts_conv), "%Y")) %>%
      group_by(., month_num, month, year) %>%
      summarize(., count = n()) %>%
      ungroup(.) %>%
      mutate(., month_num = as.numeric(month_num)) %>%
      mutate(., monthyear = paste0(month, " ", year)) %>%
      arrange(., year, month_num) %>%
      select(., monthyear, count)
    # Plot
    gvisColumnChart(t, "monthyear", "count",
                    options = list(legend = "none",
                                   title="Number of Completers, by Month"))

  })
  
}
