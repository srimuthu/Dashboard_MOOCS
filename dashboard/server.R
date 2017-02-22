
#
# Server logic for Coursera data dashboard.
# Project by Leiden University (Jasper Ginn) & EIT digital (S.M.N.Balasubramanian)
#
# Development version
#

library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
library(purrr)
library(DBI)
course_list <- jsonlite::fromJSON("settings/course_list.json")

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
  
  
  # Comparison output - Dynamically create outputs
  
  num_outputs <- 6
  output$comparisonText <- renderUI({
    compOutputText <- lapply(1:num_outputs, function(i) {
      textName <- paste("compText", i, sep="")
      h3(textOutput(textName))
    })
    
    # Convert the list to a tagList
    do.call(tagList, compOutputText)
  })
  
  output$comparisonCourse1 <- renderUI({
    compOutputC1 <- lapply(1:num_outputs, function(i) {
      textName <- paste("compOutputC1text", i, sep="")
      h3(textOutput(textName))
    })
    
    # Convert the list to a tagList
    do.call(tagList, compOutputC1)
  })
  
  output$comparisonCourse2 <- renderUI({
    compOutputC2 <- lapply(1:num_outputs, function(i) {
      textName <- paste("compOutputC2text", i, sep="")
      h3(textOutput(textName))
    })
    
    # Convert the list to a tagList
    do.call(tagList, compOutputC2)
  })
  
  output$comparisonCourse3 <- renderUI({
    compOutputC3 <- lapply(1:num_outputs, function(i) {
      textName <- paste("compOutputC3text", i, sep="")
      h3(textOutput(textName))
    })
    
    # Convert the list to a tagList
    do.call(tagList, compOutputC3)
  })
  
  
  
  # Call renderText for each one
  for(i in 1:num_outputs){
    local({
      this_i <- i
      textName <- paste("compText",this_i,sep="")
      
      output[[textName]] <- renderText({
        op <- switch(
          this_i,
          "Enrolled Students",
          "Active Students",
          "Completers",
          "Viewers",
          "Payments",
          "Financial aid"
        )
        paste(op)
      })
    })
  }
  
  for(i in 1:num_outputs){
    local({
      this_i <- i
      textName <- paste("compOutputC1text",this_i,sep="")
      
      output[[textName]] <- renderText({
        con <- src_sqlite("data/summary_stats.sqlite")
        cn <- names(which(course_list == input$tabOverviewCompareCourse1))
        cec <- tbl(con, "summary_stats") %>%
          filter(course == cn) %>%
          collect()
        dbDisconnect(con$con)
        op <- switch(
          this_i,
          cec$new_enrolments,
          cec$active_students,
          cec$course_completers,
          cec$viewing_students,
          cec$payments,
          cec$financial_aid
        )
        paste(op)
      })
    })
  }
  
  for(i in 1:num_outputs){
    local({
      this_i <- i
      textName <- paste("compOutputC2text",this_i,sep="")
      
      output[[textName]] <- renderText({
        con <- src_sqlite("data/summary_stats.sqlite")
        cn <- names(which(course_list == input$tabOverviewCompareCourse2))
        cec <- tbl(con, "summary_stats") %>%
          filter(course == cn) %>%
          collect()
        dbDisconnect(con$con)
        op <- switch(
          this_i,
          cec$new_enrolments,
          cec$active_students,
          cec$course_completers,
          cec$viewing_students,
          cec$payments,
          cec$financial_aid
        )
        paste(op)
      })
    })
  }
  
  for(i in 1:num_outputs){
    local({
      this_i <- i
      textName <- paste("compOutputC3text",this_i,sep="")
      
      output[[textName]] <- renderText({
        con <- src_sqlite("data/summary_stats.sqlite")
        cn <- names(which(course_list == input$tabOverviewCompareCourse3))
        cec <- tbl(con, "summary_stats") %>%
          filter(course == cn) %>%
          collect()
        dbDisconnect(con$con)
        op <- switch(
          this_i,
          cec$new_enrolments,
          cec$active_students,
          cec$course_completers,
          cec$viewing_students,
          cec$payments,
          cec$financial_aid
        )
        paste(op)
      })
    })
  }

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
    # Get map color scheme input
    csi <- input$mapColorSchemeInput
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
    if(csi == "Quantiles"){
    # Create color palette
    pal <- colorQuantile(RColorBrewer::brewer.pal(5, "Blues"), 
                         domain = tmpvalues$n, n=5)
    # Validate that quantiles are unique
    validate(
      need(
        length(unique(quantile(tmpvalues$n))) == 5, "Not enough data to produce unique breaks for quantile values. Please extend the data range."
      )
    )
    } else if(csi == "Bin"){
      # Create color palette
      pal <- colorBin(RColorBrewer::brewer.pal(5, "Blues"), 
                           domain = tmpvalues$n, n=5)
    } else if(csi == "Numeric"){
      # Create color palette
      pal <- colorNumeric(RColorBrewer::brewer.pal(5, "Blues"), 
                      domain = tmpvalues$n, n=5)
    }
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

  # ---------------------------------------
  # TAB VIDEOS OUTPUT
  # --------------------------------------- 
  # Total videos
  output$valueBoxTotalVideos <- renderValueBox({
    d<-dates()
    # Connect to postgres
    con <- psql(psql_host(), psql_port(), psql_user(), psql_pwd(), psql_db())
    # calculate total students
    TV <- totalVideos(con)
    # Close connection
    dbDisconnect(con$con)
    # Valuebox
    valueBox(
      TV, "Total videos", icon = icon("video-camera"), color = "purple"
    )
  })
  
  # Create a drop-down list for available videos
  output$tabVideosSelectVideo <- renderUI({
    # Get dates
    d<-dates()
    # Connect to postgres
    con <- psql(psql_host(), psql_port(), psql_user(), psql_pwd(), psql_db())
    # Retrieve tests
    t <- retrieveVideosList(con, from = d[1], to = d[2]) 
    # Close connection
    dbDisconnect(con$con)
    # Add selectize input
    selectInput("videosSelectVideo", "Select a video:", 
                choices = unique(t$course_item_name), selected = 1, width = "75%")
  }) 
  
  # Completers per video
  output$valueBoxCompletersPerVideo <- renderValueBox({
    # Get dates
    d<-dates()
    # Connect to postgres
    con <- psql(psql_host(), psql_port(), psql_user(), psql_pwd(), psql_db())
    # Retrieve completers for the video
    t <- completersPerVideo(con, eval(input$videosSelectVideo), from = d[1], to = d[2]) 
    # Close connection
    dbDisconnect(con$con)
  
    valueBox(
      t, "Completers", icon = icon("user-circle-o"), color = "purple"
    )
        
  })
  
  # Completers per video graph
  output$tabVideosCompletersGraph <- renderPlotly({
    # Get dates
    d<-dates()
    # Connect to postgres
    con <- psql(psql_host(), psql_port(), psql_user(), psql_pwd(), psql_db())
    # Retrieve tests
    t <- retrieveVideosList(con, from = d[1], to = d[2]) 
    # Close connection
    dbDisconnect(con$con)
    # Filter the data and compile into a data frame
    t <- t %>%
      filter(course_progress_state_type_id == 2) %>%
      select(course_item_name)
    cv <- table(t)
    CPV <- as.data.frame(cv)  
    
    # Rename columns
    colnames(CPV)[colnames(CPV)=="t"] <- "Video"
    colnames(CPV)[colnames(CPV)=="Freq"] <- "Completers"
    
    # Plot
    p <- ggplot(CPV, aes(x = Video, 
                        y = Completers)) +
      geom_bar(stat= "identity") +
      theme_cfi_scientific() +
      scale_fill_manual(values = CFI_palette(),
                        guide = guide_legend(reverse = TRUE)) +
      scale_x_discrete(name = "",
                       expand = c(0.01,0))  +
      scale_y_continuous(expand = c(0.01,0))
    # Plotly
    ggplotly(p)

  })
  
  # ---------------------------------------
  # TAB GRADED QUIZZES OUTPUT
  # --------------------------------------- 
  
  # When the selected course changes, reset all values
  observeEvent(input$selectCourse, {
    # Reset the shiny input value to NULL (else it keeps the input value of a previous course if that course had branches)
    # resetValue is defined in ui.R on lines 53-55
    session$sendCustomMessage(type = "resetValue", message = "gradedTestSelectBranch")
    session$sendCustomMessage(type = "resetValue", message = "gradedTestSelectQuiz")
    session$sendCustomMessage(type = "resetValue", message = "gradedTestSelectQuizVersion")
  })
  
  # When the quiz question changes, reset quiz values and quiz versions
  observeEvent(input$gradedTestSelectQuiz, {
    session$sendCustomMessage(type = "resetValue", message = "gradedTestSelectQuizVersion")
  })
  
  # If the course has branches, show branches
  output$tabGradedTestsSelectBranch <- renderUI({
    d <- dates()
    # Get course selection
    c <- psql_db()
    # Connect to postgres
    con <- psql(psql_host(), psql_port(), psql_user(), psql_pwd(), psql_db())
    # If branches exist, query
    if(checkIfMultipleBranches(con)) {
      t <- retrieveGradedTests(con, from = d[1], to = d[2]) %>%
        select(course_branch_id) 
      # Return a selectInput
      ret <- selectInput("gradedTestSelectBranch", "Select a branch:", 
                    choices = unique(t$course_branch_id), selected = 1, width = "75%")
    } else {
      ret <- NULL
    }
    # Disconnect
    dbDisconnect(con$con)
    return(ret)
  })
  
  # Create a drop-down list for available quizzes
  output$tabGradedTestsSelectQuiz <- renderUI({
    # Get dates
    d<-dates()
    # Connect to postgres
    con <- psql(psql_host(), psql_port(), psql_user(), psql_pwd(), psql_db())
    # Retrieve tests
    t <- retrieveGradedTests(con, from = d[1], to = d[2]) 
    # Close connection
    dbDisconnect(con$con)
    # If branch is select, filter for branch
    if(!is.null(input$gradedTestSelectBranch)) {
      t <- t %>%
        filter(course_branch_id == input$gradedTestSelectBranch)
    } 
    # Add selectize input
    selectInput("gradedTestSelectQuiz", "Select a quiz:", 
                choices = unique(t$course_item_name), selected = 1, width = "75%")
  }) 
  
  # Create a drop-down list for available versions
  output$tabGradedTestsSelectQuizVersion <- renderUI({
    # If no quiz selected, return NULL
    if(is.null(input$gradedTestSelectQuiz)) {
      return(NULL)
    }
    # Get dates
    d<-dates()
    # Connect to postgres
    con <- psql(psql_host(), psql_port(), psql_user(), psql_pwd(), psql_db())
    # Retrieve tests
    t <- retrieveGradedTests(con, from = d[1], to = d[2]) %>%
      # Create version
      mutate(version = getIds(assessment_id)) %>%
      # Arrange each item by version number
      group_by(course_item_name) %>%
      arrange(version) %>%
      ungroup()
    # Close connection
    dbDisconnect(con$con)
    # If branch is select, filter for branch
    if(!is.null(input$gradedTestSelectBranch)) {
      t <- t %>%
        filter(course_branch_id == input$gradedTestSelectBranch) %>%
        # Then filter for question
        filter(course_item_name == input$gradedTestSelectQuiz)
    } else { # Filter for question
      t <- t %>%
        filter(course_item_name == input$gradedTestSelectQuiz)
    }
    # Return selectize input for quiz version
    selectInput("gradedTestSelectQuizVersion", 
                "Select a quiz version:", choices = unique(t$version), 
                selected = length(unique(t$version)),
                width = "75%")
  })
  
  # Reactive value to select quiz + version on branch
  quizData <- reactive({
    # Get dates
    d<-dates()
    # Get select input data
    branch <- input$gradedTestSelectBranch
    quiz <- input$gradedTestSelectQuiz
    version.p <- input$gradedTestSelectQuizVersion
    # If quiz / version are NULL, return NULL
    if(is.null(quiz) | is.null(version.p)) {
      return(NULL)
    }
    # Connect to postgres
    con <- psql(psql_host(), psql_port(), psql_user(), psql_pwd(), psql_db())
    # Retrieve tests
    t <- retrieveGradedTests(con, from = d[1], to = d[2]) %>%
      # Create version
      mutate(version = getIds(assessment_id)) 
    # Disconnect
    dbDisconnect(con$con)
    # If branch is not NULL, select it
    if(!is.null(branch)) {
      t <- t %>%
        filter(course_branch_id == branch)
    }
    # Filter for quiz and version
    t <- t %>%
      filter(course_item_name == quiz,
             version == as.numeric(version.p))
    # Return
    return(t)
  })
  
  # Value box for average grade
  output$valueBoxAverageQuizGrade <- renderValueBox({
    d <- quizData()
    # Validate data
    validate(
      need(!is.null(d), "Loading ...")
    )
    # Attempt
    at <- input$gradedTestSelectAttempt
    # Open connection
    con <- psql(psql_host(), psql_port(), psql_user(), psql_pwd(), psql_db())
    # Query data
    g <- quizAvgGrade(con, d$assessment_id, d$course_item_id, at)
    # Close conncetion
    dbDisconnect(con$con)
    if(is.null(g)) g <- "-"
    # Valuebox
    valueBox(g, "Average grade", icon=icon("star"), color = "purple")
  })
  
  # Value box for total number of responses
  output$valueBoxQuizTotalResponses <- renderValueBox({
    d <- quizData()
    # Validate data
    validate(
      need(!is.null(d), "Loading ...")
    )
    # Open connection
    con <- psql(psql_host(), psql_port(), psql_user(), psql_pwd(), psql_db())
    # Query data
    g <- quizTotalResponses(con, d$assessment_id, d$course_item_id)
    # Close conncetion
    dbDisconnect(con$con)
    # Valuebox
    valueBox(g, "Total responses", icon=icon("clipboard"), color = "purple")
  })
  
  # Value box for total number of unique users
  output$valueBoxQuizUniqueLearners <- renderValueBox({
    d <- quizData()
    # Validate data
    validate(
      need(!is.null(d), "Loading ...")
    )
    # Open connection
    con <- psql(psql_host(), psql_port(), psql_user(), psql_pwd(), psql_db())
    # Query data
    g <- quizUniqueUsers(con, d$assessment_id, d$course_item_id)
    # Close conncetion
    dbDisconnect(con$con)
    # Valuebox
    valueBox(g, "Unique learners", icon=icon("user"), color = "purple")
  })
  
  # Density plot for grade distribution
  output$tabGradedTestsQuizDistribution <- renderPlotly({
    d <- quizData()
    # Validate data
    validate(
      need(!is.null(d), "Loading ...")
    )
    # Open connection
    con <- psql(psql_host(), psql_port(), psql_user(), psql_pwd(), psql_db())
    # Query data
    g <- quizGetGrades(con, d$assessment_id, d$course_item_id)
    # Need number of rows > 0
    validate(
      need(nrow(g) > 0, "No quiz scores for this assessment.")
    )
    # Close conncetion
    dbDisconnect(con$con)
    # Plot
    p <- ggplot(g, aes(x=course_item_grade_overall)) +
      geom_density(fill = "#0073B7") +
      theme_cfi_scientific() 
    ggplotly(p)
  })
  
  # Reactive for attempt scores
  attempt <- reactive({
    input$pvalritscoreselectattempt
  })
  
  # Histogram for RIT scores
  output$tabGradedTestsQuizPvalRIT <- renderPlotly({
    d <- quizData()
    # Validate data when is null
    validate(
      need(!is.null(d), "Loading ...")
    )
    # Get attempt
    at <- attempt()
    cat(at)
    # Open connection
    con <- psql(psql_host(), psql_port(), psql_user(), psql_pwd(), psql_db())
    # Collect test stats
    ts <- queryTestStats(con, d$assessment_id, d$course_item_id, attempt = at)
    # Disconnect
    dbDisconnect(con$con)
    # Validate ts data
    validate(
      need(!is.null(ts), "Loading ...")
    )
    # Ritscores & pval
    pvals <- calculatePValue(ts$sumStats)
    # Validate pvals
    validate(
      need(!is.null(pvals), "Cannot calculate this statistic for this quiz.")
    )
    ritscores <- ritScore(ts$sumStats, ts$sd)
    # Validate ritscores
    validate(
      need(!is.null(ritscores), "Cannot calculate this statistic for this quiz.")
    )
    # Join together
    joined <- pvals %>%
      inner_join(ritscores, by = "assessment_question_id") %>%
      left_join(ts$sumStats, by = "assessment_question_id")
    # Label
    joined$color <- classifyQuizItems(joined$pval, joined$ritscore)
    # Plot
    p <- ggplot(joined, aes(x=pval, y=ritscore, color = color, label = label)) +
      geom_point() +
      theme_cfi_scientific() +
      scale_x_continuous(limits=c(0,1),
                         name = "P-value") +
      scale_y_continuous(name = "RIT score",
                         limits = c(-1,1)) +
      theme(axis.text.x = element_text(),
            axis.ticks.x = element_line(),
            axis.title.x = element_text(),
            axis.title.y = element_text(),
            legend.position = "none") +
      scale_color_manual(values = c("green", "orange", "red"),
                         name  = "Color coding")
    
    ggplotly(p)
  })
  
  # ---------------------------------------
  # TAB FORUM OUTPUT
  # ---------------------------------------  
  
  # Active Forum Users
  output$valueBoxActiveForumUsers <- renderValueBox({
    d<-dates()
    # Connect to postgres
    con <- psql(psql_host(), psql_port(), psql_user(), psql_pwd(), psql_db())
    # calculate total users
    TS <- activeForumUsers(con, from = d[1], to = d[2])
    # Close connection
    dbDisconnect(con$con)
    # Valuebox
    valueBox(
      TS, "Number of unique forum users", 
      icon = icon("users"), color = "purple"
    )
  })
  
  # Active forum posters
  output$valueBoxActiveForumPosters <- renderValueBox({
    d<-dates()
    # Connect to postgres
    con <- psql(psql_host(), psql_port(), psql_user(), psql_pwd(), psql_db())
    # calculate total students
    TS <- activeForumPostInitiators(con, from = d[1], to = d[2])
    # Close connection
    dbDisconnect(con$con)
    # Valuebox
    valueBox(
      TS, "Number of students who initiated a post",
      icon = icon("clipboard"), color = "purple"
    )
  })
  
  # Students who respond to a post
  output$valueBoxActiveForumResponders <- renderValueBox({
    d<-dates()
    # Connect to postgres
    con <- psql(psql_host(), psql_port(), psql_user(), psql_pwd(), psql_db())
    # calculate total number of students who respond to a post
    TS <- activeForumPostResponders(con, from = d[1], to = d[2])
    # Close connection
    dbDisconnect(con$con)
    # Valuebox
    valueBox(
      TS, "Number of students who responded to a post", 
      icon = icon("mail-reply"), color = "purple"
    )
  })
  
  # Course completers
  output$valueBoxUniqueForumPosts<- renderValueBox({
    d<-dates()
    # Connect to postgres
    con <- psql(psql_host(), psql_port(), psql_user(), psql_pwd(), psql_db())
    # calculate total students
    TS <- forumPosts(con, from = d[1], to = d[2])
    # Close connection
    dbDisconnect(con$con)
    # Valuebox
    valueBox(
      TS, "Unique posts", icon = icon("file-text"), color = "blue"
    )
  })
  
  # Number of unique responses
  output$valueBoxUniqueForumResponses <- renderValueBox({
    d<-dates()
    # Connect to postgres
    con <- psql(psql_host(), psql_port(), psql_user(), psql_pwd(), psql_db())
    # calculate total students
    TS <- forumResponses(con, from = d[1], to = d[2])
    # Close connection
    dbDisconnect(con$con)
    # Valuebox
    valueBox(
      TS, "Unique responses", icon = icon("mail-reply-all"), color = "blue"
    )
  })
  
  # Average post length
  output$valueBoxAveragePostLength <- renderValueBox({
    d<-dates()
    # Connect to postgres
    con <- psql(psql_host(), psql_port(), psql_user(), psql_pwd(), psql_db())
    # calculate total students
    TS <- averagePostLength(con, from = d[1], to = d[2])
    if(is.nan(TS)) TS <- "-"
    # Close connection
    dbDisconnect(con$con)
    # Valuebox
    valueBox(
      TS, "Average post length", icon = icon("long-arrow-right"), color = "blue"
    )
  })
  
  
  
}
