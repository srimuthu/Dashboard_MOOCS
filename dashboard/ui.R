
#
# User Interface for Coursera data dashboard.
# Project by Leiden University (Jasper Ginn) & EIT digital (S.M.N.Balasubramanian)
#
# Development version
#

library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)

# Load json with postgresql defaults
postgres_defaults <- jsonlite::fromJSON("settings/postgres_defaults.json")
course_list <- jsonlite::fromJSON("settings/course_list.json")

# Header -----

header <- dashboardHeader(
  title = "MOOC Dashboard" # Let's give this a generic name for now
)

# Sidebar ----

# TODO: Settings below (mailto etc) should be customizable in a file maybe

sidebar <-   dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", tabName = "coursesoverview"), 
    menuItem("Dashboard", tabName = "dashboard"),
    menuItem("Geography", tabName = "geography"),
    menuItem("Graded assignments", tabName = "gradedassignments"),
    menuItem("Forum", tabName = "forum"),
    menuItem("Settings", tabName = "settings"),
    menuItem("Contact", href = "mailto:s.m.n.balasubramanian@student.tue.nl", 
             icon = icon("envelope")),
    selectInput("selectCourse", label = h3("Select Course"), 
                choices = course_list, 
                selected = 1),
    dateRangeInput("daterange", 
                   label = h3("Select Date Range"), 
                   start = Sys.Date() - 30,
                   end = Sys.Date())
  )
)

# Body -----

body <- dashboardBody(
  
  # Use custom CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  
  tabItems(
    #EIT Overview
    tabItem(tabName = "coursesoverview",
            fluidRow(
              valueBoxOutput("valueBoxSummaryTotalStudents"),
              valueBoxOutput("valueBoxSummaryCompleted"),
              valueBoxOutput("valueBoxSummaryPayments")
            ),
            fluidRow(
              column(
                width = 6,
                box(
                  width = NULL,
                  title = "Conversion rate",
                  p("The conversion rate is calculated by dividing the number of paying students by the total number of enrolled students."),
                  plotlyOutput("chartSummaryConversion", height = "300px")
                )
              ),
              column(
                width = 6,
                box(
                  width = NULL,
                  title = "Graduation percentage",
                  p("The graduation percentage is calculated by dividing the number of students who have finished the course by the total number of enrolled students."),
                  plotlyOutput("chartSummaryGraduation", height = "300px")
                )
              )
            )
            ),
    #Dashboard
    tabItem(tabName = "dashboard",
            fluidRow(
              valueBoxOutput("valueBoxTotalStudents"),
              valueBoxOutput("valueBoxActiveStudents"),
              valueBoxOutput("valueBoxBrowsingStudents"),
              valueBoxOutput("valueBoxCourseCompleters"),
              valueBoxOutput("valueBoxPayments"),
              valueBoxOutput("valueBoxFinancialAid"),
              box(width = 12,
                  title = "Enrollers over the past 120 days",
                  plotlyOutput("chartUsersOverTime"))
              )
            ),
    # Geography
    tabItem(tabName = "geography",
            fluidRow(
              column(
                width = 8,
                box(
                  width = NULL,
                  title = "Where do learners come from?",
                  leafletOutput("leafletMapCountryOfOrigin",
                                width = "100%",
                                height = "500")
                )
              ),
              column(
                width = 4,
                box(
                  width = NULL,
                  title = "Map settings",
                  p("You can show counts, or you can normalize the raw figures by population and by the number of internet users in a country."),
                  selectInput("mapSettingsInput", "", choices = c("None", 
                                                                  "Population", 
                                                                  "Internet users"),
                              selected = "None")
                )
              )
            )

            ),
    # Forum stats
    tabItem(tabName = "forum",
            fluidRow(
              valueBoxOutput("valueBoxActiveForumUsers"),
              valueBoxOutput("valueBoxActiveForumPosters"),
              valueBoxOutput("valueBoxActiveForumResponders"),
              valueBoxOutput("valueBoxUniqueForumPosts"),
              valueBoxOutput("valueBoxUniqueForumResponses"),
              valueBoxOutput("valueBoxAveragePostLength")
            )
    ),
    # Settings
    tabItem(tabName = "settings",
            # Fluidrow is bootstrap allocation of space (essentially 12 columns)
            fluidRow(
              # Here, we allocate a column of width 4 
              column(
                width = 4,
                # And in that we put the box
                box(
                  width = NULL,
                  h3("PostgreSQL settings"),
                  textInput(inputId = "psqlhostname", 
                            label = "Hostname", 
                            value = postgres_defaults$hostname),
                  textInput(inputId = "psqlport", 
                            label = "Port", 
                            value = postgres_defaults$port),
                  textInput(inputId = "psqlusername", 
                            label = "User", value = postgres_defaults$user),
                  textInput(inputId = "psqlpassword", 
                            label = "Password", value = postgres_defaults$password),
                  textInput(inputId = "psqldatabase", 
                            label = "Database", value = postgres_defaults$database),
                  p("Click to save as default"),
                  actionButton("saveSettingsButton", "Save")
                )
              ),
              # This column is for course settings
              column(
                width=4,
                box(
                  width = NULL,
                  h3("Course names"),
                  textAreaInput(inputId = "courses", 
                                label = "List of courses",
                                value = paste0(
                                    names(course_list), " = ", unlist(unname(course_list)),
                                    collapse = ",\n"
                                ),
                                rows = ifelse(length(course_list) <= 10, 
                                              length(course_list), 10)
                              ),
                  p("Click to save as default"),
                  actionButton("saveCoursesButton", "Save")
                )
              )
            )
      )
    )
)

# Pull it together ---- 

dashboardPage(
  header,
  sidebar,
  body,
  skin = "blue"
)