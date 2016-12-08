
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)

# Load json with postgresql defaults
postgres_defaults <- jsonlite::fromJSON(content = "settings/postgres_defaults.json")
course_list <- jsonlite::fromJSON(content = "settings/course_list.json")

# Header -----

header <- dashboardHeader(
  title = "EIT Digital Dashboard"
)

# Sidebar ----

# TODO: Settings below (mailto etc) should be customizable in a file maybe

sidebar <-   dashboardSidebar(
  sidebarMenu(
    menuItem("EIT-Overview", tabName = "eitOverview"),
    menuItem("Dashboard", tabName = "dashboard"),
    selectInput("selectCourse", label = h3("Select Course"), 
                choices = course_list, 
                selected = 1),
    menuItem("Settings", tabName = "settings"),
    menuItem("Contact", href = "mailto:s.m.n.balasubramanian@student.tue.nl", icon = icon("envelope"))
  )
)

# Body -----

body <- dashboardBody(
  tabItems(
    #EIT Overview
    tabItem(tabName = "eitOverview",
            titlePanel("EIT-Overview"),
            fluidRow(
              valueBoxOutput("enrolledOverview"),
              valueBoxOutput("activeStud2Weeks"),
              valueBoxOutput("viewersOverview"),
              valueBoxOutput("completersOverview"),
              valueBoxOutput("paymentUsersOverview")
            )
            ),
    #Dashboard
    tabItem(tabName = "dashboard",
            titlePanel("Dashboard"),
            fluidRow(
              valueBoxOutput("compRate"),
              valueBoxOutput("compTM"),
              valueBoxOutput("avgGr")
            ),

            ###Sign-up dates and completion rates

            fluidRow(
              #Grade distribution
              column(width = 6,
                     box(title = "Grade distribution", status = "primary", solidHeader = TRUE,
                         htmlOutput("histGrades"), width=12, height = NULL, background = "blue")
                     ),
            
              #Completion rates
              column(width = 6,
                     box(title = "Completion rates", status = "primary", solidHeader = TRUE,
                         htmlOutput("barChartComp"), width = 12, height = NULL, background = "blue")
                     )
            ),

            ### Completers per month and grade distribution

            fluidRow(
              #Completers per month
              column(width = 6,
                     box(title = "Completers per month", status = "primary", solidHeader = TRUE,
                         htmlOutput("completersPM"), width=12, height = NULL, background = "blue")
              ),
            
              #Sign-ups
              column(width = 6,
                     box(title = "Sign-up dates over time", status = "primary", solidHeader = TRUE,
                         htmlOutput("joinLine"), width = 12, height = NULL, background = "blue")
              )
            )
            ),
    tabItem(tabName = "settings",
            h3("PostgreSQL settings"),
            textInput(inputId = "psqlhostname", label = "Hostname", value = postgres_defaults$hostname),
            textInput(inputId = "psqlport", label = "Port", value = postgres_defaults$port),
            textInput(inputId = "psqlusername", label = "User", value = postgres_defaults$user),
            textInput(inputId = "psqlpassword", label = "Password", value = postgres_defaults$password),
            #textInput(inputId = "psqldatabase", label = "Database", value = postgres_defaults$database),
            p("Click to save as default"),
            actionButton("saveSettingsButton", "Save")
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