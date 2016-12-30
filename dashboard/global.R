#Global settings

# Load geojson with countries for map
countries <- rgdal::readOGR("data/map_layers/countries.geojson", "OGRGeoJSON")

# -------------------------------
# GENERIC HELPER FUNCTIONS
# -------------------------------

source("functions/psql.R")
source("functions/ggplot_theme_cfi.R")

# -------------------------------
# TAB DASHBOARD HELPER FUNCTIONS
# -------------------------------

source("functions/dashboard/totalUsers.R")
source("functions/dashboard/activeStudents.R")
source("functions/dashboard/viewingStudents.R")
source("functions/dashboard/courseCompleters.R")
source("functions/dashboard/numberPayments.R")
source("functions/dashboard/numberFinancialAid.R")
source("functions/dashboard/usersOverTime.R")

# -------------------------------
# TAB GEOGRAPHY HELPER FUNCTIONS
# -------------------------------

source("functions/geography/countryOfOrigin.R")

# -------------------------------
# TAB VIDEOS HELPER FUNCTIONS
# -------------------------------

source("functions/videos/totalVideos.R")
source("functions/videos/retrieveVideosList.R")

# -------------------------------
# TAB FORUM HELPER FUNCTIONS
# -------------------------------

source("functions/forum/activeForumInitiators.R")
source("functions/forum/activeForumUsers.R")
source("functions/forum/activeForumResponders.R")
source("functions/forum/uniqueForumPosts.R")
source("functions/forum/uniqueForumResponses.R")
source("functions/forum/averagePostLength.R")

# ----------------------------------
# TAB GRADED TESTS HELPER FUNCTIONS
# ----------------------------------

source("functions/gradedTests/retrieveGradedTests.R")
source("functions/gradedTests/getIds.R")
source("functions/gradedTests/checkIfMultipleBranches.R")
source("functions/gradedTests/quizAverageGrade.R")
source("functions/gradedTests/quizTotalResponses.R")
source("functions/gradedTests/quizUniqueLearners.R")
source("functions/gradedTests/getQuizGrades.R")
source("functions/gradedTests/calculatePValue.R")
source("functions/gradedTests/classifyQuizItems.R")
source("functions/gradedTests/queryTestStats.R")
source("functions/gradedTests/ritScore.R")
