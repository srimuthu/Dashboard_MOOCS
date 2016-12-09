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
