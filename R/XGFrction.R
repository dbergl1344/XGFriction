library(devtools)
library(REddyProc)
library(xgboost)
library(roxygen2)
create_package("~/Documents/GitHub/XGFriction")

colnames(Bartlett_30_minute_data_July2023)

roxygen2::roxygenize()
# Define the XGFrictionProcessor class
XGFrictionProcessor <- setRefClass("XGFrictionProcessor",
                                   fields = list(site = "character"),
                                   methods = list(
                                     initialize = function(site) {
                                       site <<- site
                                     },
                                     initialize_xgfriction_processing = function(dataframe, LatDeg, LongDeg, TimeZoneHour) {
                                       # Check if the data is in 30-minute timestamp format
                                       if (any(diff(dataframe$DateTime) != 30*60)) {
                                         mins <- 15*60
                                         print("Adding 15 minutes to DateTime")
                                         print(dataframe$DateTime + mins)
                                         dataframe$DateTime <- (dataframe$DateTime + mins)
                                       }

                                       # Initialize the processing
                                       xgfriction_proc <- REddyProc::sEddyProc$new(site, dataframe$DateTime,
                                                                                   c('NEE', 'Rg', 'Tair', 'VPD', 'Ustar'))
                                       xgfriction_proc$sSetLocationInfo(LatDeg = LatDeg, LongDeg = LongDeg, TimeZoneHour = TimeZoneHour)
                                       return(xgfriction_proc)
                                     }
                                   )
)

#' Initialize XGFriction Processor
#'
#' This function initializes the XGFriction processor object.
#'
#' @param site Site name.
#' @return XGFrictionProcessor object.
#' @export
initialize_xgfriction_processor <- function(site) {
  return(XGFrictionProcessor$new(site))
}


##Test dataframe
# Subsetting to get data for the year 2007
Bartlett_2010 <- Bartlett_30_minute_data_July2023[Bartlett_30_minute_data_July2023$YEAR == 2010, ]

# check
print(sum(is.na(Bartlett_2010$NEE)))


##example usage for functions above:
# Create an instance of the XGFrictionProcessor class for the site 'US-Bar'
processor <- initialize_xgfriction_processor('US-Bar')
# Call the initialize_xgfriction_processing method with their dataframe and other parameters
xgfriction_proc <- processor$initialize_xgfriction_processing(Bartlett_2010, LatDeg = 40.7128, LongDeg = -74.0060, TimeZoneHour = -5)


