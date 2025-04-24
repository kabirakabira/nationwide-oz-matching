#! Setting up workspace
## Modifying options
options(scipen = 999)
options(timeout = 180)
## Installing and loading required packages
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
usePackage("dplyr")
usePackage("tidyr")
usePackage("tidycensus")
usePackage("tigris")
usePackage("sf")
usePackage("readxl")
usePackage("leaflet")
census_api_key(readLines("dependencies/censuskey.txt", warn = F))

## Read in:
### tract time series data
tract.timeseries <- read.csv("data/processed/Tracts_TimeSeries.csv")
### tract neighbor groups table
tract.neighbors <- read.csv("data/processed/Tracts_NeighborGroups.csv")

## Summarize Neighbor Data
tract.neighbors <- tract.neighbors %>%
  mutate(Count = 1) %>%
  group_by(Parent_GEOID) %>%
  summarize(
    Total_Neighbors = sum(Count) - 1,
    OZ_Neighbors = sum(Is_OZ), ## This overcounts by 1 where GEOID == Parent_GEOID and Is_OZ == 1
  )
  
## Join to Tract Time Series Data and account for the previously mentioned overcount
tract.timeseries <- tract.timeseries %>%
  left_join(tract.neighbors,
            by = c("GEOID" = "Parent_GEOID")) %>%
  mutate(OZ_Neighbors = ifelse(Is_OZ == 1, OZ_Neighbors - 1, OZ_Neighbors))

## Write out updated tract time series
write.csv(tract.timeseries,
          "data/processed/Tracts_TimeSeries_WithNeighborData.csv",
          row.names = F)