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

## Join time series data to neighbor data
tract.neighbors <- tract.neighbors %>% 
  left_join(tract.timeseries %>% select(-Is_OZ),
            by = "GEOID")

## Summarize Neighbor Data
tract.neighbors <- tract.neighbors %>%
  mutate(Count = 1) %>%
  group_by(Parent_GEOID, Year) %>%
  summarize(
    Total_Neighbors = sum(Count) - 1,
    OZ_Neighbors = sum(Is_OZ), ## This overcounts by 1 where GEOID == Parent_GEOID and Is_OZ == 1
    N_Pov_Below100 = mean(Pov_Below100, na.rm = T),
    N_Pov_Below150 = mean(Pov_Below150, na.rm = T),
    N_MHHI = mean(MHHI, na.rm = T)
  )

## Join to time series data
tract.timeseries <- tract.timeseries %>%
  left_join(tract.neighbors, by = c("GEOID" = "Parent_GEOID", "Year")) %>%
  mutate(
    OZ_Neighbors = ifelse(Is_OZ == 1, OZ_Neighbors - 1, OZ_Neighbors) ## Account for the overcount
  )
  