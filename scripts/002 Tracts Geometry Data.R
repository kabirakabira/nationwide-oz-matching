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
census_api_key(readLines("dependencies/censuskey.txt", warn = F))## Get data on all census tracts nationwide

## Grab geometries for all tracts
state.list <- c(
  "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", 
  "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", 
  "LA", "ME", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", 
  "NC", "ND", "OH", "OK", "OR", "MD", "MA", "MI", "MN", 
  "MS", "MO", "PA", "RI", "SC", "SD", "TN", "TX", "UT", 
  "VT", "VA", "WA", "WV", "WI", "WY"
)

tracts.geometry <- get_acs(
  geography = "tract",
  state = state.list[1],
  year = 2012,
  survey = "acs5",
  variables = "B01001_001E",
  geometry = TRUE
) %>%
  select(GEOID, geometry)

for (i in 2:length(state.list)) {
  temp <- get_acs(
    geography = "tract",
    state = state.list[i],
    year = 2012,
    survey = "acs5",
    variables = "B01001_001E",
    geometry = TRUE
  ) %>%
    select(GEOID, geometry)
  
  tracts.geometry <- tracts.geometry %>%
    rbind(temp)
}

### Read in and add OZ designation
oz.designations <- read.csv("dependencies/OZ_Tracts.csv",
                            colClasses = c("character", "character", "character"))

tracts.geometry <- tracts.geometry %>%
  mutate(
    Is_OZ = ifelse(GEOID %in% oz.designations$GEOID, 1, 0)
  )

### Write out
st_write(tracts.geometry,
         "data/processed/Tracts_Geos.gpkg", 
         delete_layer = TRUE)