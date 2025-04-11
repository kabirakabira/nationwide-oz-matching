#! Setting up workspace
## Modifying options
options(scipen = 999)
options(timeout = 180)
options(tigris_use_cache = TRUE)
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
census_api_key(readLines("dependencies/censuskey.txt", warn = F))

## Folder structure
unlink("data/", recursive = TRUE)
dir.create("data")
dir.create("data/raw")
dir.create("data/processed")
dir.create("data/output")

## Download list of census tracts in OZs
download.file("https://www.cdfifund.gov/sites/cdfi/files/documents/designated-qozs.12.14.18.xlsx",
              destfile = "data/raw/OZ_Tracts.xlsx")

## Get data on all census tracts nationwide
state.list <- c(
  "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", 
  "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", 
  "LA", "ME", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", 
  "NC", "ND", "OH", "OK", "OR", "MD", "MA", "MI", "MN", 
  "MS", "MO", "PA", "RI", "SC", "SD", "TN", "TX", "UT", 
  "VT", "VA", "WA", "WV", "WI", "WY"
)

tracts.nationwide <- get_acs(
  geography = "tract",
  state = state.list[1],
  year = 2015,
  survey = "acs5",
  variables = "B01001_001E",
  geometry = TRUE
) %>%
  select(GEOID, geometry)

for (i in 2:length(state.list)) {
  temp <- get_acs(
    geography = "tract",
    state = state.list[i],
    year = 2015,
    survey = "acs5",
    variables = "B01001_001E",
    geometry = TRUE
  ) %>%
    select(GEOID, geometry)
  
  tracts.nationwide <- tracts.nationwide %>%
    rbind(temp)
}

BACKUP <- tracts.nationwide

### Read in and add OZ designation
oz.designations <- read.csv("dependencies/OZ_Tracts.csv",
                            colClasses = c("character", "character", "character"))

tracts.nationwide <- tracts.nationwide %>%
  mutate(
    Is_OZ = ifelse(GEOID %in% oz.designations$GEOID, 1, 0)
  )

### Write out
st_write(tracts.nationwide,
         "data/processed/Tracts_OZdesignation.shp")


