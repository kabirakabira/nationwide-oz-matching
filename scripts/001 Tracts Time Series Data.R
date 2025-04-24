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

## Folder structure
unlink("data/", recursive = TRUE)
dir.create("data")
dir.create("data/raw")
dir.create("data/processed")
dir.create("data/output")

## Grab Universe Data
## Get data on all census tracts nationwide
state.list <- c(
  "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", 
  "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", 
  "LA", "ME", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", 
  "NC", "ND", "OH", "OK", "OR", "MD", "MA", "MI", "MN", 
  "MS", "MO", "PA", "RI", "SC", "SD", "TN", "TX", "UT", 
  "VT", "VA", "WA", "WV", "WI", "WY"
)

variable.list <- c(
  "B06012_001E", # Total Pop
  "B06012_002E", # Below 100%
  "B06012_003E", # 100-150%
  "B19013_001E"  # MHHI
)

year.list <- 2010:2019

tracts.nationwide <- get_acs(
  geography = "tract",
  state = state.list[1],
  year = year.list[1],
  survey = "acs5",
  variables = variable.list,
  geometry = FALSE
) %>%
  select(GEOID, variable, estimate) %>%
  mutate(Year = year.list[1])

for (i in 2:length(state.list)) {
  temp <- get_acs(
    geography = "tract",
    state = state.list[i],
    year = year.list[1],
    survey = "acs5",
    variables = variable.list,
    geometry = FALSE
  ) %>%
    select(GEOID, variable, estimate) %>%
    mutate(Year = year.list[1])
  
  tracts.nationwide <- tracts.nationwide %>%
    rbind(temp)
}

for (i in 2:length(year.list)) {
  
  temp <- get_acs(
    geography = "tract",
    state = state.list[1],
    year = year.list[i],
    survey = "acs5",
    variables = variable.list,
    geometry = FALSE
  ) %>%
    select(GEOID, variable, estimate) %>%
    mutate(Year = year.list[i])
  
  for (j in 2:length(state.list)) {
    temp.2 <- get_acs(
      geography = "tract",
      state = state.list[j],
      year = year.list[i],
      survey = "acs5",
      variables = variable.list,
      geometry = FALSE
    ) %>%
      select(GEOID, variable, estimate) %>%
      mutate(Year = year.list[i])
    
    temp <- temp %>%
      rbind(temp.2)
  }
  
  tracts.nationwide <- tracts.nationwide %>%
    rbind(temp)
  
}

BACKUP <- tracts.nationwide

## Clean up nationwide time series
tracts.nationwide <- tracts.nationwide %>%
  pivot_wider(names_from = variable,
              values_from = estimate) %>%
  mutate(Pov_Below100 = B06012_002 / B06012_001,
         Pov_Below150 = (B06012_002 + B06012_003) / B06012_001,
         MHHI = B19013_001) %>%
  select(GEOID, Year, Pov_Below100, Pov_Below150, MHHI)

### Read in and add OZ designation
oz.designations <- read.csv("dependencies/OZ_Tracts.csv",
                            colClasses = c("character", "character", "character"))

tracts.nationwide <- tracts.nationwide %>%
  mutate(
    Is_OZ = ifelse(GEOID %in% oz.designations$GEOID, 1, 0)
  )

### Write out .csv
write.csv(tracts.nationwide,
          "data/processed/Tracts_TimeSeries.csv",
          row.names = F)




