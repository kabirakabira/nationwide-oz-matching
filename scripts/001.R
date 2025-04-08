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

variable.df <- read.csv("dependencies/variable_list.csv")

tracts.nationwide <- get_acs(
  geography = "tract",
  state = state.list[1],
  year = 2015,
  survey = "acs5",
  variables = variable.df$Var.Code
) %>%
  select(-NAME, -moe) %>%
  pivot_wider(names_from = variable,
              values_from = estimate)

for (i in 2:length(state.list)) {
  temp <- get_acs(
    geography = "tract",
    state = state.list[i],
    year = 2015,
    survey = "acs5",
    variables = variable.df$Var.Code
  ) %>%
    select(-NAME, -moe) %>%
    pivot_wider(names_from = variable,
                values_from = estimate)
  
  tracts.nationwide <- tracts.nationwide %>%
    rbind(temp)
}

BACKUP <- tracts.nationwide

### Cleaning Tract-level data
column.names <- colnames(tracts.nationwide)

for (i in 1:length(column.names)) {
  current.column <- paste0(column.names[i], "E")
  if (current.column %in% variable.df$Var.Code) {
    column.names[i] <- variable.df$Var.Desc[match(current.column, variable.df$Var.Code)]
  }
}

colnames(tracts.nationwide) <- column.names

## Get State-level Median Income stats
state.income <- get_acs(
  geography = "state",
  state = state.list,
  year = 2015,
  survey = "acs5",
  variables = "B19013_001E"
) %>%
  select(GEOID, estimate) %>%
  `colnames<-`(c("GEOID_ST", "MHHI_State"))

tracts.nationwide <- tracts.nationwide %>%
  mutate(GEOID_ST = substr(GEOID, 1,2)) %>%
  left_join(state.income, by = "GEOID_ST") %>%
  select(-GEOID_ST)
  

## Calculating columns
tracts.nationwide <- tracts.nationwide %>%
  mutate(
    
    ## Calculating percent of population with income:povert ratio less than 1.5
    Poverty.Below.150Percent = (IncomePovertyRatio_0.5 + IncomePovertyRatio_0.99 +
                                  IncomePovertyRatio_1.5) / IncomePovertyRatio_All,
    
    ## Unemployment Rate
    Unemployment.Rate = (Workers_Unemployed / Workers_LaborForce),
    
    ## Housing Cost Burdened
    Percent.CostBurdened = (OwnerOccupied_IncBelow20_CostOver30Percent +
                              OwnerOccupied_IncBelow35_CostOver30Percent +
                              OwnerOccupied_IncBelow50_CostOver30Percent +
                              OwnerOccupied_IncBelow75_CostOver30Percent +
                              RenterOccupied_IncBelow20_CostOver30Percent +
                              RenterOccupied_IncBelow35_CostOver30Percent +
                              RenterOccupied_IncBelow50_CostOver30Percent +
                              RenterOccupied_IncBelow75_CostOver30Percent) /
      OccupiedHousingUnits,
    
    ## Percent of population without high school diploma
    Percent.NoHSD = Education_NoHSDP / Education_Total,
    
    ## Percent of population with No Health Insurance
    Percent.NoInsurance = Insurance_NoCoverage / Insurance_Total,
    
    ## Percent of population over the age of 65
    Percent.AgeAbove65 = Population_Over65 / Population,
    
    ## Percent of population under the age of 18
    Percent.AgeUnder18 = Population_Under18 / Population,
    
    ## Percent of population with Disability
    Percent.Disabled = (Population_Disability_Under19 + 
                          Population_Disability_19to64 + 
                          Population_Disability_Over64) / 
      Population,
    
    ## Percent of households that are single-parent households
    Percent.SPH = (Households_SPH_Female + Households_SPH_Male) / 
      Households,
    
    ## Percent of households that are limited-english speaking households
    Percent.LimitedEnglishHH = (LimitedEnglishHouseholds_Spanish +
                                  LimitedEnglishHouseholds_IndoEuropean +
                                  LimitedEnglishHouseholds_AAPI +
                                  LimitedEnglishHouseholds_Other) / 
      Households,
    
    ## Percent of population belonging to minortiy groups (basically, not "White, Not Hispanic")
    Percent.Minority = 1 - (Minority_WhiteNotHispanic / Population),
    
    ## Percent of housing units that are in units with 10+ units (multi-structure)
    Percent.MSU = (MultiUnit_10to19 + MultiUnit_20to49 + 
                     MultiUnit_50plus) / MultiUnit_Total,
    
    ## Percent of housing units that are mobile homes
    Percent.MobileHome = MobileHomes / OccupiedHousingUnits,
    
    ## Percent of housing units with more than 1.0 occupant per room
    Percent.Overcrowded = (Overcrowding_OwnerOccupied_1to1.5Occupants + 
                             Overcrowding_OwnerOccupied_1.5to2Occupants + 
                             Overcrowding_OwnerOccupied_2plusOccupants +
                             Overcrowding_RenterOccupied_1to1.5Occupants + 
                             Overcrowding_RenterOccupied_1.5to2Occupants + 
                             Overcrowding_RenterOccupied_2plusOccupants) / 
      Overcrowding_Total,
    
    ## Percent of households with no access to vehicles
    Percent.NoVehicle = Vehicles_None / Vehicles_Total,
    
    ## Percent of population living in group quarters
    Percent.GroupQuarters = GroupQuartersPopulation / Population,
    
    ## Calculating (Census Tract MHHI - State MHHI)
    MHHI.TractMinusState = (Median.HH.Income - MHHI_State)
  )

tracts.nationwide <- tracts.nationwide %>%
  select(
    GEOID,
    
    Poverty.Below.150Percent,
    Unemployment.Rate,
    Percent.CostBurdened,
    Percent.NoHSD,
    Percent.NoInsurance,
    
    Percent.AgeAbove65,
    Percent.AgeUnder18,
    Percent.Disabled,
    Percent.SPH,
    Percent.LimitedEnglishHH,
    
    Percent.Minority,
    
    Percent.MSU,
    Percent.MobileHome,
    Percent.Overcrowded,
    Percent.NoVehicle,
    Percent.GroupQuarters,
    
    MHHI.TractMinusState
  )

## 1,004 rows dropped for NAs
tracts.nationwide <- tracts.nationwide[complete.cases(tracts.nationwide),]

#### Read in and add OZ-designation
oz.designations <- read.csv("dependencies/OZ_Tracts.csv",
                            colClasses = c("character", "character", "character"))

tracts.nationwide <- tracts.nationwide %>%
  mutate(
    Is_OZ = ifelse(GEOID %in% oz.designations$GEOID, 1, 0)
  )

















