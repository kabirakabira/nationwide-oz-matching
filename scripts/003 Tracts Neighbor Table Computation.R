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

## Read in Tract Geometry Data
tracts.geometry <- st_read("data/processed/Tracts_Geos.gpkg")

## Find neighbors
if (!file.exists("data/processed/Tracts_Neighbors_SGBP.RData")) {
  neighbors <- st_touches(tracts.geometry)
  save(neighbors, file = "data/processed/Tracts_Neighbors_SGBP.RData")
} else {
  load("data/processed/Tracts_Neighbors_SGBP.RData")
}

### Drop geo
tracts.geometry <- tracts.geometry %>% st_drop_geometry()

### Split process into N dataframes
desired.split = 50

for (i in 1:desired.split) {
  assign(paste0("neighbors.", i), tracts.geometry[0,])
}

for (i in 1:nrow(tracts.geometry)) {
  start.time <- Sys.time()
  row.nos <- c(i, neighbors[[i]])
  temp <- tracts.geometry[row.nos,]
  temp <- temp %>%
    mutate(Parent_GEOID = tracts.geometry$GEOID[i])

  current.index <- ceiling(i / ceiling(nrow(tracts.geometry) / desired.split))
  
  assign((paste0("neighbors.", current.index)), rbind(get(paste0("neighbors.", current.index)), temp))
  
  out.txt <- paste0("Completed row ", i, " in ", as.character(Sys.time() - start.time), " seconds.")
  print(out.txt)
}

neighbor.groups <- tracts.geometry[0,]

for (i in 1:desired.split) {
  neighbor.groups <- neighbor.groups %>%
    rbind(get(paste0("neighbors.", i)))
  print(paste0("Joined neighbor df ", i))
}

## Write out
write.csv(neighbor.groups,
          "data/processed/Tracts_NeighborGroups.csv",
          row.names = F)
