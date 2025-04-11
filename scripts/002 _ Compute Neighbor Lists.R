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

## Read in Tract Limited Data
tracts.nationwide <- st_read("data/processed/Tracts_OZdesignation.shp")

## Find neighbors for each OZ
time1 <- Sys.time()
oz.neighbors <- st_touches(tracts.nationwide)
neighbor.groups <- tracts.nationwide[0,]
for (i in 1:nrow(tracts.nationwide)) {
  if (tracts.nationwide$Is_OZ[i] == 1) {
    row.nos <- c(i, oz.neighbors[[i]])
    temp <- tracts.nationwide[row.nos,]
    temp <- temp %>%
      mutate(Group_byOZ = tracts.nationwide$GEOID[i])
    neighbor.groups <- neighbor.groups %>% rbind(temp)
  }
}
time2 <- Sys.time() - time1

## Write out neighbor list (as csv), don't need SF now that we have computed neighbors
write.csv(neighbor.groups %>% st_drop_geometry(),
          "data/processed/OZ_TractNeighbors.csv",
          row.names = F)
