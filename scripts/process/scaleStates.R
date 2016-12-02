#scale states according to water use
# two different sized states with equal water use should have the 
# same area
library(rgeos)
library(dplyr)

process.scaleStates <- function(viz){
  #could put these in yaml too?
  coreCats <- c("Thermoelectric", "Irrigation", "Public Supply", "Industrial")
  
  #read sp, wuClean
  statePoly <- readData(viz[['depends']]$stateMap)
  wuClean <- readData(viz[['depends']]$wuClean)
  
  wuClean$state_name <- tolower(wuClean$state_name)
  #get areas, join to wuClean
  areas <- data.frame(gArea(statePoly, byid = TRUE), stringsAsFactors = FALSE)
  areas$state_name <- rownames(areas)
  areas["district of columbia",][2] <- "dist. of columbia"
  wuAreas <- left_join(wuClean, areas, by = 'state_name')
  names(wuAreas)[6] <- "area"
  
  #compute ratios
  wuAreas <- mutate(wuAreas, wuPerArea = value/area)
  finalScaleFactors <- data.frame()
  #per category
  for(cat in coreCats){
    thisCat <- filter(wuAreas, category == cat)
    maxRatio <- max(thisCat$wuPerArea, na.rm = TRUE)
    thisCat <- mutate(thisCat, scale = wuPerArea/maxRatio)
    thisCat <- mutate(thisCat, newArea = area*scale)
    thisCat <- mutate(thisCat, scaleFactor = newArea/area)
    maxScaleFactor <- range(thisCat$scaleFactor, na.rm = TRUE, finite = TRUE)[2]
    thisCat <- mutate(thisCat, scaleFactor = scaleFactor/maxScaleFactor)
    
    finalScaleFactors <- bind_rows(finalScaleFactors, thisCat)
  }
  
  #save
  saveRDS(finalScaleFactors, file = viz[['location']])
}