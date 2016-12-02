#scale states according to water use
# two different sized states with equal water use should have the 
# same area
library(rgeos)
library(dplyr)

process.scaleStates <- function(viz){
  #could put these in yaml too?
  coreCats <- c("Thermoelectric", "Irrigation", "Public Supply", "Industrial")
  
  #read sp, wuClean
  statePoly <- readData(viz[['depends']]$stateMap)$states
  wuClean <- readData(viz[['depends']]$wuClean)
  
  wuClean$state_name <- tolower(wuClean$state_name)
  #get areas, join to wuClean
  areas <- data.frame(gArea(statePoly, byid = TRUE), stringsAsFactors = FALSE)
  areas$state_name <- rownames(areas)
  #need to change DC so that it matches state_names in the water use df
  areas["district of columbia",][2] <- "dist. of columbia"
  wuAreas <- left_join(wuClean, areas, by = 'state_name')
  names(wuAreas)[6] <- "area"
  
  #compute ratios
  wuAreas <- mutate(wuAreas, wuPerArea = value/area)
  finalScaleFactors <- data.frame()
  #per category
  for(cat in coreCats){
    thisCat <- filter(wuAreas, category == cat)
    
    magicState <- thisCat[which.max(thisCat$wuPerArea), ]
    thisCat <- mutate(thisCat, newArea = value * magicState$area / magicState$value)
    thisCat <- mutate(thisCat, scaleFactor = newArea / area)
    
    finalScaleFactors <- bind_rows(finalScaleFactors, thisCat)
  }
  
  #save
  saveRDS(finalScaleFactors, file = viz[['location']])
}


#convert the rds from above to appropriately-formatted json
library(jsonlite)

process.scaleFactors2json <- function(viz){
  scaleFactors <- readData(viz[['depends']]$scaleFactors)
  
  scaleFactors <- select(scaleFactors, -area, -wuPerArea, -scale, -newArea)
  
  #replace spaces with underscore
  scaleFactors <- mutate(scaleFactors, state_name = gsub(" ", "_", scaleFactors$state_name))
  
  
  uniqYears <- unique(scaleFactors$year)
  forJson <- vector("list", length(uniqYears)) #initialize
  
  for(i in 1:length(uniqYears)){
    thisYear <- filter(scaleFactors, year == uniqYears[i])
    
    #convert to list of dfs for each category
    forJson[[i]] <- list(Thermoelectric=filter(thisYear, category == "Thermoelectric"),
                    Industrial = filter(thisYear, category == "Industrial"),
                    Public_Supply = filter(thisYear, category == "Public Supply"),
                    Irrigation = filter(thisYear, category == "Irrigation"))
  }
  names(forJson) <- uniqYears
  
  #to JSON
  jsOut <- toJSON(forJson)
  write(jsOut, viz[['location']])
   
}