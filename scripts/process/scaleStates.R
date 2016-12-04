#scale states according to water use
# two different sized states with equal water use should have the
# same area
library(rgeos)
library(dplyr)

process.scaleStates <- function(viz){

  #read sp, wuClean
  statePoly <- readData(viz[['depends']]$stateMap)$states
  wuClean <- readData(viz[['depends']]$wuClean)
  vals <- expand.grid(state_name = unique(wuClean$state_name),
                      year = unique(wuClean$year),
                      category = unique(wuClean$category))
  wuClean <- merge(vals, wuClean, all=TRUE) %>% 
    arrange(state_cd, state_name, category)
  wuClean$state_name <- tolower(wuClean$state_name)
  #get areas, join to wuClean
  areas <- data.frame(gArea(statePoly, byid = TRUE), stringsAsFactors = FALSE)
  areas$state_name <- rownames(areas)
  wuAreas <- left_join(wuClean, areas, by = 'state_name')
  names(wuAreas)[6] <- "area"

  #compute ratios
  wuAreas <- mutate(wuAreas, wuPerArea = value/area)
  finalScaleFactors <- data.frame()
  #per category
  for(cat in unique(wuAreas$category)){
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
  scaleFactorsNational <- readData(viz[['depends']]$scaleFactorsNational)

  scaleFactors <- select(scaleFactors, -area, -wuPerArea, -newArea)

  #replace spaces with underscore and remove &
  scaleFactors <- mutate(scaleFactors, state_name = gsub(" ", "_", scaleFactors$state_name))
  scaleFactors <- mutate(scaleFactors, state_name = gsub("&_", "", scaleFactors$state_name))


  uniqYears <- unique(scaleFactors$year)
  uniqYears <- uniqYears[order(uniqYears)]
  forJson <- vector("list", length(uniqYears)) #initialize

  for(i in 1:length(uniqYears)){
    thisYear <- filter(scaleFactors, year == uniqYears[i])

    #convert to list of dfs for each category
    forJson[[i]] <- list(Thermoelectric=filter(thisYear, category == "Thermoelectric"),
                          Industrial = filter(thisYear, category == "Industrial"),
                          Public_Supply = filter(thisYear, category == "Public Supply"),
                          Irrigation = filter(thisYear, category == "Irrigation"),
                          Total = filter(thisYear, category == "Total")
                         )
  }
  names(forJson) <- uniqYears

  natScaleTypes <- list()

  for(type in names(scaleFactorsNational)) {
    dat <- scaleFactorsNational[type][[1]]
    uniqYears <- unique(dat$Year)
    natJson <- vector("list", length(uniqYears))
    for(i in 1:length(uniqYears)){
      thisYear <- filter(dat, Year == uniqYears[i])

      #convert to list of dfs for each category
      natJson[[i]] <- list(Thermoelectric = filter(thisYear, category == "Thermoelectric"),
                           Industrial = filter(thisYear, category == "Industrial"),
                           Public_Supply = filter(thisYear, category == "Public Supply"),
                           Irrigation = filter(thisYear, category == "Irrigation"),
                           Total = filter(thisYear, category == "Total"))
    }
    names(natJson) <- uniqYears
    natScaleTypes[type][[1]] <- natJson
  }

  allJson <- list(totState = forJson, pCapNat = natScaleTypes$pCapData, totNat = natScaleTypes$totData)

  jsOut <- toJSON(allJson)
  write(jsOut, viz[['location']])

}
