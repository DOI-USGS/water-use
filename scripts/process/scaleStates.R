#scale states according to water use
# two different sized states with equal water use should have the
# same area
library(rgeos)
library(dplyr)

process.scaleStates <- function(viz){

  #read sp, wuClean
  depends <- vizlab::readDepends(viz)
  statePoly <- depends[['state-map']]$states
  wuClean <- depends[["calc-histWaterData"]]
  vals <- expand.grid(state_name = unique(wuClean$state_name),
                      year = c(unique(wuClean$year)),
                      category = unique(wuClean$category))
  wuClean <- merge(vals, wuClean, all=TRUE) %>% 
    arrange(state_cd, state_name, desc(category))
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

process.categoryValues <- function(viz){
  data.in <- readDepends(viz)
  legend.circles <- data.in[['state-map']]$`legend.circles`
  legend.area <- unname(gArea(legend.circles, byid = TRUE)[1])
  scale.factors <- data.in[["calc-scaleFactors"]]
  catvalues <- list()
  for(cat in unique(scale.factors$category)){
    thisCat <- filter(scale.factors, category == cat)
    magicState <- thisCat[which.max(thisCat$wuPerArea), ]
    catvalues[[cat]] = round(legend.area * magicState$value / magicState$area)
  }
  names(catvalues)[names(catvalues) == 'Public Supply'] <- "Public_Supply"
  saveRDS(catvalues, file = viz[['location']])
}

#convert the rds from above to appropriately-formatted json
library(jsonlite)

process.scaleFactors2json <- function(viz){
  scaleFactors <- readData(viz[['depends']]$scaleFactors)
  scaleFactorsNational <- readData(viz[['depends']]$scaleFactorsNational)
  catVals <- readData(viz[['depends']]$catScaleValues)
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

  dat <- scaleFactorsNational
  uniqYears <- unique(dat$year)
  natJson <- vector("list", length(uniqYears))
  
  roundUp <- function(x) ceiling(max(x)/10)*10
  
  for(i in 1:length(uniqYears)){
    thisYear <- filter(dat, year == uniqYears[i])

    Thermoelectric = data.frame(filter(thisYear, category == "Thermoelectric")) %>%
      mutate(value = roundUp(value))
    Industrial = data.frame(filter(thisYear, category == "Industrial"))%>%
      mutate(value = roundUp(value))
    Public_Supply = data.frame(filter(thisYear, category == "Public Supply"))%>%
      mutate(value = roundUp(value))
    Irrigation = data.frame(filter(thisYear, category == "Irrigation"))%>%
      mutate(value = roundUp(value))
    Total = data.frame(filter(thisYear, category == "Total"))%>%
      mutate(value = roundUp(value))
    
    #convert to list of dfs for each category
    natJson[[i]] <- list(Thermoelectric = Thermoelectric,
                         Industrial = Industrial,
                         Public_Supply = Public_Supply,
                         Irrigation = Irrigation,
                         Total = Total)
  }
  names(natJson) <- uniqYears

  allJson <- list(totState = forJson, totNat = natJson, catVals=catVals)

  jsOut <- toJSON(allJson)
  write(jsOut, viz[['location']])

}
