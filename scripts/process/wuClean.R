process.wuClean <- function(viz){
  library(tidyr)
  library(dplyr)
  
  rawWaterUse <- readData(viz[['depends']][['waterUse']])
  
  longWU <- gather(rawWaterUse, category, value, -state_cd, -state_name, -year)

  uglyCats <- c( "Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d",
                 "Irrigation..Total.total.self.supplied.withdrawals..fresh..in.Mgal.d",
                 "Industrial.total.self.supplied.withdrawals..in.Mgal.d",
                 "Total.Thermoelectric.Power.total.self.supplied.withdrawals..fresh..in.Mgal.d",
                 "Fossil.fuel.Thermoelectric.Power.total.self.supplied.withdrawals..fresh..in.Mgal.d",
                 "Geothermal.Thermoelectric.Power.total.self.supplied.withdrawals..fresh..in.Mgal.d",
                 "Nuclear.Thermoelectric.Power.total.self.supplied.withdrawals..fresh..in.Mgal.d")
                 # "Thermoelectric.Power..Once.through.cooling..total.self.supplied.withdrawals..fresh..in.Mgal.d",
                 # "Thermoelectric.Power..Closed.loop.cooling..total.self.supplied.withdrawals..fresh..in.Mgal.d" )
  
  longWU <- longWU %>%
    mutate(year = as.numeric(year)) %>%
    filter(category %in% uglyCats) %>%
    mutate(value = replace(value, value=='-', NA)) %>%
    mutate(value = as.numeric(value))
  
  longWU$category[longWU$category == uglyCats[1]] <- "Public Supply"
  longWU$category[longWU$category == uglyCats[2]] <- "Irrigation"
  longWU$category[longWU$category == uglyCats[3]] <- "Industrial"
  longWU$category[longWU$category == uglyCats[4]] <- "Thermoelectric"
  longWU$category[longWU$category == uglyCats[5]] <- "Thermoelectric Fossil"
  longWU$category[longWU$category == uglyCats[6]] <- "Thermoelectric Geothermal"
  longWU$category[longWU$category == uglyCats[7]] <- "Thermoelectric Nuclear"
  
  wideWU <- spread(longWU, category, value) 
  
  wideWU$`Thermoelectric`[is.na(wideWU$`Thermoelectric`)] <- rowSums(data.frame(wideWU$`Thermoelectric Fossil`[is.na(wideWU$`Thermoelectric`)] ,
                                                                             wideWU$`Thermoelectric Geothermal`[is.na(wideWU$`Thermoelectric`)] ,
                                                                             wideWU$`Thermoelectric Nuclear`[is.na(wideWU$`Thermoelectric`)] ),
                                                                             na.rm = TRUE)
  wideWU <- select(wideWU, -`Thermoelectric Fossil`, -`Thermoelectric Geothermal`, -`Thermoelectric Nuclear`)
  
  wideWU[["Total"]] <- rowSums(data.frame(wideWU$Industrial, 
                                          wideWU$Irrigation, 
                                          wideWU$`Public Supply`,
                                          wideWU$Thermoelectric), 
                               na.rm = TRUE)
  
  longWU <- gather(wideWU, category, value, -state_cd, -state_name, -year)
  
  saveRDS(longWU, file=viz[["location"]])
}