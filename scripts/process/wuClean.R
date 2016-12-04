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
  
  alaska.industrial.fresh.1985 <- 114
  alaska.irrigation.fresh.1985 <- 0
  alaska.publicSupply.fresh.1985 <- 76
  alaska.thermoelectric.fresh.1985 <- 30
  wideWU <- rbind(wideWU, list(2, "Alaska", 1985,   
                            alaska.industrial.fresh.1985,
                            alaska.irrigation.fresh.1985,
                            alaska.publicSupply.fresh.1985,
                            alaska.thermoelectric.fresh.1985))
  
  arizona.publicSupply.fresh.1985 <- 618
  arizona.irrigation.fresh.1985 <- 5520
  arizona.industrial.fresh.1985 <- 45 # self supplied withdrawals only to avoid double counting.
  arizona.thermoelectric.fresh.1985 <- 25 # self supplied withdrawals only to avoid double counting.
  wideWU[wideWU[,"state_name"]=="Arizona" & wideWU[,"year"]==1985,] <- list(4, "Arizona", 1985, 
                                                                         arizona.industrial.fresh.1985,
                                                                         arizona.irrigation.fresh.1985,
                                                                         arizona.publicSupply.fresh.1985,
                                                                         arizona.thermoelectric.fresh.1985)
  
  washington.industrial.fresh.1985 <- 519
  wideWU[wideWU[,"state_name"]=="Washington" & wideWU[,"year"]==1985,]$Industrial <- washington.industrial.fresh.1985
  
  arizona.publicSupply.fresh.1990 <- 707
  arizona.irrigation.fresh.1990 <- 5300
  arizona.industrial.fresh.1990 <- 163 # self supplied withdrawals only to avoid double counting.
  arizona.thermoelectric.fresh.1990 <- 61 # self supplied withdrawals only to avoid double counting.
  wideWU[wideWU[,"state_name"]=="Arizona" & wideWU[,"year"]==1990,] <- list(4, "Arizona", 1990, 
                                                                         arizona.industrial.fresh.1990,
                                                                         arizona.irrigation.fresh.1990,
                                                                         arizona.publicSupply.fresh.1990,
                                                                         arizona.thermoelectric.fresh.1990)
  
  washington.industrial.fresh.1990 <- 536 # self supplied withdrawals only to avoid double counting.
  wideWU[wideWU[,"state_name"]=="Washington" & wideWU[,"year"]==1990,]$Industrial <- washington.industrial.fresh.1990
  
  washington.industrial.fresh.1995 <- 611 # self supplied withdrawals only to avoid double counting.
  wideWU[wideWU[,"state_name"]=="Washington" & wideWU[,"year"]==1995,]$Industrial <- washington.industrial.fresh.1995
  
  ohio.industrial.fresh.2000 <- 807  # self supplied withdrawals only to avoid double counting.
  ohio.thermoelectric.fresh.2000 <- 8590
  wideWU[wideWU[,"state_name"]=="Ohio" & wideWU[,"year"]==2000,]$Industrial <- ohio.industrial.fresh.2000
  wideWU[wideWU[,"state_name"]=="Ohio" & wideWU[,"year"]==2000,]$Thermoelectric <- ohio.thermoelectric.fresh.2000
  
  
  massachusetts.industrial.fresh.2005 <- 112 # self supplied withdrawals only to avoid double counting.
  wideWU[wideWU[,"state_name"]=="Massachusetts" & wideWU[,"year"]==2005,]$Industrial <-  massachusetts.industrial.fresh.2005
  
  wideWU[["Total"]] <- rowSums(data.frame(wideWU$Industrial, 
                                          wideWU$Irrigation, 
                                          wideWU$`Public Supply`,
                                          wideWU$Thermoelectric))
  
  longWU <- gather(wideWU, category, value, -state_cd, -state_name, -year)
  
  saveRDS(longWU, file=viz[["location"]])
}