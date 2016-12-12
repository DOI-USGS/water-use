process.wuClean <- function(viz){
  library(tidyr)
  library(dplyr)
  
  rawWaterUse <- readData(viz[['depends']][['waterUse']])
  
  longWU <- gather(rawWaterUse, category, value, -state_cd, -state_name, -year)

  uglyCats <- data.frame(c( "Public Supply" = "Public.Supply.total.self.supplied.withdrawals..fresh..in.Mgal.d",
                 "Irrigation" = "Irrigation..Total.total.self.supplied.withdrawals..fresh..in.Mgal.d",

                 "Industrial Com Surf" = "Commercial.self.supplied.surface.water.withdrawals..fresh..in.Mgal.d",
                 "Industrial Com Ground" = "Commercial.self.supplied.groundwater.withdrawals..fresh..in.Mgal.d",

                 "Industrial Industrial Surf" = "Industrial.self.supplied.surface.water.withdrawals..fresh..in.Mgal.d",
                 "Industrial Industrial Ground" = "Industrial.self.supplied.groundwater.withdrawals..fresh..in.Mgal.d",

                 "Industrial Mining Surf" = "Mining.self.supplied.surface.water.withdrawals..fresh..in.Mgal.d",
                 "Industrial Mining Ground" = "Mining.self.supplied.groundwater.withdrawals..fresh..in.Mgal.d",

                 "Thermoelectric" = "Total.Thermoelectric.Power.total.self.supplied.withdrawals..fresh..in.Mgal.d",
                 "Thermoelectric Fossil" = "Fossil.fuel.Thermoelectric.Power.total.self.supplied.withdrawals..fresh..in.Mgal.d",
                 "Thermoelectric Geothermal" = "Geothermal.Thermoelectric.Power.total.self.supplied.withdrawals..fresh..in.Mgal.d",
                 "Thermoelectric Nuclear" = "Nuclear.Thermoelectric.Power.total.self.supplied.withdrawals..fresh..in.Mgal.d"), stringsAsFactors = FALSE) %>%
    mutate(shortName = rownames(.)) 
  
  names(uglyCats) <- c("longName","shortName")
  
  longWU <- longWU %>%
    mutate(year = as.numeric(year)) %>%
    filter(category %in% uglyCats$longName) %>%
    mutate(value = replace(value, value=='-', NA)) %>%
    mutate(value = as.numeric(value)) %>%
    left_join(uglyCats, by=c("category"="longName")) %>%
    mutate(category = shortName) %>%
    select(-shortName)
  
  wideWU <- spread(longWU, category, value) 
  
  wideWU$Industrial <- rowSums(wideWU[,uglyCats$shortName[3:8]], na.rm = TRUE)
  
  wideWU$`Thermoelectric`[is.na(wideWU$`Thermoelectric`)] <- rowSums(data.frame(wideWU$`Thermoelectric Fossil`[is.na(wideWU$`Thermoelectric`)] ,
                                                                                wideWU$`Thermoelectric Geothermal`[is.na(wideWU$`Thermoelectric`)] ,
                                                                                wideWU$`Thermoelectric Nuclear`[is.na(wideWU$`Thermoelectric`)] ),
                                                                     na.rm = TRUE)
  
  wideWU <- wideWU[,c("state_cd","state_name","year",
                      "Industrial","Irrigation", "Public Supply","Thermoelectric")]
  
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