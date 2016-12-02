library(tidyr)
library(svglite)
nationalData<-readRDS("cache/nationalClean.rds")
dataList <- list(totData = spread(nationalData$nationalData, category, value)[,c(1,2,5,8,10,11,16)])
names(dataList$totData) <- c("year", "population", "irrigation", "public", "industrial", "thermoelectric", "total")
dataList["pCapData"][[1]] <- dataList$totData
dataList$pCapData[c(3,4,5,6,7)] <- as.data.frame(Map("/", dataList$pCapData[c(3,4,5,6,7)], dataList$pCapData[2])) # bgal/d to tgal/day
dataList$pCapData[c(3,4,5,6,7)] <- as.data.frame(Map("*", dataList$pCapData[c(3,4,5,6,7)], 365)) # tgal/d to tgal/y
maxList <- list(totData = max(dataList$totData[c(3,4,5,6,7)]),
                pCapData = max(dataList$pCapData[c(3,4,5,6,7)]))
minList <- list(totData = min(dataList$totData[c(3,4,5,6,7)]),
                pCapData = min(dataList$pCapData[c(3,4,5,6,7)]))

rangeList <- list(totData = c(0, 500),
                 pCapData = c(0, 800))

for(plotVer in c("totData", "pCapData")) {
  plotData <- as.data.frame(array(dim = c((2010-1949), length(names(dataList[plotVer][[1]])))))
  names(plotData) <- names(dataList[plotVer][[1]])
  plotData$year <- 1950:2010
  for(row in 1:nrow(dataList[plotVer][[1]])) {
    plotData[which(plotData$year == dataList[plotVer][[1]]$year[row]),] <- dataList[plotVer][[1]][row,]
  }
  for(plotVar in c(2:length(names(plotData)))) {
    plotDataVar<-plotData[c(1,plotVar)]
    names(plotDataVar) <- c("x", "y")
    midpoints <- barplot(plotDataVar$y, names = plotDataVar$x, las=2, axes = FALSE, xaxt='n', ann=FALSE)
  }
}