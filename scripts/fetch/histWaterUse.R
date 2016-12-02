fetch.histWaterUse <- function(viz){
  library(readxl)
  
  filePath <- tempdir()
  
  fileName<-"SIU_USStateWaterUse1950_80_01Dec2016.xlsx"
  
  download.file("ftp://ftpext.usgs.gov/pub/er/wi/middleton/dblodgett/SIU_USStateWaterUse1950_80_01Dec2016.xlsx", file.path(filePath,fileName), mode='wb')
  
  histExDat <- list(list(sheet = "1950", skip = 11),
                    list(sheet = "1955", skip = 15),
                    list(sheet = "1960", skip = 11),
                    list(sheet = "1965", skip = 11),
                    list(sheet = "1970", skip = 11),
                    list(sheet = "1975", skip = 15),
                    list(sheet = "1980", skip = 11))
  histDat <- list()
  
  for(sheet in 1:length(histExDat)) {
    sheet.data <- list(read_excel(file.path(filePath,fileName), sheet = histExDat[[sheet]]["sheet"][[1]], skip = histExDat[[sheet]]["skip"][[1]]))
    histDat <- append(histDat, sheet.data)
  }
  
  saveRDS(histDat, viz[["location"]])
  
}