

to_sp <- function(...){
  library(maptools)
  library(maps)
  map <- maps::map(..., fill=TRUE, plot = FALSE)
  IDs <- sapply(strsplit(map$names, ":"), function(x) x[1])
  map.sp <- map2SpatialPolygons(map, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
  map.sp.t <- spTransform(map.sp, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
  return(map.sp.t)
}

process.map_states <- function(){
  library(sp)
  conus <- to_sp('state')
  HI <- to_sp("world", "USA:hawaii")
  AK <- to_sp("world", "USA:alaska")
  
  # thanks to Bob Rudis (hrbrmstr):
  # https://github.com/hrbrmstr/rd3albers
  
  alaska <- elide(AK, rotate=-50)
  alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
  alaska <- elide(alaska, shift=c(-2100000, -2500000))
  proj4string(alaska) <- proj4string(conus)
  
  hawaii <- elide(HI, rotate=-35)
  hawaii <- elide(hawaii, shift=c(5400000, -1400000))
  proj4string(hawaii) <- proj4string(conus)
 
  rbind(conus, alaska, hawaii, makeUniqueIDs = TRUE)
}