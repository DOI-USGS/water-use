
#' take map arguments and return a projected sp object
#' 
#' @param \dots arguments passed to \code{\link[maps]{map}} excluding \code{fill} and \code{plot}
to_sp <- function(...){
  library(maptools)
  library(maps)
  map <- maps::map(..., fill=TRUE, plot = FALSE)
  IDs <- sapply(strsplit(map$names, ":"), function(x) x[1])
  map.sp <- map2SpatialPolygons(map, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
  map.sp.t <- spTransform(map.sp, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
  return(map.sp.t)
}

#' create the sp object 
#'
#'@param viz the vizlab object 
process.state_map <- function(viz){
  library(sp)
  conus <- to_sp('state')
  HI <- to_sp("world", "USA:hawaii")
  AK <- to_sp("world", "USA:alaska")
  
  # thanks to Bob Rudis (hrbrmstr):
  # https://github.com/hrbrmstr/rd3albers
  
  alaska <- elide(AK, rotate=-50)
  alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
  alaska <- elide(alaska, shift=c(-2100000, -2500000))
  row.names(alaska) <- 'alaska'
  proj4string(alaska) <- proj4string(conus)
  
  hawaii <- elide(HI, rotate=-35)
  hawaii <- elide(hawaii, shift=c(5400000, -1400000))
  row.names(hawaii) <- 'hawaii'
  proj4string(hawaii) <- proj4string(conus)
  
  out <- rbind(conus, alaska, hawaii, makeUniqueIDs = TRUE)
  sp::plot(out, col='grey97',border='white', lwd=2)
  #sp::plot(out[names(out) == 'california',], add=TRUE, col='darkgreen', border=NA)
  cols <- brewer.pal(9,'Greens')
  names.st <- names(out)#[!names(out) %in% 'california']
  for (name in names.st){
    scale.n <- runif(1, min = 0.1, max=1)
    col.i <- round(scale.n*9)
    sp::plot(scale_transform(out[names(out) == name,], scale.n), add=TRUE, col=cols[col.i], border=NA)
  }
  
  saveRDS(out, file = viz[['location']])
}

library(rgeos)
scale_transform <- function(obj, scale){
  centroid <- c(unlist(rgeos::gCentroid(obj,byid=TRUE)@coords))
  obj <- elide(obj, scale=max(apply(bbox(obj), 1, diff)) * scale) # do by area?
  elide(obj, shift=centroid - c(unlist(rgeos::gCentroid(obj,byid=TRUE)@coords)))
}