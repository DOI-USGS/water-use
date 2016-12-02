
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
  
  # -- if moving any more states, do it here: --
  alaska <- elide(AK, rotate=-50)
  alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
  alaska <- elide(alaska, shift=c(-2100000, -2500000))
  row.names(alaska) <- 'alaska'
  proj4string(alaska) <- proj4string(conus)
  
  hawaii <- elide(HI, rotate=-35)
  hawaii <- elide(hawaii, shift=c(5400000, -1400000))
  row.names(hawaii) <- 'hawaii'
  proj4string(hawaii) <- proj4string(conus)
  
  # resize: c('district of columbia', 'maryland', 'deleware')
  
  conus <- shift_state(conus, 'district of columbia', 3, c(300000,100000))
  
  states.out <- rbind(conus, alaska, hawaii, makeUniqueIDs = TRUE)
  
  # shifting states to be located at (0,0) in svg-space:
  shifted.states <- zero_shift_sp(states.out)
  shifted.centroids <- zero_shift_sp(shifted.states)
  state.centroids <- state_centroids(states.out) # keeps plot order
  
  out <- list(states = states.out, 
              shifted.states = shifted.states, 
              shifted.centroids = shifted.centroids,
              state.centroids = state.centroids, 
              bbox = bbox(states.out)) # then spatial points for centroids, and other things
  
  saveRDS(out, file = viz[['location']])
}

library(rgeos)
scale_transform <- function(obj, scale){
  centroid <- c(unlist(rgeos::gCentroid(obj,byid=TRUE)@coords))
  obj <- elide(obj, scale=max(apply(bbox(obj), 1, diff)) * scale) # do by area?
  elide(obj, shift=centroid - c(unlist(rgeos::gCentroid(obj,byid=TRUE)@coords)))
}

shift_state <- function(sp, state, scale, shift){
  obj <- sp[names(sp) %in% state,]
  orig.cent <- rgeos::gCentroid(obj,byid=TRUE)@coords
  obj.out <- sp[!names(sp) %in% state,]
  obj <- elide(obj, scale=max(apply(bbox(obj), 1, diff)) * scale)
  new.cent <- rgeos::gCentroid(obj,byid=TRUE)@coords
  obj <- elide(obj, shift=shift+c(orig.cent-new.cent))
  proj4string(obj) <- proj4string(sp)
  return(rbind(obj.out, obj))
}

state_centroids <- function(sp){
  state.centroids <- NULL
  ordered.names <- names(sp)[sp@plotOrder]
  for (name in ordered.names){
    obj <- rgeos::gCentroid(sp[names(sp) == name,],byid=TRUE)
    if (name == ordered.names[1]){
      state.centroids <- obj
    } else {
      state.centroids <- rbind(state.centroids, obj)
    }
  }
  return(state.centroids)
}
zero_shift_sp <- function(sp){
  sp.shifted <- NULL
  for (name in names(sp)){
    obj <- sp[names(sp) == name,]
    centroid <- c(unlist(rgeos::gCentroid(obj,byid=TRUE)@coords))
    zero.coord <- c(bbox(sp)[1,1], bbox(sp)[2,2]) # min x, but max y
    if (name == names(sp)[1]){
      sp.shifted <- elide(obj, shift=zero.coord-centroid)
    } else {
      sp.shifted <- rbind(sp.shifted, elide(obj, shift=zero.coord-centroid))
    }
  }
  return(sp.shifted)
}