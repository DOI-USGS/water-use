
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
  alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 5)
  alaska <- elide(alaska, shift=c(-1800000, -2000000))
  row.names(alaska) <- 'alaska'
  proj4string(alaska) <- proj4string(conus)
  
  hawaii <- elide(HI, rotate=-35)
  hawaii <- elide(hawaii, shift=c(5200000, -1100000))
  row.names(hawaii) <- 'hawaii'
  proj4string(hawaii) <- proj4string(conus)
  
  # resize: c('district of columbia', 'maryland', 'delaware')
  
  conus <- shift_state(conus, 'district of columbia', 8, c(21,-1))
  conus <- shift_state(conus, 'maryland', 1.4, c(33,2.8))
  conus <- shift_state(conus, 'delaware', 1.4, c(39,8))
  conus <- shift_state(conus, 'new jersey', 1.6, c(45,15))
  conus <- shift_state(conus, 'connecticut', 1.4, c(6.5,14))
  conus <- shift_state(conus, 'massachusetts', 1.45, c(35,15))
  conus <- shift_state(conus, 'rhode island', 1.4, c(35,9))
  conus <- shift_state(conus, 'maine', 0.7, c(10,0))
  conus <- shift_state(conus, 'new hampshire', 0.8, c(8,9.5))
  conus <- shift_state(conus, 'vermont', 0.8, c(6,9))
  conus <- shift_state(conus, 'new york', 1.2, c(-1,6))
  
  centroid.nudge <- list(
    'florida'=c(3,13),
    'california'=c(-10,18),
    'michigan'=c(12,-12),
    'maryland'=c(0,7),
    'louisiana'=c(0,-2))
  
  states.out <- rbind(conus, alaska, hawaii, makeUniqueIDs = TRUE)
  
  # shifting states to be located at (0,0) in svg-space:
  shifted.states <- zero_shift_sp(states.out, centroid.nudge)
  shifted.centroids <- zero_shift_sp(shifted.states, centroid.nudge)
  state.centroids <- state_centroids(states.out, centroid.nudge) # keeps plot order
  
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
  obj <- elide(obj, shift=shift*10000+c(orig.cent-new.cent))
  proj4string(obj) <- proj4string(sp)
  return(rbind(obj.out, obj))
}

state_centroids <- function(sp, shifts){
  state.centroids <- NULL
  ordered.names <- names(sp)[sp@plotOrder]
  for (name in ordered.names){
    obj <- calc_centroid(sp[names(sp) == name,], shifts)
    if (name == ordered.names[1]){
      state.centroids <- obj
    } else {
      state.centroids <- rbind(state.centroids, obj)
    }
  }
  return(state.centroids)
}


calc_centroid <- function(sp, shifts){
  obj <- rgeos::gCentroid(sp, byid=TRUE)
  name <- names(sp)
  if (!is.null(shifts[[name]])){
    obj@coords <- obj@coords + shifts[[name]]*10000
  }
  return(obj)
}

zero_shift_sp <- function(sp, shifts){
  sp.shifted <- NULL
  for (name in names(sp)){
    obj <- sp[names(sp) == name,]
    centroid <- c(unlist(calc_centroid(obj,  shifts)@coords))
    zero.coord <- c(bbox(sp)[1,1], bbox(sp)[2,2]) # min x, but max y
    if (name == names(sp)[1]){
      sp.shifted <- elide(obj, shift=zero.coord-centroid)
    } else {
      sp.shifted <- rbind(sp.shifted, elide(obj, shift=zero.coord-centroid))
    }
  }
  return(sp.shifted)
}