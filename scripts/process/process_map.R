library(rgdal)
library(rgeos)
#' take map arguments and return a projected sp object
#' 
#' @param \dots arguments passed to \code{\link[maps]{map}} excluding \code{fill} and \code{plot}
#' 
proj.string <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
to_sp <- function(...){
  library(maptools)
  library(maps)
  map <- maps::map(..., fill=TRUE, plot = FALSE)
  IDs <- sapply(strsplit(map$names, ":"), function(x) x[1])
  map.sp <- map2SpatialPolygons(map, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
  map.sp.t <- spTransform(map.sp, CRS(proj.string))
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
  alaska <- elide(alaska, shift=c(-1900000, -1750000))
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
    'louisiana'=c(0,-2),
    'hawaii'=c(8.5,-8)
    )
  
  states.out <- rbind(conus, alaska, hawaii, makeUniqueIDs = TRUE)
  
  # shifting states to be located at (0,0) in svg-space:
  shifted.states <- zero_shift_sp(states.out, centroid.nudge)
  shifted.centroids <- zero_shift_sp(shifted.states, centroid.nudge)
  state.centroids <- state_centroids(states.out, centroid.nudge) # keeps plot order
  x.0 <- bbox(conus)[1,1]
  y.0 <- bbox(conus)[2,2]
  legend.circles <- rbind(create_legend(x.0, y.0,'nodata'),
                          create_legend(x.0, y.0,'categoryFill')
  )
  
  out <- list(states = states.out, 
              shifted.states = shifted.states, 
              shifted.centroids = shifted.centroids,
              state.centroids = state.centroids, 
              bbox = bbox(states.out),
              legend.circles=legend.circles) 
  
  saveRDS(out, file = viz[['location']])
}


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

create_legend <- function(x, y, row.name, w = 94000, n = 5){
  xy <- cbind(x +c(-w/2, -w/2, w/2, w/2, -w/2), y + c(w/2, -w/2, -w/2, w/2, w/2))
  sp <- SpatialPolygons(list(Polygons(list(Polygon(xy)), row.name)), proj4string = CRS(proj.string))
  return(sp)
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

process.watermark <- function(viz){
  usgs.d="m234.95 15.44v85.037c0 17.938-10.132 36.871-40.691 36.871-27.569 0-40.859-14.281-40.859-36.871v-85.04h25.08v83.377c0 14.783 6.311 20.593 15.447 20.593 10.959 0 15.943-7.307 15.943-20.593v-83.377h25.08m40.79 121.91c-31.058 0-36.871-18.27-35.542-39.03h25.078c0 11.462 0.5 21.092 14.282 21.092 8.472 0 12.62-5.482 12.62-13.618 0-21.592-50.486-22.922-50.486-58.631 0-18.769 8.968-33.715 39.525-33.715 24.42 0 36.543 10.963 34.883 36.043h-24.419c0-8.974-1.492-18.106-11.627-18.106-8.136 0-12.953 4.486-12.953 12.787 0 22.757 50.493 20.763 50.493 58.465 0 31.06-22.75 34.72-41.85 34.72m168.6 0c-31.06 0-36.871-18.27-35.539-39.03h25.075c0 11.462 0.502 21.092 14.285 21.092 8.475 0 12.625-5.482 12.625-13.618 0-21.592-50.494-22.922-50.494-58.631 0-18.769 8.969-33.715 39.531-33.715 24.412 0 36.536 10.963 34.875 36.043h-24.412c0-8.974-1.494-18.106-11.625-18.106-8.144 0-12.955 4.486-12.955 12.787 0 22.757 50.486 20.763 50.486 58.465 0 31.06-22.75 34.72-41.85 34.72m-79.89-46.684h14.76v26.461l-1.229 0.454c-3.816 1.332-8.301 2.327-12.453 2.327-14.287 0-17.943-6.645-17.943-44.177 0-23.256 0-44.348 15.615-44.348 12.146 0 14.711 8.198 14.933 18.107h24.981c0.198-23.271-14.789-36.043-38.42-36.043-41.021 0-42.52 30.724-42.52 60.954 0 45.507 4.938 63.167 47.12 63.167 9.784 0 25.36-2.211 32.554-4.18 0.436-0.115 1.212-0.596 1.212-1.216v-59.598h-38.612v18.09"
  wave.d="m48.736 55.595l0.419 0.403c11.752 9.844 24.431 8.886 34.092 2.464 6.088-4.049 33.633-22.367 49.202-32.718v-10.344h-116.03v27.309c7.071-1.224 18.47-0.022 32.316 12.886m43.651 45.425l-13.705-13.142c-1.926-1.753-3.571-3.04-3.927-3.313-11.204-7.867-21.646-5.476-26.149-3.802-1.362 0.544-2.665 1.287-3.586 1.869l-28.602 19.13v34.666h116.03v-24.95c-2.55 1.62-18.27 10.12-40.063-10.46m-44.677-42.322c-0.619-0.578-1.304-1.194-1.915-1.698-13.702-10.6-26.646-5.409-29.376-4.116v11.931l6.714-4.523s10.346-7.674 26.446 0.195l-1.869-1.789m16.028 15.409c-0.603-0.534-1.214-1.083-1.823-1.664-12.157-10.285-23.908-7.67-28.781-5.864-1.382 0.554-2.7 1.303-3.629 1.887l-13.086 8.754v12.288l21.888-14.748s10.228-7.589 26.166 0.054l-0.735-0.707m68.722 12.865c-4.563 3.078-9.203 6.203-11.048 7.441-4.128 2.765-13.678 9.614-29.577 2.015l1.869 1.797c0.699 0.63 1.554 1.362 2.481 2.077 11.418 8.53 23.62 7.303 32.769 1.243 1.267-0.838 2.424-1.609 3.507-2.334v-12.234m0-24.61c-10.02 6.738-23.546 15.833-26.085 17.536-4.127 2.765-13.82 9.708-29.379 2.273l1.804 1.729c0.205 0.19 0.409 0.375 0.612 0.571l-0.01 0.01 0.01-0.01c12.079 10.22 25.379 8.657 34.501 2.563 5.146-3.436 12.461-8.38 18.548-12.507l-0.01-12.165m0-24.481c-14.452 9.682-38.162 25.568-41.031 27.493-4.162 2.789-13.974 9.836-29.335 2.5l1.864 1.796c1.111 1.004 2.605 2.259 4.192 3.295 10.632 6.792 21.759 5.591 30.817-0.455 6.512-4.351 22.528-14.998 33.493-22.285v-12.344"
  watermark.paths <- c('usgs'=usgs.d, 'wave'=wave.d)
  saveRDS(watermark.paths, file = viz[['location']])
}