#' init_dtcurve
#' 
#' expand depth and time to a dtcurve data.frame 
#' 
#' @param depth Depth of the dive in meter. Need to be positive values.
#' A single value will result in square dive.
#' @param time Duration of the dive in minute. Need to be positive values.
#' A single value will result in square dive.
#' @param ascent_speed Ascent_speed in meter/minute. 10 m/min by default. 
#' Most dive table advice to limite this speed to 20M/min maximum.
#' @param way If the dive is one way (one way : 'OW') or if the diver return by 
#' the same depth (way back : 'WB'). 
#'
#' @export
init_dtcurve <- function(depth, time, ascent_speed = 10, way = c("OW", "WB")) {
  #### IDIOT PROOF ####
  if (any(depth < 0) | !is.numeric(depth)) {
    stop("depth must be positive numeric value(s).")
  }
  if (any(time < 0) | !is.numeric(time)) {
    stop("time must be positive numeric value(s).")
  }
  if (any(time != sort(time))) {
    stop("time values need to be sorted, you don't own a subaquatic dolorean")
  }
  if (any(ascent_speed <= 0) | !is.numeric(ascent_speed) |
    length(ascent_speed) > 1) {
    stop("ascent_speed must be a single positive numeric value(s).")
  }

  way <- match.arg(way)

  if (length(depth) != length(time)) {
    stop("depth and time must be of same length")
  }

  if (length(depth) == 1) {
    # square profile
    if (depth > 6) { # ascent speed above 6m is 6m/min
      ascent_time <- ((depth - 6) / ascent_speed) + 1
    } else {
      ascent_time <- depth / 6
    }
    dtcurve <- data.frame(
      depth = c(0, depth, depth, 0),
      time = c(0, 0, time, time + ascent_time)
    )
  } else {
    # check if origin point
    if(time[1] > 0 | depth[1] > 0){
      time <- c(0, time)
      depth <- c(0, depth)
    }
    # way of the dive
    if (way == "WB") {
      depth <- c(depth, rev(depth)[-1])
      time <- c(time, cumsum(c(tail(time, 1), rev(diff(time))))[-1] )
    }
    # make df
    dtcurve <- data.frame(depth = depth, time = time)
    # 
    # if (sum(dtcurve[1, ]) > 0) {
    #   dtcurve <- rbind(c(0, 0), dtcurve)
    # }
    # check for end point
    if (tail(dtcurve$depth, 1) > 0) {
      tmp_d <- tail(dtcurve$depth, 1)
      if (tmp_d > 6) { # ascent speed above 6m is 6m/min
        ascent_time <- ((tmp_d - 6) / ascent_speed) + 1
      } else {
        ascent_time <- tmp_d / 6
      }
      dtcurve <- rbind(dtcurve, c(0, tail(dtcurve$time, 1) + ascent_time))
    }
  }
  return(dtcurve)
}


#' add_desat
#' 
#' Add the desaturation stops to the dive curve. It also check if there is no
#' depth point higher and before the stops, to prevent accidents.
#' 
#' @param dtcurve a depth time curve in a data.frame with 2 columns depth and 
#' time. Depths values are meters (positive values) and time is in minute.
#' @param desat a desat object that contain the desaturation stops of given dive
#' @param ascent_speed Ascent_speed in meter/minute. 10 m/min by default. 
#' Most dive table advice to limite this speed to 20M/min maximum.
#' @param secu security decompression stop of 3 min at 3 m. FALSE by default.
#'
#' @export
add_desat <- function(dtcurve, desat, ascent_speed = 10, secu = FALSE) {
  #### IDIOT PROOF ####
  if (!inherits(dtcurve, 'data.frame') | any(is.na(dtcurve)) | 
      any(colnames(dtcurve) != c('depth', 'time'))){
    stop(paste('dtcurve must be a data.frame with 2 columns named',
               'depth and time without any NA value'))
  }
  if (any(dtcurve$depth < 0) | !is.numeric(dtcurve$depth)) {
    stop("depth must be positive numeric value(s).")
  }
  if (any(dtcurve$time < 0) | !is.numeric(dtcurve$time)) {
    stop("time must be positive numeric value(s).")
  }
  if (any(dtcurve$time != sort(dtcurve$time))) {
    stop("time values need to be sorted, you don't own a subaquatic dolorean")
  }
  
  if(!is.desat(desat)){
    stop("desat must be of class desat made with desat_model function")
  }
  
  if (any(ascent_speed <= 0) | !is.numeric(ascent_speed) |
      length(ascent_speed) > 1) {
    stop("ascent_speed must be a single positive numeric value(s).")
  }
  if( !is.logical(secu) | is.na(secu) )
    stop('secu must be TRUE or FALSE')
  
  if(is.null(desat$hour)){
    print(1)
  } else {
    print(2)
  }
  
  return(dtcurve)
}
