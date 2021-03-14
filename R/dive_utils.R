#' dist2time
#' 
#' @param dist Distance vector in meters from points to points.
#' @param speed A diver speed value in meter/minute. 
#' 
#' @export
dist2time <- function(dist, speed){ # TODO : need to find default value
  time = c(0, cumsum(dist / speed))
}


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
init_dtcurve <- function(depth, time, ascend_speed, way = c("OW", "WB")) {
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
  } else if (ascent_speed < 10 | ascent_speed > 15) {
    warning(paste(
      "Ascent speed is usually set between 10 and 20 m/min in",
      "most desaturation models.",
      "6m/min is used between 6m and the surface"
    ))
  }

  way <- match.arg(way)

  if (length(depth) == length(time)) {
    stop("depth and dist must be of same length")
  }

  if (length(depth) == 1) {
    # square profile
    if (depth > 6) { # ascent speed above 6m is 6m/min
      ascent_time <- ((depth - 6) / ascend_speed) + 1
    } else {
      ascent_time <- depth / 6
    }
    dtcurve <- data.frame(
      depth = c(0, depth, depth, 0),
      time = c(0, 0, time, time + ascent_time)
    )
  } else {
    # way of the dive
    if (way == "WB") {
      depth <- c(depth, rev(depth)[-1])
      time <- c(time, rev(time)[-1])
    }
    # make df
    dtcurve <- data.frame(depth = depth, time = time)
    # check if origin point
    if (sum(dtcurve[1, ]) > 0) {
      dtcurve <- rbind(c(0, 0), dtcurve)
    }
    # check for end point
    if (tail(dtcurve$depth, 1) < 0) {
      tmp_d <- tail(dtcurve$depth, 1)
      if (tmp_d > 6) { # ascent speed above 6m is 6m/min
        ascent_time <- ((tmp_d - 6) / ascend_speed) + 1
      } else {
        ascent_time <- tmp_d / 6
      }
      dtcurve <- rbind(dtcurve, c(0, tail(dtcurve$time, 1) + ascent_time))
    }
  }
  return(dtcurve)
}
