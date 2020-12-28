#' @import graphics
#' @import shiny
#' @import utils
NULL

#' mn90
#'
#' A package to code some basic function about dive profiles.
#'
#' @docType package
#' @name mn90
NULL

#' mn90 app
#' 
#' custom function to run the shiny app (thinkR idea, to explore in docker)
#' 
#' @examples 
#' # mn90::shiny_mn90_app()
#' 
#' @export
shiny_mn90_app <- function(){
  appDir <- system.file("app", package = "mn90")
  shiny::runApp(appDir, display.mode = "normal")
}

#' check_val
#' 
#' custom function for checking if values are single positive numeric.
#' 
#' @param val a single positive numeric value
#' @param zero set to \code{FALSE} by default, to include zero value to the test
#' 
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @export
check_val <- function(val, zero = FALSE) {
  if (zero){
    if (val <= 0 | !is.numeric(val)) {
      stop(paste(
        deparse(substitute(val)),
        "must be a single positive numeric value."
      ))
    }
    
  } else {
    if (val < 0 | !is.numeric(val)) {
      stop(paste(
        deparse(substitute(val)),
        "must be a single positive numeric value."
      ))
    }
  }
  
}

#' dive
#' 
#' @param depth in meter
#' @param time in minute
#' @param secu true by default, secu deco stage 3 min at 3 meter
#' @param vup 10 m/min by default
#' @param maj 0 by default
#' @param hour NULL not implemented yet
#' 
#' @details 
#' See \code{\link[mn90]{tablecheck}} for limit values of depth and time.
#' 
#' @examples 
#' dive = dive(depth = 39, time = 22, secu = TRUE, vup = 10)
#' 
#' @return dive, a dive class object.
#' 
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @export
dive <- function(depth = 20, time = 40, secu = TRUE,
                 vup = 10, maj = 0, hour = NULL,
                 dist = NULL,  speed = NULL, way = c('AS','AR')) {
  # depth = 39; time = 22; secu = TRUE; vup = 10
  # checks
  check_val(vup)
  check_val(maj)
  
  if (length(depth) > 1){
    if (is.null(speed)){stop('A speed must be provided')}
    
    if (is.null(dist) & length(time) == length(depth)){
      dist <- time * speed
    } else {
      stop(paste('multiple points dive need a same lenght numeric vector of',
                 'time or distances'))
    }
    
    if (length(dist) != length(depth)){
      stop('depth and dist must be of same length')
    }
    
    vdepth <- depth
    depth <- max(vdepth)
    check_val(depth)
    
    if (way == 'AR'){
      vdepth <- c(vdepth, rev(vdepth)[-1])
      dist <- c(dist, rev(dist))
    } 
    time <- sum(dist)/speed
    
  } else {
    vdepth <- depth
    check_val(time)
  }

  if (maj > 0) {
    timaj <- time + maj
  } else {
    timaj <- time
  }

  # check for values
  tablecheck(depth, timaj)
  # get the palier from the table
  palier <- palier(depth, timaj, secu)
  # dive dtcurve
  if (length(vdepth) > 1){
    # stop('not yet implemented')
    dtcurve <- dtcurve(depth = vdepth, time = time, dist = dist, palier = palier, vup = vup, 
                       speed = speed, way = way)
  } else {
    dtcurve <- dtcurve(time = time, depth = depth, palier = palier, vup = vup)
  }
  # compute the dtr from palier and depth in square profile
  # dtr <- dtr(palier, depth = depth, vup = vup)
  dtr <- unname(unlist(dtcurve[3]))
  dtcurve[3] <- NULL
  
  # hour 
  if (is.null(hour)) {
    hour <- c(0, tail(dtcurve$time,1))
  } else {
    hour <- c(hour, hour + tail(dtcurve$time,1))
  }

  dive <- list(
    dtcurve = dtcurve, dtr = dtr, palier = palier,
    maj = maj, hour = hour
  )
  class(dive) <- "dive"
  return(dive)
}
#' ndive
#' 
#' @param dive1 the first dive, obtained by the dive function
#' @param dive2 the first dive, obtained by the dive function. This one will be
#' modified with a majoration obtained from dive1 and the interval.
#' @param inter 16 by default, interval between dives in minutes
#' 
#' @details 
#' See \code{\link[mn90]{tablecheck}} for limit values of depth and time 
#' of a dive.
#' 
#' @examples 
#' dive1 = dive(depth = 39, time = 22, secu = TRUE, vup = 10)
#' dive2 = dive(depth = 20, time = 40, secu = TRUE, vup = 10)
#' divet = ndive(dive1, dive2, inter = 30)
#' 
#' @return ndive, a ndive class object.
#' 
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @export
ndive <- function(dive1, dive2, inter = 16) {
  # checks
  if (class(dive1) != "dive") stop("dive1 must be of class dive")
  if (class(dive2) != "dive") stop("dive2 must be of class dive")
  # retrive some data avout dive2
  time2 <- dtime(dive2)
  depth2 <- depth(dive2)

  if (inter <= 15) {
    # consecutiv dives 
    warning("A minimum of 15 minutes is requiered between dives to consider them
            as different dives.")
    # total time of dive
    time <- dtime(dive1) + dive1$dtr + inter + time2
    # total depth
    depth <- max(depth(dive1), depth2)
    if (max_depth_t(depth) > time) { # check if second dive possible with time
      ndive <- list(
        dive1 = dive1,
        dive2 = dive(
          depth = depth, time = time,
          hour = dive1$hour[1]
        ),
        inter = inter, type = "consec"
      )
      # modification of dive2 curve in times for graphics !
      ndive$dive2$dtcurve$times[-c(1,2)] <- ndive$dive2$dtcurve$time[-c(1,2)] - 
        (dtime(dive1) + dive1$dtr + inter)
      ndive$dive2$hour[1] <- (dtime(dive1) + dive1$dtr + inter)
    } else {
      # second dive is impossible here in the table
      warning("Cumulated time of both dives and interval is larger than table.")
      ndive <- list(dive1 = dive1, dive2 = "STOP", inter = inter, type = "solo")
    }
  } else {
    # successiv dives
    if (inter > 720){ # 12h interv is not longuer
      maj <- 0
    } else {
      # compute maj
      maj <- majoration(
        depth = depth2, inter = inter,
        group = dive1$palier$group
      )
    }
    
    # check if second dive possible (time in talbe)
    if (tablecheck(depth2, time2 + maj, force = TRUE) &
      max_depth_t(depth2) > time2 + maj & depth(dive1) <= 60) {
      hour2 <- dive1$hour[2] + inter
      suc_dive <- dive(depth = depth2, time = time2, maj = maj, hour = hour2)
      
      ndive <- list(
        dive1 = dive1, dive2 = suc_dive,
        inter = inter, type = "success"
      )
      
      if (inter > 720){
        ndive$type <- "diff"
        }
    } else {
      # second dive is impossible here in the table
      ndive <- list(dive1 = dive1, dive2 = "STOP", inter = inter, type = "solo")
    }
  }

  class(ndive) <- "ndive"
  return(ndive)
}