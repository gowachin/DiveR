#' @import graphics
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
#' custom function to run the shiny app
#' 
#' @examples 
#' mn90::shiny_mn90_app()
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
#' @export
check_val <- function(val) {
  if (val < 0 | !is.numeric(val)) {
    stop(paste(
      deparse(substitute(val)),
      "must be a single positive numeric value."
    ))
  }
}

#' dive
#' 
#' @param depth in meter
#' @param time in minute
#' @param secu true by default, secu deco 3 min at 3 meter
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
                 vup = 10, maj = 0, hour = NULL) {
  # depth = 39; time = 22; secu = TRUE; vup = 10
  # checks
  check_val(depth)
  check_val(time)
  check_val(vup)
  check_val(maj)

  if (maj > 0) {
    timaj <- time + maj
  } else {
    timaj <- time
  }

  # check for values
  tablecheck(depth, timaj)
  # get the palier from the table
  palier <- palier(depth, timaj, secu)
  # compute the dtr from palier and depth in square profile
  dtr <- dtr(palier, depth = depth, vup = vup)
  # dive dtcurve
  dtcurve <- dtcurve(time = time, depth = depth, palier = palier, vup = vup)[-3]
  # hour 
  if (!is.null(hour)) {
    hour <- c(hour, hour + time + dtr)
  } else {
    hour <- c(0, time + dtr)
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
#' 
#' @return ndive, a ndive class object.
#' 
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @export
ndive <- function(dive1, dive2, inter = 16) {
  # depth = 39; time = 22; secu = TRUE; vup = 10
  # checks
  check_val(inter)
  if (class(dive1) != "dive") stop("dive1 must be of class dive")
  if (class(dive2) != "dive") stop("dive2 must be of class dive")
  # retrive some data avout dive2
  time2 <- dtime(dive2)
  depth2 <- depth(dive2)

  if (inter <= 15) {
    warning("A minimum of 15 minutes is requiered between dives to consider them
            as different dives.")

    time <- dtime(dive1) + dive1$dtr + inter + time2
    depth <- max(depth(dive1), depth2)
    if (max_depth_t(depth) > time) {
      ndive <- list(
        dive1 = dive1,
        dive2 = dive(
          depth = depth, time = time,
          hour = dive1$hour[1]
        ),
        inter = inter, type = "consec"
      )
      
      ndive$dive2$dtcurve$times[-c(1,2)] <- ndive$dive2$dtcurve$time[-c(1,2)] - 
        (dtime(dive1) + dive1$dtr + inter)
      ndive$dive2$hour[1] <- (dtime(dive1) + dive1$dtr + inter)
    } else {
      # second dive is impossible here in the table
      warning("Cumulated time of both dives and interval is larger than table.")
      ndive <- list(dive1 = dive1, dive2 = "STOP", inter = inter, type = "solo")
    }
  } else {
    # compute maj
    maj <- majoration(
      depth = depth2, inter = inter,
      group = dive1$palier$group
    )
    
    # check if second dive possible (time in talbe)
    if (tablecheck(depth2, time2 + maj, force = TRUE) &
      max_depth_t(depth2) > time2 + maj & depth(dive1) <= 60) {
      hour2 <- dive1$hour[2] + inter
      suc_dive <- dive(depth = depth2, time = time2, maj = maj, hour = hour2)

      ndive <- list(
        dive1 = dive1, dive2 = suc_dive,
        inter = inter, type = "success"
      )
    } else {
      # second dive is impossible here in the table
      ndive <- list(dive1 = dive1, dive2 = "STOP", inter = inter, type = "solo")
    }
  }

  class(ndive) <- "ndive"
  return(ndive)
}