#' @import graphics
#' @import utils
NULL

#' DiveR
#'
#' A package to code some basic function about dive profiles.
#'
#' @docType package
#' @name DiveR
NULL

#' dive
#' 
#' @param depth Depth of the dive in meter. Need to be positive values.
#' A single value is needed for square dive, however if a vector is provided
#' a decompression model need to be selected. Default is 20 meter
#' @param time Duration of the dive in minute. Need to be positive values.
#' A single value is needed for square dive, however if a vector is provided
#' a decompression model need to be selected. Default is 40 minutes
#' @param secu security decompression stop of 3 min at 3 m. FALSE by default.
#' @param ascent_speed Ascent_speed in meter/minute. 10 m/min by default. 
#' Most dive table advice to limite this speed to 20M/min maximum.
#' @param maj Time majoration for the dive. 
#' Only used by table decompression model.
#' @param desat_model Which desaturation model is used to compute desaturation
#' stops during ascent, to eliminate nitrogen. Default is tables as only
#' this one works today.
#' @param hour NULL not implemented yet
#' @param dist a distance vector
#' @param speed speed of the diver
#' @param way If the dive is one way (one way : 'OW') or if the diver return by 
#' the same depth (way back : 'WB'). 
#' 
#' 
#' @details 
#' See \code{\link[DiveR]{tablecheck}} for limit values of depth and time.
#' 
#' @examples 
#' dive = dive(depth = 39, time = 22, secu = TRUE, ascent_speed = 10)
#' 
#' @return dive, a dive class object.
#' 
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @export
dive <- function(depth = 20, time = 40, secu = TRUE,
                 ascent_speed = 10, maj = 0, 
                 desat_model = c('table'),
                 
                 hour = NULL,
                 dist = NULL,  speed = NULL, way = c('OW','WB')
                 ) {
  #### IDIOT PROOF ####
  if (any(depth < 0) | !is.numeric(depth) ) {
    stop("depth must be positive numeric value(s).")
  }
  if (any(time < 0) | !is.numeric(time) ) {
    stop("time must be positive numeric value(s).")
  }
  if( !is.logical(secu) | is.na(secu) ){
    stop('secu must be TRUE or FALSE')
  }
  if (any(ascent_speed <= 0) | !is.numeric(ascent_speed) | 
      length(ascent_speed) > 1 ) {
    stop("ascent_speed must be a single positive numeric value(s).")
  }
  if( any(maj != 0)){
    if (any(maj < 0) | !is.numeric(maj) | length(maj) > 1 ) {
      stop("maj must be a single positive numeric value.")
    }
  }
  desat_model <- match.arg(desat_model)
  way <- match.arg(way)
  
  if (ascent_speed < 10 | ascent_speed > 15) {
    warning(paste( 
      "Ascent speed is usually set between 10 and 20 m/min in",
      "most desaturation models.",
      "\n6m/min is used between 6m and the surface"
    ))
  }
  # draw raw dtcurve
  raw_dtcurve <- init_dtcurve(depth, time, ascent_speed, way)
  if(desat_model == "table"){
    # time maj and tablecheck is done in dest_table
    desat_stop <- desat_table(raw_dtcurve, maj)
  } else {
    message("Not yet implemented")
    desat_stop <- list(depth = 0, time = 0, group = 'Z', hour = NULL)
    class(desat) <- "desat"
  }
  desat_dtcurve <- add_desat(raw_dtcurve, desat_stop, ascent_speed, secu)
  dtr <- max(desat_dtcurve$time) - tail(raw_dtcurve$time, 2)[1]
  
  dtcurve <- desat_dtcurve # TODO : to remove
  colnames(dtcurve) <- c("depths", "times")
  
  #### Will be removed .......................................................##
  if (maj > 0) {
    timaj <- time + maj
  } else {
    timaj <- time
  }
  # check for values
  # tablecheck(depth, timaj)
  # get the palier from the table
  palier <- palier(depth, timaj, secu)
  #### .......................................................................##
  
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
#' @param verbose allow cat return in consol for debug purposes
#' 
#' @details 
#' See \code{\link[DiveR]{tablecheck}} for limit values of depth and time 
#' of a dive.
#' 
#' @examples 
#' dive1 = dive(depth = 39, time = 22, secu = TRUE, ascent_speed = 10)
#' dive2 = dive(depth = 20, time = 40, secu = TRUE, ascent_speed = 10)
#' divet = ndive(dive1, dive2, inter = 30)
#' 
#' @return ndive, a ndive class object.
#' 
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @export
ndive <- function(dive1, dive2, inter = 16, verbose = FALSE) {
  # checks
  if (class(dive1) != "dive") stop("dive1 must be of class dive")
  if (class(dive2) != "dive") stop("dive2 must be of class dive")
  # retrive some data avout dive2
  time2 <- dtime(dive2)
  depth2 <- depth(dive2)
  secu2 <- secu(dive2)
  speed2 <- speed(dive2)$asc

  if (inter <= 15) {
    # consecutiv dives 
    warning("A minimum of 15 minutes is requiered between dives to consider them
            as different dives.")
    # total time of dive
    time <- dtime(dive1) + dive1$dtr + inter + time2
    # total depth
    depth <- max(depth(dive1), depth2)
    if (max_depth_time(depth) >= time) { # check if second dive possible with time
      ndive <- list(
        dive1 = dive1,
        dive2 = dive(
          depth = depth, time = time, ascent_speed = speed2, secu = secu2,
          hour = dive1$hour[1]
        ),
        inter = inter, type = "consec"
      )
      if(verbose) cat('consec\n')
      # modification of dive2 curve in times for graphics !
      ndive$dive2$dtcurve$times[-c(1,2)] <- ndive$dive2$dtcurve$time[-c(1,2)] - 
        (dtime(dive1) + dive1$dtr + inter)
      ndive$dive2$hour[1] <- (dive1$hour[1] + dtime(dive1) + dive1$dtr + inter)
    } else {
      if(verbose) cat('no_consec\n')
      # second dive is impossible here in the table
      warning("Cumulated time of both dives and interval is larger than table.")
      ndive <- list(dive1 = dive1, dive2 = "STOP", inter = inter, type = "solo")
    }
  } else {
    # successiv dives
    if (inter > 720 ){ # 12h interv is not longuer
      maj <- 0
    } else if( depth2 > 60 | dive1$palier$group == 'Z'){ # Z is for 60+ dive1
      warning(paste0( "Second dive impossible in less than 12h ",
                      "after a dive a 60 more meters" ))
        ndive <- list(dive1 = dive1, dive2 = "STOP", 
                      inter = inter, type = "solo")
        class(ndive) <- "ndive"
        if(verbose) cat('60_no_success\n')
        return(ndive)
      } else {
      # compute maj
      maj <- majoration(
        depth = depth2, inter = inter,
        group = dive1$palier$group
      )
    }
    
    # check if second dive possible (time in talbe)
    if (tablecheck(depth2, time2 + maj, force = TRUE) &
      max_depth_time(depth2) >= time2 + maj ){ # & depth(dive1) <= 60) {
      hour2 <- dive1$hour[2] + inter

      suc_dive <- dive(depth = depth2, time = time2, maj = maj, secu = secu2,
                       ascent_speed = speed2, hour = hour2)
      
      ndive <- list(
        dive1 = dive1, dive2 = suc_dive,
        inter = inter, type = "success"
      )
      
      if (inter > 720){
        if(verbose) cat('diff\n')
        ndive$type <- "diff"
      } else {if(verbose) cat('success\n')}
    } else {
      if(verbose) cat('maj_no_success\n')
      warning(paste0( "Second dive impossible due to majoration of time"))
      # second dive is impossible here in the table
      ndive <- list(dive1 = dive1, dive2 = "STOP", inter = inter, type = "solo")
    }
  }

  class(ndive) <- "ndive"
  return(ndive)
}