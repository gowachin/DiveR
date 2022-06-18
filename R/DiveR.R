#' @import graphics
#' @import utils
#' @useDynLib DiveR, .registration=TRUE
#' @importFrom Rcpp sourceCpp
#' @import checkmate
NULL

#' DiveR
#'
#' A package to simulate scuba-diving depth curve and desaturation models.
#'
#' @docType package
#' @name DiveR
NULL

#' dive
#' 
#' Create a dive class object simulating a dive for given depth and times.
#' To do so, it create a depth/time curve and find the desaturation stops
#' according to a desaturation model.
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
#' @param altitude Heigth in meter from sea level, it will impact desaturation
#' process and ascen_speed.. Default is sea level (0m).
#' @param desat_model Which desaturation model is used to compute desaturation
#' stops during ascent, to eliminate nitrogen. Default is tables as only
#' this one works today.
#' @param hour The first immersion hour in minute. Need to be 24h format 
#' converted in minutes (hour = hours * 60 + minutes). 0 by default.
#' @param way If the dive is one way (one way : 'OW') or if the diver return by 
#' the same depth (way back : 'WB'). 
#' @param gas The gas used by the diver, will be used to modify saturation
#' computation. Default is 'AIR' but nitrox is possible and can be written as
#' 'NXy' where y is the O2 percentage. Example 'NX32' is a 32% nitrogen tank.
#' Can go up to 'NX100' and be aware that this will limit the depth due to 
#' oxygen toxicity wuth a partial pressure of 1.6 bar. 
#' 
#' @details 
#' See \code{\link[DiveR]{tablecheck}} for limit values of depth and time.
#' 
#' @examples 
#' dive = dive(depth = 39, time = 22, secu = TRUE, ascent_speed = 10)
#' 
#' @return dive, a list containing a depth/time curve in a data.frame,
#' the desaturation stops acording to the model used, and some information
#' about depth to surface time, and input parameters as ascent_speed and secu.
#' 
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @import checkmate
#' @export
dive <- function(depth = 20, time = 40, secu = TRUE,
                 ascent_speed = 10, maj = 0, 
                 altitude = 0,
                 desat_model = c('table', 'other'),
                 hour = 0, way = c('OW','WB'), 
                 gas = 'AIR'
                 ) {
  #### IDIOT PROOF ####
  assertNumeric(depth, lower = 0)
  assertNumeric(time, lower = 0)
  assertLogical(secu, any.missing = FALSE)
  assertNumber(ascent_speed, lower = 1e-6)
  
  assertNumber(maj, lower = 0)
  assertNumber(altitude, lower = 0)
  
  desat_model <- match.arg(desat_model)
  
  assertNumber(hour, lower = -1e-6)
  
  way <- match.arg(way)

  assertCharacter(gas)
  gas <- as.gas(gas)
  
  if (ascent_speed > 120){
    stop("This is not the sport to do if you want to go to the moon",
            call. = interactive())
  }
  if (ascent_speed < 10 | ascent_speed > 15) {
    warning(paste( 
      "Ascent speed is usually set between 10 and 20 m/min in",
      "most desaturation models.",
      "\n6m/min is used between 6m and the surface"
    ),call. = interactive())
  }
  
  # draw raw dtcurve
  raw_dtcurve <- init_dtcurve(depth, time, ascent_speed, way)
  
  
  if(desat_model == "table"){
    # time maj and tablecheck is done in desat_table
    desat_stop <- desat_table(dtcurve = raw_dtcurve, maj = maj, 
                              altitude = altitude, ppo2 = gas$ppo2)
  } else if(desat_model == "haldane"){
    message("Not yet fully implemented")
    desat_stop <- desat_haldane(dtcurve = raw_dtcurve, maj = maj,
                                ppn2 = gas$ppn2, ncomp = 12)
  } else {
    message("Not yet implemented")
    desat_stop <- list(desat_stop = data.frame(depth = 0, time = 0, hour = NA,
                                               row.names = "m0"),
                       group = 'Z', model = "other")
    class(desat_stop) <- "desat"
  }
  desat_dtcurve <- add_desat(raw_dtcurve, desat_stop, ascent_speed, secu)
  dtr <- max(desat_dtcurve$time) - tail(raw_dtcurve$time, 2)[1]
  
  # adding secu stop to desat_stop object.
  # TODO : same code as in dive_utils, consider function !
  if(secu){
    depths <- desat_stop$desat_stop$depth
    if(3 %in% depths){
      desat_stop$desat_stop$time[depths==3] <- desat_stop$desat_stop$time[depths==3] + 3
    } else {
      desat_stop$desat_stop <- rbind(desat_stop$desat_stop, 
                                     m3 = data.frame(depth = 3, time = 3, 
                                                     hour = NA))
    }
  }
  
  # retrieve hour of desat
  for ( i in 1:nrow(desat_stop$desat_stop)){
    if (desat_stop$desat_stop$time[i] > 0){
      times <- desat_dtcurve$time[desat_dtcurve$depth == 
                                     desat_stop$desat_stop$depth[i]]
      if(round(desat_stop$desat_stop$time[i]) == round(diff(tail(times, 2)))){
        desat_stop$desat_stop$hour[i] <- tail(times, 2)[1]
      }
    }
  } 
  
  dtcurve <- desat_dtcurve # TODO : to remove
  colnames(dtcurve) <- c("depths", "times")
  
  # hour 
  hour <- c(hour, hour + tail(dtcurve$time,1))
  
  # other_info
  params <- c(maj = maj, altitude = altitude, secu = secu, 
              ascent_speed = ascent_speed, dtr = dtr, ppo2 = gas$ppo2)
  

  dive <- list(
    dtcurve = dtcurve, desat = desat_stop,
    hour = hour, params = params
  )
  class(dive) <- "dive"
  return(dive)
}


#' ndive
#' 
#' Combine 2 dives object in a sequence. To do so, it checks if the desaturation
#' models are coherent and if the second dive is possible according to 
#' residual azote and interval time.
#' 
#' @param dive1 the first dive, must be a \code{\link[DiveR]{dive}} object
#' @param dive2 the second dive, must be a \code{\link[DiveR]{dive}} object. 
#' This one will be modified with a majoration obtained from dive1 and 
#' the interval.
#' @param inter 16 by default, interval in minute between the end of the first 
#' dive and the beginning of the second.
#' @param verbose allow cat return in consol for debug purposes. Show which
#' case of sequence is used.
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
#' @import checkmate
#' @export
ndive <- function(dive1, dive2, inter = 16, verbose = FALSE) {
  #### IDIOT PROOF ####
  if (!is.dive(dive1)) stop("dive1 must be a dive object",call. = interactive())
  if (!is.dive(dive2)) stop("dive2 must be a dive object",call. = interactive())

  assertNumber(inter, lower = 0)
  assertLogical(verbose, any.missing = FALSE)
  
  desat_model <- dive2$desat$model
  
  if (desat_model == "table"){
    ndive <- table_ndive(dive1, dive2, inter = inter, verbose = verbose)
  } else if (desat_model != "table") {
    stop("There is no other model yet",call. = interactive())
  }
  
  return(ndive)
}
