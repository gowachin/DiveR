#' dist2time
#' 
#' @param dist Distance vector in meters from points to points.
#' @param speed A diver speed value in meter/minute. 
#' 
dist2time <- function(dist, speed){ # TODO : need to find default value
  time = c(0, cumsum(dist / speed))
}


#' summary
#'
#' summary of a dive object
#'
#' @param object is a DiveR object. There are methods for 
#' \code{\link[DiveR]{dive}}, \code{\link[DiveR]{tank}} and
#'  \code{\link[DiveR]{conso}} objects.
#' @param ... other arguments not used
#'
#' @return A brief summary of a dive and its main parameters. 
#'
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#'
#' @export
summary.dive <- function(object, ...){ 
  # parameters depth and time
  cat(paste("Dive for :",dtime(object), "minutes at",depth(object),"meters\n"))
  # dtr
  dtr <- object$parames["dtr"]
  cat('Total dive time is',dtime(object)+dtr,'with a dive ascent of',dtr,'minutes\n')
  # desat & maj
  summary(object$desat)
}


#' @rdname summary.dive
#'  
#' @examples 
#' 
#' 
#' @export
summary.desat <- function(object, ...) {
  sup <- object$desat_stop$time > 0
  n <- sum(sup)
  diz <- sum( object$desat_stop$time[sup] > 9)
  sp <- c(rep('  ',n-diz), rep(' ', diz))
  if (n > 0) {
    t <- paste(sprintf('%s%d minutes at %d meters', sp, object$desat_stop$time[sup], 
                       object$desat_stop$depth[sup]), collapse = '\n          ')
  } else {
    t <- "no stop for deco.\n  Please consider a minimal safety 
                 stop of 3 minute at 3 meter"
  }
  
  cat(n, "stops :", t,'\n')
  cat('The dive group is',object$group, '\n')
}


#' dtime
#'
#' \code{dtime} retrieve the depth time of a singular or multiple dive sequence.
#' WORK ONLY ON SQUARE DIVE
#'
#' @param object is a DiveR object. There are methods for 
#' \code{\link[DiveR]{dive}} objects.
#'
#' @return a single numeric value
#' 
#' @examples 
#' # Simple dive
#' dtime(dive(20,40))
#' # Multiple dives
#' dtime(ndive(dive(20,40), dive(15, 80), inter = 540))
#'
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#'
#' @export
dtime <- function(object) {
  UseMethod("dtime")
}

#' @rdname dtime
#' 
#' @export
dtime.dive <- function(object) {
  t <- object$dtcurve$times
  dt <- min(t[t > 0])
  return(dt)
}

#' @rdname dtime
#' 
#' @export
dtime.ndive <- function(object) {
  d1 <- dtime(object$dive1)
  d2 <- dtime(object$dive2)
  return(c(d1,d2))
}

#' minute_to_time
#'
#' Transform minute variable into a character string in hour format.
#'
#' @param time positive numeric value in minute
#' @param sec add the sec part of the string. 
#' @param sep ':' by default, choice between ':' and 'h'. Only affect only the
#' first separator character
#' @param day TRUE by default, allow to set time in a day period (betwwen 00h00 
#' and 23:59)
#' 
#' @return character string
#' 
#' @examples 
#' minute_to_time(130.5, sec = TRUE)
#' minute_to_time(130.5, sec = FALSE)
#' minute_to_time(130.5, sec = TRUE, sep = 'h')
#' minute_to_time(130.5, sec = FALSE, sep = 'h')
#' minute_to_time(1440, sec = FALSE, sep = 'h')
#' minute_to_time(1664, sec = FALSE, sep = 'h')
#'
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#'
#' @export
minute_to_time <- function(time, sec = TRUE, sep = c(':', 'h'), day = TRUE){
  sep <- match.arg(sep)
  
  while(any(time >= 1440) & day){
    time[time >= 1440 ] <- time[time >= 1440] - 1440
  }
  
  if(sec){
    res = sprintf("%02.0f%s%02.0f:%02.0f", time %/% 60, sep, time %% 60, 
                  (time %% 60 %% 1) * 60 )
  } else {
    res = sprintf("%02.0f%s%02.0f", time %/% 60, sep, time %% 60)
  }
  return(res)
}


#' depth at time
#'
#' Find the depth for a given time and dive.
#'
#' @param dive \code{\link[DiveR]{dive}} object.
#' @param time positive numeric value in minute 
#' 
#' @examples 
#' depth_at_time(dive(20,40), 38)
#' depth_at_time(dive(20,40), 41)
#'
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#'
#' @export
depth_at_time <- function(dive, time){
  time <- time #+ dive$hour[1]
  times <- dive$dtcurve$times #+ dive$hour[1]
  depths <- dive$dtcurve$depths
  if(time > max(times) | time <= 0){return(0)}
  
  befd <-  tail(depths[times < time], 1) ; beft <-  tail(times[times < time], 1)
  aftd <- depths[times >= time][1] ; aftt <- times[times >= time][1]
  if(befd==aftd){
    return(befd)
  } else {
    reg <- lm(c(befd,aftd)~ c(beft,aftt))
    res <- reg$coefficients[2] * time + reg$coefficients[1]
  }
  return(res)
}


#' time at depth
#'
#' Find the time for a given depth and dive.
#'
#' @param dive \code{\link[DiveR]{dive}} object.
#' @param depth positive numeric value in meter 
#' 
#' @examples 
#' time_at_depth(dive(20,40), 3)
#' time_at_depth(dive(20,40), 10)
#' time_at_depth(dive(20,40), 20)
#'
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#'
#' @export
time_at_depth <- function(dive, depth){
  times <- dive$dtcurve$times #+ dive$hour[1]
  depths <- dive$dtcurve$depths
  res <- c()
  
  if(depth > max(depths) | depth < 0){
    warning('Depth must be set between 0 and the maximum depth of the dive.')
    return(res)
  }
  # TODO : refacto this code with vector !
  for(i in 2:length(times)){
    if(depth %in% depths[i-1]:depths[i] & depths[i-1] != depths[i]){
      if(times[i-1] == times[i]){
        res <- c(res,times[i])
        next
      }
      reg <- lm(c(depths[i-1], depths[i])~ c(times[i-1], times[i]))
      tmp <- unname( ( depth - reg$coefficients[1] ) / reg$coefficients[2])
      res <- c(res, tmp )
    }
  }

  return(res)
}


#'simpl
#'
#' Remove points of curve when isodepth around
#'
#' @param dtcurve a curve dive list
#' 
#' @examples 
#' cons <- conso(dive = dive(20,40), tank(12, 200), cons = 20)
#' cons$dtcurve
#' simpl(cons$dtcurve)
#'
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#'
#' @export
simpl <- function(dtcurve){
  to_rm <- c()
  for(i in c(2: (length(dtcurve$depths)-1))){
    if (dtcurve$depths[i] == dtcurve$depths[i-1] & dtcurve$depths[i] == dtcurve$depths[i+1]){
      to_rm <- append(to_rm, i)
    }
  }
  if(is.null(to_rm)) return(dtcurve)
  if(class(dtcurve) == "data.frame"){
    dtcurve <- dtcurve[-to_rm,]
  } else {
    dtcurve$depths <- dtcurve$depths[-to_rm]
    dtcurve$times <- dtcurve$times[-to_rm]
  }
  return(dtcurve)
}