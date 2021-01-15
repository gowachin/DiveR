#' dtr
#'
#' \code{dtr} compute the ascent time from a depth and between deco stages 
#' of a dive.
#' It can be retrieved from a dive object or computed with a depth and 
#' a palier object.
#'
#' @param object is a DiveR object. There are methods for 
#' \code{\link[DiveR]{dive}} and \code{\link[DiveR]{palier}} objects.
#' @param ... other arguments passed to the dtr.palier method.
#' \describe{
#'   \item{depth}{a numeric in meter}
#'   \item{vup}{10 meter/minute by default.}
#' }
#' @param vup speed of ascent, 10 m/min by default
#'
#' @return It returns a numeric with the duration of the ascent and deco.
#' 
#' @examples 
#' dtr(dive(20,40))
#' # absurd slowness with vup = 1
#' dtr(dive(20,40, vup = 1))
#' # from a palier object
#' dtr(palier(20, 40), depth = 20, vup = 10)
#'
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#'
#' @export
dtr <- function(object, ...) {
  UseMethod("dtr")
}

#' @rdname dtr
#' 
#' @export
dtr.dive <- function(object, ...) {
  object$dtr
}

#' @rdname dtr
#' 
#' @export
dtr.palier <- function(object, ..., vup = 10) {
  palier <- object

  call_par <- list(...)

  if (is.null(call_par$depth)) {
    stop("dtr computed with a palier object require a depth 
         to start the ascent")
  } else {
    depth <- call_par$depth
  }

  # time of deco
  tpal <- sum(palier$time)

  maxpal <- sum(3 * (palier$time > 0))
  # time to deco
  up <- (depth - maxpal) / vup
  # time between deco
  vpal <- maxpal / 6

  dtr <- up + tpal + vpal
  # end
  return(dtr)
}

#' speed
#'
#' \code{speed} compute the ascent speed from a depth and between deco stages 
#' of a dive.
#' It can be retrieved from a dive object.
#'
#' @param object is a DiveR object. There are methods for 
#' \code{\link[DiveR]{dive}} objects.
#'
#' @return It returns a numeric with the duration of the ascent and deco.
#' 
#' @examples 
#' # Dive with default parameters
#' speed(dive(20,40, vup = 10, secu = TRUE))
#' # different vup
#' speed(dive(20,40, vup = 15, secu = TRUE))
#' # No desat stop induce NA for plt speed (between desat stop speed)
#' speed(dive(20,40, vup = 10, secu = FALSE))
#'
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#'
#' @export
speed <- function(object) {
  UseMethod("speed")
}

#' @rdname speed
#' 
#' @export
speed.dive <- function(object) {
  times <- object$dtcurve$times
  depths <- object$dtcurve$depths
  l = length(times)
  asc <- -(depths[4] - depths[3]) / (times[4] - times[3])
  
  if(sum(object$palier$time) > 0){
    plt <- -(depths[l] - depths[l-1]) / (times[l] - times[l -1])
  } else {
    # cat('There is not deco stop so no speed between deco and surface.')
    plt <- NA
  }
  
  return(list(asc = asc, plt = plt))
}

#' summary
#'
#' summary of a dive object
#'
#' @param object is a DiveR object. There are methods for 
#' \code{\link[DiveR]{dive}} and \code{\link[DiveR]{palier}} objects.
#' @param ... other arguments passed to the dtr.palier method.
#' \describe{
#'   \item{plot}{FALSE by default}
#' }
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
  dtr <- dtr(object)
  cat('Total dive time is',dtime(object)+dtr,'with a dive ascent of',dtr,'minutes\n')
  # palier & maj
  summary(object$palier)
}

#' @rdname summary.dive
#'  
#' @examples 
#' summary(palier(depth = 20, time = 50))
#' summary(palier(depth = 20, time = 40,secu = FALSE))
#' 
#' @export
summary.palier <- function(object, ...) {
  sup <- object$time > 0
    n <- sum(sup)
    diz <- sum( object$time[sup] > 9)
    sp <- c(rep('  ',n-diz), rep(' ', diz))
    if (n > 0) {
    t <- paste(sprintf('%s%d minutes at %d meters', sp, object$time[sup], 
                      object$depth[sup]), collapse = '\n          ')
  } else {
    t <- "no stop for deco.\n  Please consider a minimal safety 
                 stop of 3 minute at 3 meter"
  }

  cat(n, "stops :", t,'\n')
  cat('The dive group is',object$group)
}


#' dtime
#'
#' \code{dtime} retrieve the depth time of a singular or multiple dive sequence.
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
dtime.conso <- function(object) {
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

#' depth
#'
#' \code{depth} retrieve the depth of a singular or multiple dive sequence.
#'
#' @param object is a DiveR object. There are methods for 
#' \code{\link[DiveR]{dive}} and \code{\link[DiveR]{ndive}} objects.
#' 
#' @return It returns a numeric with the depth of the dive. 
#' Is a vector if working on ndive object
#' 
#' @examples 
#' # Simple dive
#' depth(dive(20,40))
#' # Multiple dives
#' depth(ndive(dive(20,40), dive(15, 80), inter = 540))
#'
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#'
#' @export
depth <- function(object) {
  UseMethod("depth")
}

#' @rdname depth
#' 
#' @export
depth.dive <- function(object) {
  return(max(object$dtcurve$depths))
}

#' @rdname depth
#' 
#' @export
depth.conso <- function(object) {
  return(max(object$dtcurve$depths))
}

#' @rdname depth
#' 
#' @export
depth.ndive <- function(object) {
  d1 <- depth(object$dive1)
  d2 <- depth(object$dive2)
  return(c(d1,d2))
}


#' secu
#'
#' \code{secu} retrieve if a dive was set with secu = TRUE or FALSE.
#'
#' @param object is a DiveR object. There are methods for 
#' \code{\link[DiveR]{dive}} objects.
#' 
#' @return Boolean
#' 
#' @examples 
#' # Without desat stop
#' secu(dive(20,40, secu = TRUE))
#' secu(dive(20,40, secu = FALSE))
#' # With desat stop
#' secu(dive(20,60, secu = TRUE))
#' secu(dive(20,60, secu = FALSE))
#'
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#'
#' @export
secu <- function(object) {
  UseMethod("secu")
}

#' @rdname secu
#' 
#' @export
secu.dive <- function(object) {
  secutime <- palier(depth = depth(object), dtime(object)+ object$maj, 
                     secu = TRUE)$time
  divetime <- object$palier$time
  return((secutime - divetime)[3] == 0)
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
  time <- time + dive$hour[1]
  times <- dive$dtcurve$times
  depths <- dive$dtcurve$depths
  if(time > max(times)){return(0)}
  
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


#'simpl
#'
#' Remove points of curve when isodepth around
#'
#' @param dtcurve a curve dive list
#' 
#' @examples 
#' cons <- conso(dive = dive(20,40), bloc = bloc(10, 230), cons = 20, mid = 100, reserve = 50)
#' cons$dtcurve
#' simpl(cons$dtcurve)
#'
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#'
#' @export
simpl <- function(dtcurve){
  rm <- c()
  for(i in c(2: (length(dtcurve$depths)-1))){
    if (dtcurve$depths[i] == dtcurve$depths[i-1] & dtcurve$depths[i] == dtcurve$depths[i+1]){
      rm <- append(rm, i)
    }
  }
  if(is.null(rm)) return(dtcurve)
  dtcurve$depths <- dtcurve$depths[-rm]
  dtcurve$times <- dtcurve$times[-rm]
  return(dtcurve)
}