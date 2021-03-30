#' is.dive
#'
#' check if the object is of \code{\link[DiveR]{dive}} class
#' 
#' @param x any R object
#' 
#' @return
#' TRUE or FALSE
#' 
#' @examples
#' d <- dive(20,40)
#' is.dive(d)
#' is.dive('dive')
#' 
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @export
is.dive <- function(x){
  inherits(x, 'dive')
}

#' depth
#'
#' \code{depth} retrieve the maximum depth of a singular or multiple dive 
#' sequence. As conso has a dive inside, the depth function also works on it.
#'
#' @param object is a DiveR object. There are methods for 
#' \code{\link[DiveR]{dive}}, \code{\link[DiveR]{ndive}} and
#'  \code{\link[DiveR]{conso}} objects.
#' 
#' @return It returns a numeric with the depth of the dive. 
#' Is a vector if working on \code{\link[DiveR]{ndive}} object
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
depth.ndive <- function(object) {
  d1 <- depth(object$dive1)
  d2 <- depth(object$dive2)
  return(c(d1,d2))
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
  unname(diff(object$hour) - object$params["dtr"])
}

#' @rdname dtime
#' 
#' @export
dtime.ndive <- function(object) {
  d1 <- dtime(object$dive1)
  d2 <- dtime(object$dive2)
  return(c(d1,d2))
}


#' dtr
#'
#' \code{dtr} retrieve time from end dive time to the surface including
#' desaturation time
#'
#' @param object is a DiveR object. This method is defined for 
#' \code{\link[DiveR]{dive}} objects.
#'
#' @return It returns a numeric with the duration of the ascent 
#' to end of the dive.
#' 
#' @examples 
#' dtr(dive(20,40))
#' # absurd slowness with ascent_speed = 1
#' dtr(dive(20,40, ascent_speed = 1))
#'
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#'
#' @export
dtr <- function(object) {
  UseMethod("dtr")
}

#' @rdname dtr
#' 
#' @export
dtr.dive <- function(object) {
  unname(object$params["dtr"])
}

# TODO : define method for desat


#' depth at time
#'
#' Find the depth for a given time and \code{\link[DiveR]{dive}}.
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
    return(unname(befd))
  } else {
    reg <- lm(c(befd,aftd)~ c(beft,aftt))
    res <- reg$coefficients[2] * time + reg$coefficients[1]
  }
  return(unname(res))
}
