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
#'   \item{ascent_speed}{10 meter/minute by default.}
#' }
#' @param ascent_speed speed of ascent, 10 m/min by default
#'
#' @return It returns a numeric with the duration of the ascent and deco.
#' 
#' @examples 
#' dtr(dive(20,40))
#' # absurd slowness with ascent_speed = 1
#' dtr(dive(20,40, ascent_speed = 1))
#' # from a palier object
#' dtr(palier(20, 40), depth = 20, ascent_speed = 10)
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
  unname(object$params["dtr"])
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
#' speed(dive(20,40, ascent_speed = 10, secu = TRUE))
#' # different ascent_speed
#' speed(dive(20,40, ascent_speed = 15, secu = TRUE))
#' # No desat stop induce NA for plt speed (between desat stop speed)
#' speed(dive(20,40, ascent_speed = 10, secu = FALSE))
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