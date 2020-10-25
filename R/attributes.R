#' dtr 
#'
#' \code{dtr} returns the ascent and deco time of a dive.
#'
#' @param object is a mn90 object. There are methods for \code{\link[mn90]{dive}} and \code{\link[mn90]{palier}} objects.
#' @param ... other arguments passed to or from other methods
#' @param depth a numeric in meter
#' @param vup 10 meter/minute by default.
#'
#' @return It returns a numeric with the duration of the ascent and deco.
#'
#' 
#' @author Jaunatre Maxime <maxime.jaunatre@etu.univ-grenoble-alpes.fr>
#'
#' @export
dtr <- function(object,...) {
  UseMethod("dtr")
}

#' @rdname dtr
#' @usage ## S3 method for class 'dive'
#' dtr(object = dive)
#' @export
dtr.dive <- function(object,...) {
  object$dtr
}

#' @rdname dtr
#' @usage ## S3 method for class 'palier'
#' dtr(object = palier, depth, vup)
#' @export
dtr.palier <- function(object,...,vup = 10){
  palier <- object
  
  call_par <- list(...)

  if (is.null(call_par$depth)) {
    stop("dtr computed with a palier object require a depth to start the ascent")
  } else {
    depth = call_par$depth
  }
  
  # time of deco
  tpal <- sum(palier$time)
  
  maxpal <- sum(3* (palier$time > 0))
  # time to deco
  up <- (depth - maxpal ) /vup
  # time between deco
  vpal <- maxpal/6
  
  dtr <- up + tpal + vpal
  # end 
  return(dtr)
}