#' dtr
#'
#' \code{dtr} compute the ascent time from a depth and between deco stages 
#' of a dive.
#' It can be retrieved from a dive object or computed with a depth and 
#' a palier object.
#'
#' @param object is a mn90 object. There are methods for 
#' \code{\link[mn90]{dive}} and \code{\link[mn90]{palier}} objects.
#' @param ... other arguments passed to the dtr.palier method.
#' \describe{
#'   \item{depth}{a numeric in meter}
#'   \item{vup}{10 meter/minute by default.}
#' }
#'
#' @return It returns a numeric with the duration of the ascent and deco.
#'
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#'
#' @export
dtr <- function(object, ...) {
  UseMethod("dtr")
}

#' @rdname dtr
#' @usage ## S3 method for class 'dive'
#' # dtr(dive)
#' @export
dtr.dive <- function(object, ...) {
  object$dtr
}

#' @rdname dtr
#' @usage ## S3 method for class 'palier'
#' # dtr(palier, depth, vup = 10)
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

#' summary
#' 
#' TODO summary ndive
#'
#' summary of a dive object
#'
#' @param object is a mn90 object. There are methods for 
#' \code{\link[mn90]{dive}} and \code{\link[mn90]{palier}} objects.
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
#' @usage ## S3 method for class 'palier'
#' # summary(palier)
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
#' \code{dtime} retrive the depth time of a singular or multiple dive sequence.
#'
#' @param object is a mn90 object. There are methods for 
#' \code{\link[mn90]{dive}} objects.
#'
#' @return a single numeric value
#'
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#'
#' @export
dtime <- function(object) {
  UseMethod("dtime")
}

#' @rdname dtime
#' @usage ## S3 method for class 'dive'
#' # dtime(dive)
#' @export
dtime.dive <- function(object) {
  t <- object$dtcurve$times
  dt <- min(t[t > 0])
  return(dt)
}

#' depth
#'
#' \code{depth} retrive the depth of a singular or multiple dive sequence.
#'
#' @param object is a mn90 object. There are methods for 
#' \code{\link[mn90]{dive}} objects.
#' 
#' @return It returns a numeric with the duration of the ascent and deco.
#'
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#'
#' @export
depth <- function(object) {
  UseMethod("depth")
}

#' @rdname depth
#' @usage ## S3 method for class 'dive'
#' # depth(dive)
#' @export
depth.dive <- function(object) {
  return(max(object$dtcurve$depths))
}
