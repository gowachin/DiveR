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


#' summary
#'
#' summary method for class \code{\link[DiveR]{dive}}.
#'
#' @param object is a DiveR object. There are methods for 
#' \code{\link[DiveR]{dive}}, \code{\link[DiveR]{tank}} and
#'  \code{\link[DiveR]{conso}} objects.
#' @param ... other arguments not used
#'
#' @return A brief summary of a dive and its main parameters. 
#' 
#' summary(dive(20, 40, secu = FALSE))
#' summary(dive(20, 40, secu = TRUE))
#' summary(dive(20, 45))
#' summary(dive(39, 22))
#' summary(dive(50, 22))
#'
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#'
#' @export
summary.dive <- function(object, ...){ 
  
  secu <- switch (object$params["secu"]+1,
                  "FALSE","TRUE")
  cat('--------------------------------------------------\n')
  cat(sprintf('Maximum depth : %3.f m  | Depth dive time : %3.f min \n', 
              depth(object), dtime(object)),
      sprintf('Dive ascent : %3.f min | Underwater time : %3.f min\n', 
              dtr(object), diff(object$hour)),
      sprintf(' Majoration : %3.f min | Security stop : %5.5s \n', 
              object$params["maj"], secu)
  )
  cat('--------------------------------------------------\n')
  
  cat('\n|- Desaturation -|\n')
  summary(object$desat)
  # add the secu stop in row
}


#' @rdname summary.dive
#'  
#' @examples
#' summary(desat_table(init_dtcurve(20, 40)))
#' summary(desat_table(init_dtcurve(20, 45)))
#' summary(desat_table(init_dtcurve(39, 22)))
#' summary(desat_table(init_dtcurve(50, 22)))
#' 
#' @export
summary.desat <- function(object, ...) {
  
  sup <- object$desat_stop$time > 0
  n <- sum(sup)
  if(n> 0){
    cat("---------------------------------\n")
    cat(" Stop | Depth | Duration |   Time \n")
    cat("---------------------------------\n")
    for(i in (4-n):3){
      stops <- paste(sprintf(' n %2.f | %3.f m | %4.f min | %2.f min',
                             i,
                             object$desat_stop[i,1],
                             object$desat_stop[i,2],
                             object$desat_stop[i,3]),
                     collapse = '\n')
      cat(stops, "\n")
      cat("---------------------------------\n")
    }
  } else {
    cat("--------- No desat stop ---------\n")
  }
  cat(
    paste(sprintf('    Group : %1.1s | Model : %7.7s \n',
                  object$group, object$model),
          collapse = '\n')
  )
}
