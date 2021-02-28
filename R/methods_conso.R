#' is.tank
#'
#' check if the object is of \code{\link[DiveR]{tank}} class
#' 
#' @param x any R object
#' 
#' @return
#' TRUE or FALSE
#' 
#' @examples
#' t <- tank(12,200)
#' is.tank(t)
#' is.tank('tank')
#' 
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @export
is.tank <- function(x){
  inherits(x, 'tank')
}

#' pressure
#' \code{pressure} retrieve the pressure of tank(s).
#' 
#' @param object is a DiveR object. There is methods for 
#' \code{\link[DiveR]{tank}} and \code{\link[DiveR]{conso}} objects.
#' @param ... other parameters passed to the pressure.conso method.
#' \describe{
#'   \item{tankn}{the number of the tank if there is multiple. NULL by default
#'   will return all the tanks}
#'   \item{time}{a time in minute. NULL by default will get the last pressure}
#'   \item{hour}{}
#' }
#' @param hour is the time defined inside the hours of the dive (TRUE) or 
#'   in relative time to dive start (FALSE). Set to FALSE by default.
#' 
#' @return 
#' single numeric value for \code{\link[DiveR]{tank}} object and vector for 
#'  \code{\link[DiveR]{conso}} object. 
#'  
#'  @examples 
#'  t <- tank(12,200)
#'  pressure(object = t)
#'  
#'  d <- dive(20,15)
#'  c <- conso(d, t, cons = 20)
#'  pressure(object = c)
#'  
#'  pressure(object = c, time = 7)
#'  
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @export
pressure <- function(object, ...){
  UseMethod("pressure")
}

#' @rdname pressure
#' 
#' @export
pressure.tank <- function(object, ...){
  object$carac['press']
}

#' @rdname pressure
#'  
#' @export
pressure.conso <- function(object, ..., hour = FALSE){
  
  call_par <- list(...)
  
  if(! is.null(call_par$tankn)){ # select only tank of interest
    if (any(call_par$tankn <= 0) | !is.numeric(call_par$tankn) | 
        any(call_par$tankn > (ncol(object$vcons)-2) ) ) {
      stop(paste("tankn must be a numeric vector with values inferior",
                 "or equal to the number of tank used"))
    }
    if (any((call_par$tankn %% 1) != 0)){
      stop("tankn can't have fractionnal part")
    }
    
    tankn <- call_par$tankn + 2
  } else { # all tanks are used
    tankn <- 3:ncol(object$vcons) 
  }
  
  if(is.null(call_par$time)){ # no time specified
    res <- apply(object$vcons,2, min, na.rm = TRUE)[tankn]
  } else {
    time <- call_par$time
    # check for hour input
    if( !is.logical(hour) | is.na(hour) ){
      stop('hour must be TRUE or FALSE')
    }
    times <- object$vcons$times
    press <- object$vcons[,tankn, drop = FALSE]
    if(hour){
      times <- times + object$hour[1]
    } 
    
    if (any(call_par$time < min(times)) | !is.numeric(call_par$time) | 
        any(call_par$time > max(times)) | length(call_par$time) > 1) {
      stop(paste("time must be a numeric value inside the time",
                 "of the dive. Check the times of the dive and you usage of",
                 "the hour parameter"))
    }
    
    befp <-  unlist(tail(press[times < time,], 1))
    aftp <- unlist(press[times >= time,, drop = FALSE][1,])
    
    beft <- tail(times[times < time], 1)
    aftt <- times[times >= time][1]
    
    res <- numeric(length = ncol(press))
    for(i in 1:ncol(press)){
      if(befp[i] == aftp[i]){
        res[i] <- befp[i]
      } else {
        reg <- lm(c(befp[i], aftp[i])~ c(beft,aftt))
        res[i] <- reg$coefficients[2] * time + reg$coefficients[1]
      }
    }
  }
  
  res[is.na(res)] <- 0
  
  return(res)
}

#' volume
#' \code{volume} retrieve the volume of tank(s).
#' 
#' @param object is a DiveR object. There is methods for 
#' \code{\link[DiveR]{tank}} object.
#' 
#' @return 
#' single numeric value for \code{\link[DiveR]{tank}} object.
#'  
#'  @examples 
#'  t <- tank(12,200)
#'  volume(object = t)
#'  
#'  
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @export
volume <- function(object){
  UseMethod("volume")
}

#' @rdname volume
#' 
#' @export
volume.tank <- function(object){
  object$carac['vol']
}