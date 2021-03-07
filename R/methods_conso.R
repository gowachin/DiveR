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

#' is.conso
#'
#' check if the object is of \code{\link[DiveR]{conso}} class
#' 
#' @param x any R object
#' 
#' @return
#' TRUE or FALSE
#' 
#' @examples
#' t <- tank(12,200)
#' d <- dive(20, 40)
#' c <- conso(d, t)
#' is.tank(c)
#' is.tank('conso')
#' 
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @export
is.conso <- function(x){
  inherits(x, 'conso')
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
  unname(object$carac['press'])
}

#' @rdname pressure
#'  
#' @export
pressure.conso <- function(object, ..., hour = FALSE){
  
  call_par <- list(...)
  
  if(! is.null(call_par$tankn)){ # select only tank of interest
    if (any(call_par$tankn <= 0) | !is.numeric(call_par$tankn) | 
        any(call_par$tankn > nrow(object$rules) ) ) {
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
  unname(object$carac['vol'])
}


#' rules
#' 
#' \code{rules} retrieve the rules of a tank or
#' pressure and times at which the pressure of tank(s) met the rules in conso.
#' 
#' @param object is a DiveR object. There is methods for 
#' \code{\link[DiveR]{tank}} and \code{\link[DiveR]{conso}} objects.
#' @param ... other parameters passed to the pressure.conso method.
#' \describe{
#'   \item{tankn}{the number of the tank if there is multiple. NULL by default
#'   will return all the tanks}
#'   \item{n}{the number of the rule. 0 mean the air-failure, 1 rule1 and 
#'   2 rule2. NULL by default will get all of them. A vector of values can 
#'   be provided}
#' }
#' @param hour if the time defined inside the hours of the dive (TRUE) or 
#'   in relative time to dive start (FALSE). Set to FALSE by default.
#' 
#' @return 
#' \describe{
#'   \item{for \code{\link[DiveR]{tank}}}{numeric vector for rules in their
#'    order}
#'   \item{for \code{\link[DiveR]{conso}}}{data.frame with rules in column and
#'   tanks in rows}
#' }
#'  
#'  @examples 
#'  t <- tank(12,200)
#'  rules(object = t)
#'  
#'  d <- dive(20,15)
#'  c <- conso(d, t, cons = 20)
#'  rules(object = c)
#'  
#'  rules(object = c, n = 0)
#'  
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @export
rules <- function(object, ...){
  UseMethod("rules")
}

#' @rdname rules
#' 
#' @export
rules.tank <- function(object, ...){
  res <- object$carac[c('rule1', 'rule2')] # values
  names(res) <- object$typo[c('rule1', 'rule2')] # names
  return(res)
}

#' @rdname rules
#'  
#' @export
rules.conso <- function(object, ..., hour = FALSE){
  
  call_par <- list(...)
  
  if(! is.null(call_par$tankn)){ # select only tank of interest
    if (any(call_par$tankn <= 0) | !is.numeric(call_par$tankn) |
        any(call_par$tankn > nrow(object$rules) ) ) {
      stop(paste("tankn must be a numeric vector with values inferior",
                 "or equal to the number of tank used"))
    }
    if (any((call_par$tankn %% 1) != 0)){
      stop("tankn can't have fractionnal part")
    }

    tankn <- call_par$tankn
  } else { # all tanks are used
    tankn <- 1:nrow(object$rules)
  }

  if(is.null(call_par$n)){ # no time specified
    res <- object$rules[tankn, ]
    n <- 1:9
  } else {
    # check for hour input
    if( !is.numeric(call_par$n) | ! all( call_par$n %in% 0:2) ){
      stop("n must be a numeric vector with values between 0 and 2")
    }
    n <- sort(unique(call_par$n))
    
    if(0 %in% n){
      af <- 7:9
      n <- n[- (n == 0)]
    } else {
      af <- c()
    }
    n <- sort(c((1 + ((n-1) *3)), (2 + ((n-1) *3)), (3 + ((n-1) *3)), af ) )
    
    res <- object$rules[tankn, n]
  }
  
  # modify with hour here
  if( !is.logical(hour) | is.na(hour) ){
    stop('hour must be TRUE or FALSE')
  }
  if(hour){
    n <- n[(n %% 3) == 0]
    res[,n] <- res[,n] + object$hour[1]
  }
  
  return(res)
}


#' @rdname summary.dive
#'  
#' @examples 
#' summary(tank(vol = 12, press = 200))
#' 
#' @export
summary.tank <- function(object, ...){
  # parameters volume and pressure
  cat(paste("Tank :",volume(object), "litre at",pressure(object),"bar\n"))
  # rules
  t <- paste(sprintf('%d %s : %3.f bar', 
                     c(1:2),
                     names(rules(object)), 
                     rules(object)), collapse = '\n        ')
  cat("rules :", t)
  # typ and gas
  cat("\nThe tank type is", object$typo["typ"],
      "and contain", object$typo["gas"], "\n")
  # name
  cat("Named :", object$typo["name"], "\n")
}

#' @rdname summary.dive
#'  
#' @examples 
#' Tank_15L <- tank(vol = 15, press = 200)
#' viable <- conso(dive = dive(20,40), tank = Tank_15L, 
#'                 cons = 20, failure_label = 'Air failure')
#' summary(viable)
#' 
#' @export
summary.conso <- function(object, ...){
  # duration and depth of the dive
  cat("Consumption simulated on dive at", depth(object),"m for", 
      diff(object$hour), "minutes\n")
  
  Ltank <- nrow(object$rules)
  cat("---------------------------------------------------------------------\n")
  cat("       Tank name |         Rule | Pressure |    Time | Final pressure \n")
  cat("---------------------------------------------------------------------\n")
  for(i in 1:Ltank){
    
    name = rownames(object$rules)[i]
    tanks <- paste(sprintf('%16.16s | %12.12s | %4.f bar | %3.f min | %3s', 
                           c(paste("Tank",name), 
                             rep(paste0(rep(" ",5+nchar(name)),
                                        collapse = ""), 2)),
                           object$rules[i,c(2,5,8)],
                           object$rules[i,c(1,4,7)], 
                           object$rules[i,c(3,6,9)],
                           c(paste(as.character(pressure(object)[i]),"bar"), 
                             c("",""))), 
                   collapse = '\n')
    cat(tanks, "\n")
    
    cat("---------------------------------------------------------------------\n")
  }
  # Viable
  if( sum(pressure(object)) ){
    cat("The dive is viable !\n")
  } else {
    cat("The dive is deadly !\n")
  }
}

#' @rdname depth
#' 
#' @export
depth.conso <- function(object) {
  return(max(object$dtcurve$depths))
}
