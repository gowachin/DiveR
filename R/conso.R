#' @import stats
NULL

#' bloc
#' 
#' @param vol tank volume in litre
#' @param press tank pression in bar
#' @param gas tank gas, by default "Air"
#' @param typ tank type, by default "back"
#' \describe{
#'   \item{"solo"}{single tank}
#'   \item{"bi"}{two separated tanks}
#'   \item{"relay"}{single tank to be dropped at certain time}
#'   \item{"deco"}{single tank to be used in deco ascent}
#' }
#' 
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @export
bloc <- function(vol, press, gas = "Air", typ = "back"){
  bloc <- list(vol = vol, press = press, gaz = gas, typ = typ)
  class(bloc) <- "bloc"
  return(bloc)
}

# conso relais - depos relais à la regle - conso bloc perso - demi-tour sur regle perso - recup relais - conso relais

#' press_time
#' 
#' compute the time at which a pression is obtained. Allow to find mid pression
#' time or reserve time.
#' 
#' @param vpress a vector of pression at different times.
#' @param times a vector of times values that match the vpress vector
#' @param press an unique value of pression to be reached.
#' 
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @export
press_time <- function(vpress, times, press = 100){
  
  part <- c(max(which(vpress > press)),min(which(vpress < press)))
  tmp <- lm(vpress[part] ~ times[part])
  timing <- (press - tmp$coefficients[1])/tmp$coefficients[2]
  return(timing)
}

#' conso
#' 
#' @param dive \code{\link[DiveR]{dive}} object
#' 
#' @details 
#' See \code{\link[DiveR]{tablecheck}} for limit values of depth and time.
#' 
#' @examples 
#' c1 = conso(dive = dive(20,40), bloc = bloc(12, 230), cons = 20, mid = 100, reserve = 50)
#' 
#' @return conso, a conso class object.
#' 
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @export
conso <- function(dive, bloc, cons = 20, mid = 100, reserve = 50, 
                  rule = NULL){ #, relais = NULL) {
  # dive = dive(20,40) ; cons = 20 ; mid = 100 ; reserve = 50 ; rule = NULL ; bloc = bloc(vol = 12, press = 230)
  # checks
  check_val(cons)
  # check_val(bloc$vol)
  # check_val(bloc$press)
  
  if (is.null(rule)) {
    check_val(reserve, zero = TRUE)
  } else {
    reserve <- bloc$press * rule$reserve
    mid <- bloc$press * rule$mid
  }
  
  # check if reserve > vol
  
  
  # compute segemental conso
  dtcurve <- dive$dtcurve
  dtcurve$depths <- dtcurve$depths / 10 + 1 
  l <- length(dtcurve$depths) -1
  vcons <- numeric(length = l)
  for (i in 1:l){
    # trapeze method
    vcons[i] <-  cons * (dtcurve$depths[i] + dtcurve$depths[i + 1]) *
      (dtcurve$times[i + 1] - dtcurve$times[i]) / 2
  } 
  vcons <- cumsum(c(0, vcons))
  vcons
  
  vpress <- bloc$press - (vcons / bloc$vol)
  vpress
  
  # compute mid time
  if (any(vpress > mid ) & any(vpress < mid ) ){
    tmid <- c(mid, press_time(vpress, dtcurve$times, mid))
    names(tmid) = c('press','time')
  } else {
    tmid <- NULL
  }
  # compute reserve time
  if (any(vpress > reserve ) & any(vpress < reserve ) ){
    treserve <- c(reserve, press_time(vpress, dtcurve$times, reserve))
    names(treserve) = c('press','time')
  } else {
    treserve <- NULL
  }
  # compute death time
  if (any(vpress <= 0 )){
    tPA <- c(0, press_time(vpress, dtcurve$times, 0))
    # ajouter un point dans la courbe a cet endroit !
    if(! tPA[2] %in% dive$dtcurve$times){
      vpress <- c(vpress, 0)
      # add a depth at this time
      dtimes <- dive$dtcurve$times
      dive$dtcurve$depths <- c(dive$dtcurve$depths[dtimes < tPA[2]],
                               unname(depth_at_time(dive = dive, time = tPA[2])),
                               dive$dtcurve$depths[dtimes > tPA[2]])
      dive$dtcurve$times <- sort(c(dive$dtcurve$times, unname(tPA[2]) ))
    }
    vpress[vpress <= 0] <- 0 
    names(tPA) = c('press','time')
  } else {
    tPA <- NULL
  }
  
  conso <- list(dtcurve = dive$dtcurve,
    vcons = vcons, vpress = vpress, lab = c('midtank', 'reserve', 'PA'),
    time_mid = tmid, time_reserve = treserve, time_PA = tPA, hour = dive$hour
  )
  class(conso) <- "conso"
  return(conso)
}