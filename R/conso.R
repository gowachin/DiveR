#' @import stats
NULL

#' bloc
#' 
#' @export
bloc <- function(vol = 10, press = 200, gaz = "O2", typ = "back"){
  bloc <- list(vol = vol, press = press, gaz = gaz, typ = typ)
  class(bloc) <- "bloc"
  return(bloc)
}

#' press_time
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
#' @param dive \code{\link[mn90]{dive}} object
#' 
#' @details 
#' See \code{\link[mn90]{tablecheck}} for limit values of depth and time.
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
  if (any(vpress < 0 )){
    tPA <- c(0, press_time(vpress, dtcurve$times, 0))
    names(tPA) = c('press','time')
  } else {
    tPA <- NULL
  }
  
  conso <- list(dtcurve = dtcurve,
    vcons = vcons, vpress = vpress, 
    time_mid = tmid, time_reserve = treserve, time_PA = tPA
  )
  class(conso) <- "conso"
  return(conso)
}