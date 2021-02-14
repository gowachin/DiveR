#' @import stats
NULL

#'tank
#'
#'Creation of tank object for usage in consumption part of a dive.
#' 
#' @param vol tank volume in litre.
#' @param press tank pression in bar.
#' @param rules tank rules to watch during a dive. A list of two named element :
#' \describe{
#'   \item{"rules"}{vector of 2 named integer indicating a percentage 
#'   or a pression. The names will be used in the plot function later}
#'   \item{"sys"}{character string, either '%' or 'bar'. Percentage must be
#'   between 0 and 100.}
#' }
#' @param gas tank gas, by default "Air". Parameter is here for future dev.
#' @param typ tank type, by default "back"
#' \describe{
#'   \item{"solo"}{single tank}
#'   \item{"relay"}{single tank to be dropped at certain time}
#' }
#' @param limit a two element vector with times between which the tank 
#' is not used. Can be used to mimic an accident, or a relay tank.
#' @param name Possibility to name the tank for better understanding after.
#' by default will be named after the typ and volume.
#' 
#' @details 
#' To set a relay tank, rule1 and rule2 must be the same. Therefore the tank 
#' won't be usable once pressure reach rule2 and until all other tanks are
#' not used. If multiple tanks are used, the relay must be the first one in order
#' 
#' @export
tank <- function(vol, press, rules = list(rules = c('mid' = 50,'res' = 25), 
                                          sys = '%' ), 
                 gas = c("Air"), typ = c("back", "relay"), 
                 limit = NULL, name = NULL){
  
  #### IDIOT PROOF ####
  # vol and press
  if(all(vol <= 0) | ! is.numeric(vol) | length(vol) > 1){
    stop('vol must be a single positive numeric value.')
  }
  if(all(press < 0) | ! is.numeric(press) | length(press) > 1){
    stop('press must be a single positive, 0 possible, numeric value.')
  }
  # rules
  if(length(rules) != 2 | any(names(rules) != c('rules', 'sys'))){
    stop(paste('rules must be a list of length 2 with a vector of 2 numeric',
               'named rules and a single character string being % or bar'))
  }
  rules$sys <- match.arg(rules$sys, c('%', 'bar'))
  if(! is.numeric(rules$rules) | length(rules$rules) != 2){
    stop('Element rules of rules argument must be a vector of 2 numeric')
  }
  for(i in 1:length(rules$rules)){
    if(rules$rules[i] < 0){
      warning('negative rules are not possible and therefor set to 0')
      rules$rules[i] <- 0
    }
  }
  if(is.null(names(rules$rules))){
    names(rules$rules) <- c('', '')
    warning('There was no names for rules, consider setting them for later use')
  }
  # gas
  gas <- match.arg(gas)
  typ <- match.arg(typ)
  
  # TODO : limit vector of 2 positive numeric, negative value trigger warning !
  #### function ####
  # modify rules to bar !
  if(rules$sys == "%"){
    for(i in 1:length(rules$rules)){
      if(rules$rules[i] > 100){
        warning(paste('The rule is superior to 100 %',
                      'Therefore it is changed to the maximum pression'))
        rules$rules[i] <- 100
      }
    }
    rules$rules <- press * rules$rules / 100
  } else if(rules$sys == "bar"){
    for(i in 1:length(rules$rules)){
      if(rules$rules[i] > press){
        warning(paste('The rule is superior to the pression in the tank.',
                      'Therefore it is changed to the maximum pression'))
        rules$rules[i] <- press
      }
    }
  }
  # limit in time 
  # TODO : maybe remove this as it's is poorly defined
  if(is.null(limit)){
    limit <- rep(NA,2)
  } else {
    limit[limit < 0] <- 0
    limit <- sort(limit)
  }
  # name
  if(is.null(name)){
    name <- paste0(typ, vol)
  }
  # numeric vector
  carac <- c(vol, press, unlist(rules$rules))
  names(carac) <- c('vol', 'press', 'rule1', 'rule2')
  # string vector
  typo <- c(gas, typ, names(rules$rules), name)
  names(typo) <- c('gas', 'typ', 'rule1', 'rule2', 'name')
  
  if(gas != 'Air'){
    stop('Only air is working at this moment') # TODO : imput other gas
  } else {
    ppo2 <- c(0.21, 1.6)
    dmin <- (ppo2[1] * 70 / 1.47) -10 # assimilé a ppo2 > 0.18
    dmax <- (ppo2[2] * 70 / 1.47) -10 # assimilé a ppo2 < 1.6
    # round them
    dmin <- ceiling(dmin)
    dmax <- floor(dmax)
  }
  
  limit <- c(dmin,dmax,limit)
  names(limit) <- c('mind', 'maxd', 't1', 't2')
  
  tank <- list(carac = carac, typo = typo, limit = limit)
  class(tank) <- "tank"
  return(tank)
}

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