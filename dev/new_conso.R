

#' @param vol tank volume in litre
#' @param press tank pression in bar
#' @param rules tank rules to watch during a dive. A list of two named element :
#' \describe{
#'   \item{"rules"}{vector of 2 named integer indicating a percentage 
#'   or a pression. The named will be used in the plot function later}
#'   \item{"sys"}{character string, either '%' or 'bar'}
#' }
#' @param gas tank gas, by default "Air"
#' @param typ tank type, by default "back"
#' \describe{
#'   \item{"solo"}{single tank}
#'   \item{"bi"}{two separated tanks}
#'   \item{"relay"}{single tank to be dropped at certain time}
#'   \item{"deco"}{single tank to be used in deco ascent}
#' }
#' @param limit a two element vector with times between which the tank 
#' is not used. Can be used to mimic an accident, or a relay tank.
#' 
ntank <- function(vol, press, rules = list(rules = c('mid' = 50,'res' = 25), 
                                           sys = '%' ), 
                  gas = "Air", typ = "back", limit = NULL){
  
  # IDIOT PROOF
  # vol numeric >= 1
  # press numeric >= 0
  # rules list length = 2 
  # gas single character string
  # typ single character string
  # limit vector of 2 positive numeric, negative value trigger warning !
  
  # modify rules to bar !
  if(rules$sys == "%"){
    rules$rules <- press * rules$rules / 100
  } else if(rules$sys == "bar"){
    for(i in 1:length(rules$rules)){
      if(rules$rules[i] > press){
        warning(paste('The rule is superior to the pression in the tank.',
                'Therefore it is changed to the maximum pression'))
        rules$rules[i] <- press
      }
    }
  } else {
    stop('rules$sys must be a single character string between "%" and "bar" ')
  }
  
  if(is.null(limit)){
    limit <- rep(NA,2)
  } else {
    limit[limit < 0] <- 0
    limit <- sort(limit)
    stop('Idiot proof of limit not coded yet')
  }
  
  carac <- c(vol, press, unlist(rules$rules))
  names(carac) <- c('vol', 'press', 'rule1', 'rule2')
  typo <- c(gas, typ, names(rules$rules))
  names(typo) <- c('gas', 'typ', 'rule1', 'rule2')
  
  if(gas != 'Air'){
    stop('Only air is working at this moment')
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

y <- ntank(12, 200, rules = list(rules = c('retour' = 100, 'reserve' = 50), 
                                 sys = "bar"))

x <- ntank(12, 200, rules = list(rules = c('retour' = 100, 'reserve' = 50), 
                                 sys = "bar"))
x$limit['t1'] <- 20
x$limit['t2'] <- 30
x

tanks <- list(y, x)

expand(tanks, dive)

expand <- function(tank, dive){
  
  dtime <- dtime(dive)
  
  if(class(tank) == "tank"){
    
    mind <- max(0, tank$limit['mind'])
    maxd <- max(depth(dive), tank$limit['maxd'])
    
    table <- data.frame(min_depth = mind, max_depth = maxd,
                        begin = 0, end = dtime, 
                        type = tank$typo['typ'], usable = T, 
                        press = tank$carac['press'])
    
    # duplicate rows to add time
    if(!all(is.na(tank$limit[c('t1', 't2')]))){
      for(i in c('t1', 't2')){
        if(tank$limit[i] >= 0 & tank$limit[i] < dtime){
          table <- rbind(table, table[1,])
          table$end[nrow(table)] <- table$end[nrow(table)-1]
          table$begin[nrow(table)] <- table$end[nrow(table)-1] <- tank$limit[i]
          
          if(i == 't1') table$usable[nrow(table)] <- FALSE
        }
      }
    }
    
  } else if (class(tank) == 'list' & unique(unlist(lapply(l, class))) == 'tank'){
    # create a table per tank
    tank_list <- lapply(tank, expand, dive)
    names(tank_list) <- lapply(lapply(tank,'[[', 2), '[', 2)
    
    # if(unique(unlist(lapply(tank_list, nrow))) == 1){
    #   # rbind list.
    # }
    
    print(tank_list)
    print(table)
    
    # call expand on every tank and merge after with a specific algo
    stop('not coded yet')
  } else {
    stop('tank must be a single tank object or a list of tanks')
  }
  # trim same time rows,
  table <- table[! table$begin == table$end,]
  # trim rows with FALSE ??
  return(table)
}

#' conso
#' 
#' @param dive \code{\link[DiveR]{dive}} object
#' @param tank \code{\link[DiveR]{ntank}} object or a list of ntank objects.
#'   
#' @return conso, a conso class object.
#' 
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @export
nconso <- function(dive, tank, cons = 20){
  # add possible accident here later one ?
  
  table <- expand(tank)
  PA <- FALSE
  while( PA == FALSE){
    PA <- TRUE
  }
  
  # same output as conso !
  # or change plot.conso and attributes
  
  
}
