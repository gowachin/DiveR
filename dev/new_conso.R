

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
#' @param name Possibility to name the tank for better understanding after.
#' by default will be named after the typ and volume.
#' 
ntank <- function(vol, press, rules = list(rules = c('mid' = 50,'res' = 25), 
                                           sys = '%' ), 
                  gas = "Air", typ = "back", limit = NULL, name = NULL){
  
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
  
  if(is.null(name)){
    name <- paste0(typ, vol)
  }
  
  carac <- c(vol, press, unlist(rules$rules))
  names(carac) <- c('vol', 'press', 'rule1', 'rule2')
  typo <- c(gas, typ, names(rules$rules), name)
  names(typo) <- c('gas', 'typ', 'rule1', 'rule2', 'name')
  
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

dive <- dive(20, 40)

y <- ntank(12, 200, rules = list(rules = c('retour' = 100, 'reserve' = 50), 
                                 sys = "bar"))

x <- ntank(12, 200, rules = list(rules = c('retour' = 100, 'reserve' = 50), 
                                 sys = "bar"), typ = 'relai')
x$limit['t1'] <- 20
x$limit['t2'] <- 30
# x$limit['mind'] <- 10
# x$limit['maxd'] <- 15

expand(x, dive)

y$limit['mind'] <- 5
y$limit['maxd'] <- 10

expand(y, dive)

tank <- list(x, y)

expand(tank, dive)


#' expand
#' 
#' @param tank \code{\link[DiveR]{ntank}} object or a list of ntank objects. 
#' Priority of consumption for tanks is set by their order in list.
#' @param dive \code{\link[DiveR]{dive}} object
#'  
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @export
expand <- function(tank, dive){
  
  dtime <- max(dive$dtcurve$times)
  depth <- depth(dive)
  
  #### Single tank ####
  if(class(tank) == "tank"){
    # init table
    table <- data.frame(min_depth = tank$limit['mind'], 
                        max_depth = tank$limit['maxd'],
                        begin = 0, end = dtime, 
                        type = tank$typo['typ'], 
                        press = tank$carac['press'],
                        vol = tank$carac['vol'])
    # duplicate row to add time when it's not allowed
    if(!all(is.na(tank$limit[c('t1', 't2')]))){
      for(i in c('t1', 't2')){
        if(tank$limit[i] >= 0 & tank$limit[i] < dtime){
          table <- rbind(table, table[1,])
          table$end[nrow(table)] <- table$end[nrow(table)-1]
          table$begin[nrow(table)] <- table$end[nrow(table)-1] <- tank$limit[i]
          
          if(i == 't1') table$press[nrow(table)] <- 0
        }
      }
    }
    # duplicate table for allowed depths or not.
    if((tank$limit['mind']) > 0){
      min_table <- table
      min_table$press <- 0
      min_table$min_depth <- 0
      min_table$max_depth <- tank$limit['mind']
    } else {min_table <- NULL}

    if((tank$limit['maxd']) < depth){
        max_table <- table
        max_table$press <- 0
        max_table$min_depth <- tank$limit['maxd']
        max_table$max_depth <- depth
    } else {
      max_table <- NULL
      table$max_depth <- depth
    }

    table <- rbind(min_table, table, max_table)
    
  #### list of tank ####  
  } else if (class(tank) == 'list' & 
             unique(unlist(lapply(tank, class))) == 'tank'){
    # create a table per tank
    tank_list <- lapply(tank, expand, dive)
    name <- lapply(lapply(tank,'[[', 2), '[', 5)
    tmp <- do.call("rbind",tank_list) # bind the tank list for unique values
    
    mindv <- sort(unique(tmp$min_depth))
    maxdv <- sort(unique(tmp$max_depth))
    t1v <- sort(unique(tmp$begin))
    t2v <- sort(unique(tmp$end))
    
    tmpa <- expand.grid(mindv, t1v)
    tmpb <- expand.grid(maxdv, t2v)
    # table of combinaisons
    table <- cbind(tmpa[,1], tmpb[, 1], tmpa[,2], tmpb[, 2])
    # init empty table of possibility
    table <- cbind(table, as.data.frame(matrix(0, nrow = nrow(table), 
                                            ncol = 3*length(tank))))
    colnames(table) <- c(colnames(tank_list[[1]])[1:4],
                         unlist(lapply(name, paste0, c('', '_press', '_vol'))))
    
    for(i in c(1:length(tank))){
      # possible depths for tank
      possib_depth <- table$min_depth >= tank[[i]]$limit['mind'] &
        table$max_depth <= tank[[i]]$limit['maxd']
      # possible time for tank
      if(!all(is.na(tank[[i]]$limit[c('t1', 't2')]))){
        possib_time <- table$begin < tank[[i]]$limit['t1'] |
          table$end > tank[[i]]$limit['t2'] 
      } else {possib_time <- TRUE}
      # set the typ in column 
      table[, 5+3*(i-1)] <- tank[[i]]$typo['typ']
      # put pression for possible usages
      table[possib_depth & possib_time, 6+3*(i-1)] <- tank[[i]]$carac['press']
      table[, 7+3*(i-1)] <- tank[[i]]$carac['vol']
    }
    
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
#' Priority of consumption for tanks is set by their order in list.
#'   
#' @return conso, a conso class object.
#' 
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @export
nconso <- function(dive, tank, cons = 20){
  
  load("~/git/mn90/prep_conso.RData") ; cons = 20
  # add possible accident here later one ?
  # checks
  check_val(cons)
  # expand the tanks possibility
  table <- expand(tank, dive)
  # extract points to cut dive in time and depths
  times <- unique(c(table$begin, table$end)) # unique(unlist(table[,c(3,4)]))
  from_times <- data.frame(times = times, depths = times)
  for(i in 1:nrow(from_times)) {
    from_times$depths[i] <- depth_at_time(dive, from_times$depths[i])
  }
  
  depths <- as.list(unique(unlist(table[,1:2])))
  for(i in 1:length(depths)) {
    tmp <- time_at_depth(dive, depths[[i]])
    depths[[i]] <- data.frame(times = tmp, 
                              depths = rep(depths[[i]], length(tmp)))
  }
  from_depths <- do.call(rbind, depths)
  
  # join dfs and sort unique with time order. 
  point <- unique(rbind(from_depths,from_times))
  point <- point[order(point$times),]
  # remove points with same time and keep first one (vertical motion)
  point <- point[!duplicated(point$times),]
  # adding points to dtcurve for cutting
  dtcurve <- unique(rbind(data.frame(times = dive$dtcurve$times,
                        depths = dive$dtcurve$depths), point))
  dtcurve <- dtcurve[order(dtcurve$times),]
  dtcurve$pressure <- dtcurve$depths / 10 + 1 

  # below use table, point, dtcurve
  l <- nrow(point) -1
  # init a list of length l
  lcons <- vector(mode = "list", length = l)
  AIR_FAIL <- FALSE
  
  
  # compute consumption dive cut by dive cut
  for (i in 1:l){
    cat('\n ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ \n')
    print(i)
    
    press_cols <- 6+3*(c(1:length(tank))-1)
    
    t1 <- point$times[i] ; t2 <- point$times[i +1]
    tmpdtcurve <- dtcurve[(dtcurve$times >=  t1) & (dtcurve$times <=  t2),]
    d1 <- min(tmpdtcurve$depths) ; d2 <- max(tmpdtcurve$depths)
    tankpres <- table[(table$min_depth <= d1 & table$max_depth >= d2 &
                        table$begin < t2 & table$end > t1),]
    
    if(nrow(tankpres) < 1){
      # case of vertical motion (like square dive first point)
      tankpres <- table[(table$begin < t2 & table$end > t1),]
      # cols <- 6+3*(c(1:length(tank))-1)
      # tank available at all depths and first pressure > 0
      tankpres[,press_cols[!((tankpres[1,press_cols] > 0) & 
                      (colSums(tankpres[,press_cols] > 0) == nrow(tankpres)))
                    ]
               ] <- 0
      tankpres <- tankpres[1, ]
      
      if(nrow(tankpres) < 0){ # TODO : round 2 has death because no tank available !!!
        # AIR FAILURE HERE /!\
        warning('No tank is available and you died. Try again !')
        AIR_FAIL <- TRUE
        break
      }
    } 
    print(tankpres)
    print(tmpdtcurve)
    cat('\n ----------------------------------------- \n')
    
    # plus besoin de point
    ll <- nrow(tmpdtcurve) -1
    # init table of cons and press
    lcons[[i]] <- as.data.frame(matrix(0, nrow = ll, ncol = 1+length(tank)))
    colnames(lcons[[i]]) <- c("vcons")
    
    for (ii in 1:ll){
      # trapeze method
      lcons[[i]][ii,1] <-  cons * (tmpdtcurve$pressure[ii] + 
                                   tmpdtcurve$pressure[ii + 1]) *
                                (tmpdtcurve$times[ii + 1] - 
                                   tmpdtcurve$times[ii]) / 2
      # compute pression in every tank
      tmp_press <- tankpres[press_cols] - (lcons[[i]][ii,1] / 
                                             tankpres[press_cols + 1])
      tmp_press[tmp_press < 0] <- 0
      for(iii in 2:length(tank)){
        if(tmp_press[iii -1] > 0){
          tmp_press[iii:length(tank)] <- 0
          # maube trigger AIR FAILURE HERE TOO
        }
      }
      lcons[[i]][ii,-1] <- tmp_press
      cat('\n ===================================== \n')
    } 
    lcons[[i]][,1] <- cumsum(lcons[[i]][,1])
    
    # apply(table[,press_cols], 1, function(x) x[x> 0] = -42)
    # table[table[,press_cols] > 0] <- 42
    
    print(lcons[[i]])
    
  }
  lcons
  
  # same output as conso !
  # or change plot.conso and attributes
}

