#' is.desat
#'
#' check if the object is of \code{\link[DiveR]{desat_table}} class
#' 
#' @param x any R object
#' 
#' @return
#' TRUE or FALSE
#'  
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @export
is.desat <- function(x){
  inherits(x, 'desat')
}


#' tablecheck
#' 
#' Check if the parameters are possible with table information.
#' 
#' @param depth depth in meter. Must be a single positive value 
#' with a maximum is 65m.
#' @param time time in minute. Must be a single positive value 
#' with a maximum is 180 min
#' @param force FALSE by default, if TRUE don't stop the function but 
#' return a TRUE/FALSE value
#' 
#' @return Nothing expect when \code{force} parameter is TRUE. 
#' Is designed to stop code and returns errors
#' 
#' @details 
#' This function will stop if the depth > 65 or time > 180 because the table 
#' are limited to this extent (the actual table in dataset can evolve)
#' However for lower values the table can return NA values. 
#' This NA return is avoided in the shinyapp.
#' 
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @export
tablecheck <- function(depth, time, force = FALSE) {
  #### LOAD DATA
  table <- DiveR::table
  #### IDIOT PROOF ####
  if (any(depth < 0) | !is.numeric(depth) | length(depth) > 1 ) {
    stop("depth must be positive numeric value.",call. = interactive())
  }
  if (any(time < 0) | !is.numeric(time) | length(time) > 1 ) {
    stop("time must be positive numeric value.", call. = interactive())
  }
  # get table values
  depths <- as.numeric(rownames(table))
  times <- as.numeric(colnames(table))
  
  maxt <- max_depth_time(depth, force = TRUE)
  
  res <- TRUE
  # checks for max
  if (depth > max(depths) | time > max(times)) {
    if (force) {
      res <- FALSE
    } else {
      stop("Time or depth values are outside the mn90 table,
depth must be not exceed 65 and time 3h (180 minutes)
please read doc with ?tablecheck or help(tablecheck)", call. = interactive())
    }
  } else if( time > maxt) {
    if (force) {
      res <- FALSE
    } else {
      stop(sprintf("Maximum time at %d meters is %d minutes",max(depth), maxt), 
           call. = interactive())
    }
  }
  return(res)
}


#' max_depth_time
#' 
#' Max time present in the table for a given depth, with or without deco
#' 
#' @param depth depth in meter. Must be a single positive value 
#' with a maximum is 65m.
#' @param force FALSE by default, if TRUE don't stop the function but 
#' return a TRUE/FALSE value
#' @param no_deco FALSE by default, if TRUE return the time without deco
#' 
#' @return 
#' Single numeric value, the max time possible to dive at the given 
#' depth is the MN90 table.
#'  
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @rdname tablecheck
#' @export
max_depth_time <- function(depth, force = FALSE, no_deco = FALSE) {
  #### LOAD DATA
  table <- DiveR::table[, , 1]
  #### IDIOT PROOF ####
  if (any(depth < 0) | !is.numeric(depth) | length(depth) > 1 ) {
    stop("depth must be positive numeric value.", call. = interactive())
  }
  
  depths <- as.numeric(rownames(table))
  # round to upper depths and times !
  depths <- depths[depths >= depth]
  if(length(depths) == 0){
    if(force){
      return(0)
    } else {
      stop("depth value is outside the mn90 table, depth
must be not exceed 65 meter
please read doc with ?tablecheck or help(tablecheck)", call. = interactive())
    }
  }
  rdepth <- min(depths)
  
  d <- as.character(rdepth)
  if(no_deco){
    t <- names(which(!is.na(table[d,table[d, ] == 0])))
    if(length(t) == 0){
      if(force){
        return(0)
      } else {
        stop("no deco dives are possible below 48m", call. = interactive())
      }
    }
  } else {
    t <- names(which(!is.na(table[d, ])))
  }
  m <- max(as.numeric(t))
  return(m)
}


#' desat_table
#' 
#' Extract time for desat stop using the MN90 tables. Desat stop expected are
#' at 3, 6 and 9 meters depths.
#' 
#' @param dtcurve a depth time curve in a data.frame with 2 columns depth and 
#' time. Depths values are meters (positive values) and time is in minute.
#' @param maj majoration time in minute in case of consecutive dive. 
#' 0 by default.
#' @param altitude Altitude of the dive in meter. Default is 0 m (sea level).
#' @param ppn2 Partial pressure of nitrogen in bar. Default is 0.791 bar
#' 
#' @return a desat object, which is a list with a data.frame containing 
#' desaturation stops at 9, 6 and 3 m depth. Next element is the dive group
#' for possible second dive and lastly the times at which the desaturation
#' stops occur during the dive. The last element is "table" because it's made with 
#' tables. TODO : this has been modified with refacto !
#' 
#' @details 
#' Dive time used in table is the maximum time in the dtcurve table, exept the 
#' last one. This is important as the last time value is expected to be the 
#' time at which the diver reach surface.
#' 
#' @export
desat_table <- function(dtcurve, maj = 0, altitude = 0, ppn2 = 0.791){
  #### LOAD DATA
  table <- DiveR::table
  grp <- DiveR::grp
  #### IDIOT PROOF ####
  if (!inherits(dtcurve, 'data.frame') | any(is.na(dtcurve)) | 
      any(colnames(dtcurve) != c('depth', 'time'))){
    stop(paste('dtcurve must be a data.frame with 2 columns named',
               'depth and time without any NA value'), call. = interactive())
  }
  if (any(dtcurve$depth < 0) | !is.numeric(dtcurve$depth)) {
    stop("depth must be positive numeric value(s).", call. = interactive())
  }
  if (any(dtcurve$time < 0) | !is.numeric(dtcurve$time)) {
    stop("time must be positive numeric value(s).", call. = interactive())
  }
  if (any(dtcurve$time != sort(dtcurve$time))) {
    stop("time values need to be sorted, you don't own a subaquatic dolorean", 
         call. = interactive())
  }
  
  if (any(maj < 0) | !is.numeric(maj) | length(maj) > 1) {
    stop("maj must be a single positive numeric value.", call. = interactive())
  }
  # extract values
  maxtime <- max(head(dtcurve$time, - 1)) + maj
  maxdepth <- max(head(dtcurve$depth, - 1)) # lst depth shld = 0 but we trim it.
  # modify depth with N2
  maxdepth <- ((maxdepth+10) * ppn2/ 0.791) -10
  # get table values
  depths <- as.numeric(rownames(table))
  times <- as.numeric(colnames(table))
  # checks for max
  tablecheck(maxdepth, maxtime)
  # round to upper depths and times !
  rdepth <- min(depths[depths >= maxdepth])
  rtime <- min(times[times >= maxtime])
  # get the times
  desat_time <- rev(table[depths == rdepth, times == rtime, ])
  desat_depth <- rev(as.numeric(gsub('m', '', unlist(dimnames(table)[3]))))
  grup <- grp[depths == rdepth, times == rtime, ]
  # remove the NA check because it's done in tablecheck
desat <- list(
  desat_stop = data.frame(
    depth = desat_depth,
    time = desat_time,
    hour = rep(NA, 3)
  ),
  group = grup, model = "table"
)
  class(desat) <- "desat"
  # end
  return(desat)
}


#' majoration
#'
#' Compute the time majoration to a second dive at a specific depth. 
#' Is related to the residual nitrogen time.
#' 
#' @param depth depth in meter. Must be a single positive value 
#' with a maximum is 60m.
#' @param group byt default "Z", the deco group indicated by a letter. 
#' This value is indicated in a desat object computed with the desat_table 
#' function.
#' @param inter surface interval time in minute. Must be a single positive value 
#' with a minimum of 16 minutes (default). Above 720 (12h), the function will
#' always return 0.
#' 
#' @return a time value to add to the second dive time for using tables.
#' 
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @export
majoration <- function(depth, group = "Z", inter = 16) {
  n2 <- DiveR::nitrogen
  tmaj <- DiveR::maj
  #### IDIOT PROOF ####
  if (any(depth < 0) | !is.numeric(depth) | length(depth) > 1 ) {
    stop("depth must be positive numeric value.", call. = interactive())
  }
  if(depth > 60) stop('depth must be inferior or equal to 60.')
  if (any(inter < 16) | !is.numeric(inter) | length(inter) > 1 ) {
    stop("inter must be positive numeric value above 15.",
         call. = interactive())
  }
  if (!group %in% c(rownames(n2), "Z")) {
    stop("group must be a capital letter between A and P or Z",
         call. = interactive())
  }
  if( group == "Z" & inter < 721){
    stop(paste0('Majoration can not be computed with a group Z',
                ' and less than 12h interval'), call. = interactive())
  }
  if(inter > 720){ # outside tables
    return(0)
  }
  # get n2 values
  grps <- rownames(n2)
  times <- as.numeric(colnames(n2))
  # get tmaj values
  nitrogens <- as.numeric(rownames(tmaj))
  depths <- as.numeric(colnames(tmaj))
  # roud the interval to lower interval given in tables and get nitrogen value
  rinter <- max(times[times <= inter])
  nitrogen <- n2[grps == group, times == rinter]
  if(is.na(nitrogen)){ # nitrogen below 0.81
    return(0)
  }
  # round depth and get maj
  rdepth <- min(depths[depths >= depth])
  rnitrogen <- min(nitrogens[nitrogens >= nitrogen])
  maj <- tmaj[nitrogens == rnitrogen, depths == rdepth]
  
  return(maj)
}


#' table_ndive
#' 
#' @param dive1 the first dive, must be a \code{\link[DiveR]{dive}} object
#' @param dive2 the second dive, must be a \code{\link[DiveR]{dive}} object. 
#' This one will be modified with a majoration obtained from dive1 and 
#' the interval.
#' @param inter 16 by default, interval in minute between the end of the first 
#' dive and the beginning of the second.
#' @param verbose allow cat return in consol for debug purposes. Show which
#' case of sequence is used.
#' 
#' @details 
#' See \code{\link[DiveR]{tablecheck}} for limit values of depth and time 
#' of a dive.
#' 
#' @return ndive, a ndive class object.
#' 
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @export
table_ndive <- function(dive1, dive2, inter = 16, verbose = FALSE){
  #### IDIOT PROOF ####
  if (!is.dive(dive1)) stop("dive1 must be a dive object", call. = interactive())
  if (!is.dive(dive2)) stop("dive2 must be a dive object", call. = interactive())
  if (any(inter < 0) | !is.numeric(inter) | length(inter) > 1 ) {
    stop("inter must be positive numeric value.", call. = interactive())
  }
  if( !is.logical(verbose) | is.na(verbose) ){
    stop('verbose must be TRUE or FALSE', call. = interactive())
  }
  if (dive2$desat$model != "table"){
    stop(paste0("This function is intended to use dive2 with the table",
                " desaturation model"), call. = interactive())
  }
  # # modify dive1 residual N2 to group
  # if (dive1$desat$model != "table"){
  #   
  # }
  
  # retrieve some data avout dive2
  time2 <- dtime(dive2)
  depth2 <- depth(dive2)
  ppo2 <- ppo2(dive2)
  depth2 <- nitrox_depth(depth = depth2, ppn2 = 1 - ppo2)
  secu2 <- as.logical(dive2$params["secu"])
  speed2 <- unname(dive2$params["ascent_speed"])
  raw_dive2 <- rm_desat(dive2)
  
  if (ppo2 == 0.209){
    gas <- 'AIR'
  } else {
    gas <- paste0("NX", ceiling(ppo2 * 100))
  }
  
  if (inter > 15) {
    # Compute majoration
    if (inter > 720) { # 12h interv is not longuer
      maj <- 0
      if (verbose) cat("diff\n")
      type <- "diff"
    } else if (depth2 > 60 | dive1$desat$group == "Z") { # Z is for 60+ dive1
      # Second dive is impossible
      warning(paste0(
        "Second dive impossible in less than 12h ",
        "after a dive a 60 more meters"
      ), call. = interactive())
      ndive <- list(dive1 = dive1, dive2 = "STOP", inter = inter, type = "solo")
      class(ndive) <- "ndive"
      if (verbose) cat("60_no_success\n") # TODO : remove this ?
      return(ndive)
    } else {
      maj <- majoration( 
        depth = depth2, inter = inter, group = dive1$desat$group
      )
      type = "success"
    }
    # check if second dive possible (time in table)
    if (tablecheck(depth2, time2 + maj, force = TRUE) &
        max_depth_time(depth2) >= time2 + maj) {
      # compute second dive
      suc_dive <- dive(
        depth = raw_dive2$dtcurve$depths, time = raw_dive2$dtcurve$times,
        maj = maj, secu = secu2, ascent_speed = speed2, 
        hour = dive1$hour[2] + inter, desat_model = "table", gas = gas
      )
      ndive <- list(
        dive1 = dive1, dive2 = suc_dive, inter = inter, type = type
      )
      if (verbose) cat("success\n")
    } else {
      if (verbose) cat("maj_no_success\n")
      warning(paste0("Second dive impossible due to majoration of time"), 
              call. = interactive())
      # second dive is impossible here in the table
      ndive <- list(
        dive1 = dive1, dive2 = "STOP", inter = inter,
        type = "solo"
      )
    }
  } else {
    # consecutiv dives 
    warning("A minimum of 15 minutes is requiered between dives to consider them
            as different dives.", call. = FALSE)
    # total time of dive
    time <- dtime(dive1) + dive1$params["dtr"] + time2
    # total depth
    depth <- max(depth(dive1), depth2)
    if (max_depth_time(depth) >= time) { # check if second dive possible 
      time1 <- dtime(dive1) + unname(dive1$params["dtr"])
      
      raw_dive2$dtcurve$times <- raw_dive2$dtcurve$times + time1
      res <- rbind(dive1$dtcurve, raw_dive2$dtcurve)
      res <- res[!duplicated(res),]
      
      res <- dive(res$depths, res$times, ascent_speed = speed2, secu = secu2, 
                  hour = dive1$hour[1], gas = gas)
      
      res$dtcurve <- res$dtcurve[! (res$dtcurve$times %in% 
                                      head(dive1$dtcurve$times, -1)),]
      res$dtcurve$times <- res$dtcurve$times - time1
      rownames(res$dtcurve) <- 1:nrow(res$dtcurve)
      res$desat$desat_stop[,3] <- res$desat$desat_stop[,3] - time1
      res$hour <- c((dive1$hour[1] + time1 + inter), res$hour[2] + inter)
      
      ndive <- list(dive1 = dive1, dive2 = res, inter = inter, type = "consec")
      if(verbose) cat('consec\n')
    } else {
      if(verbose) cat('no_consec\n')
      # second dive is impossible here in the table
      warning("Cumulated time of both dives and interval is larger than table.",
              call. = interactive())
      ndive <- list(dive1 = dive1, dive2 = "STOP", inter = inter, type = "solo")
    }
  }
  class(ndive) <- "ndive"
  return(ndive)
}