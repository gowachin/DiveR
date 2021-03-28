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
    stop("depth must be positive numeric value.")
  }
  if (any(time < 0) | !is.numeric(time) | length(time) > 1 ) {
    stop("time must be positive numeric value.")
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
please read doc with ?tablecheck or help(tablecheck)")
    }
  } else if( time > maxt) {
    if (force) {
      res <- FALSE
    } else {
      stop(sprintf("Maximum time at %d meters is %d minutes",depth, maxt), 
           call. = F)
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
    stop("depth must be positive numeric value.")
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
please read doc with ?tablecheck or help(tablecheck)")
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
        stop("no deco dives are not possible below 48m")
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
#' 
#' @return a desat object, which is a list with a data.frame containing 
#' desaturation stops at 9, 6 and 3 m depth. Next element is the dive groupe
#' for possible second dive and lastly the times at which the desaturation
#' stops occur during the dive. The last element is NULL because it's made with 
#' tables.
#' 
#' @details 
#' Dive time used in table is the maximum time in the dtcurve table, exept the 
#' last one. This is important as the last time value is expected to be the 
#' time at which the diver reach surface.
#' 
#' @export
desat_table <- function(dtcurve, maj = 0){
  #### LOAD DATA
  table <- DiveR::table
  grp <- DiveR::grp
  #### IDIOT PROOF ####
  if (!inherits(dtcurve, 'data.frame') | any(is.na(dtcurve)) | 
      any(colnames(dtcurve) != c('depth', 'time'))){
    stop(paste('dtcurve must be a data.frame with 2 columns named',
               'depth and time without any NA value'))
  }
  if (any(dtcurve$depth < 0) | !is.numeric(dtcurve$depth)) {
    stop("depth must be positive numeric value(s).")
  }
  if (any(dtcurve$time < 0) | !is.numeric(dtcurve$time)) {
    stop("time must be positive numeric value(s).")
  }
  if (any(dtcurve$time != sort(dtcurve$time))) {
    stop("time values need to be sorted, you don't own a subaquatic dolorean")
  }
  
  if (any(maj < 0) | !is.numeric(maj) | length(maj) > 1) {
    stop("maj must be a single positive numeric value.")
  }
  # extract values
  maxtime <- max(head(dtcurve$time, - 1)) + maj
  maxdepth <- max(head(dtcurve$depth, - 1)) # lst depth shld = 0 but we trim it.
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
    stop("depth must be positive numeric value.")
  }
  if(depth > 60) stop('depth must be inferior or equal to 60.')
  if (any(inter < 16) | !is.numeric(inter) | length(inter) > 1 ) {
    stop("inter must be positive numeric value above 15.")
  }
  if (!group %in% c(rownames(n2), "Z")) {
    stop("group must be a capital letter between A and P or Z")
  }
  if( group == "Z" & inter < 721){
    stop(paste0('Majoration can not be computed with a group Z',
                ' and less than 12h interval'))
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