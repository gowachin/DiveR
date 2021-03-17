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
#' Max time present in the table for a given depth.
#' 
#' @param depth depth in meter. Must be a single positive value 
#' with a maximum is 65m.
#' @param force FALSE by default, if TRUE don't stop the function but 
#' return a TRUE/FALSE value
#' 
#' @return 
#' Single numeric value, the max time possible to dive at the given 
#' depth is the MN90 table.
#'  
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @rdname tablecheck
#' @export
max_depth_time <- function(depth, force = FALSE) {
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
  t <- names(which(!is.na(table[d, ])))
  m <- max(as.numeric(t))
  return(m)
}

#' @rdname tablecheck
#' 
#' @export
max_depth_t <- function(depth, force = FALSE) {
  # TODO : will be removed once useless
  warning("This function is deprecated")
  max_depth_time(depth = depth, force = force)
}

#' desat_table
#' 
#' @param dtcurve a depth time curve in a data.frame with 2 columns depth and 
#' time. Depths values are meters (positive values) and time is in minute.
#' @param maj majoration time in minute in case of consecutive dive. 
#' 0 by default.
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
      all(colnames(dtcurve) != c('depth', 'time'))){
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
  
  return(NA)
}