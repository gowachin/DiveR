#' tablecheck
#' 
#' @param depth in meter. Max is 65m see Detail !
#' @param time in minute. Max is 180' see Detail !
#' 
#' @details 
#' This function will stop if the depth > 65 or time > 180 because the table are limited to this extent.
#' However for lower values the table can return NA values. 
#' 
#' @export
tablecheck <- function(depth, time){
  table <- mn90::table
  
  depths <- as.numeric(rownames(table))
  times <- as.numeric(colnames(table))
  # checks for max
  if(depth > max(depths) | time > max(times)){
    stop("Time or depth values are outside the mn90 table\n
         depth must be not exceed 65 and time 3h (180 minutes)\n
         please read doc with ?tablecheck or help(tablecheck)", call. = F)
  }
}

#' palier
#'
#' compute the palier depth and time from mn90 table.
#' @param depth in meter. Max is 65m see Detail !
#' @param time in minute. Max is 180' see Detail !
#' @param secu true by default, add a secu deco 3 min at 3 meter
#' 
#' @details 
#' This function will stop if the depth > 65 or time > 180 because the table are limited to this extent.
#' However for lower values the table can return NA values. 
#' 
#' @return palier, a list with a vector of depth and a vector of time. 
#' Vectors are ordered from deepest deco stage (9m) to the higher (3m).
#' @export
palier <- function(depth, time, secu = TRUE){
  table <- mn90::table
  grp <- mn90::grp
  #print(table)
  # get table values
  depths <- as.numeric(rownames(table))
  times <- as.numeric(colnames(table))
  # checks for max
  tablecheck(depth,time)
  
  # round to upper depths and times !
  rdepth <- min(depths[depths>= depth])
  rtime <- min(times[times>= time])
  # get the times
  pal <- rev(table[depths == rdepth, times == rtime,])
  
  grup <- grp[depths == rdepth, times == rtime,]
  
  # if table return NA
  if(sum(is.na(pal))){
    # return empty dive ?
    stop("out of the table")
  }
  
  # secu palier
  if( secu) pal[3] <- pal[3] + 3
  palier <- list(depth = c(9,6,3),
                 time = pal,
                 group = grup)
  class(palier) = "palier"
  # end
  return(palier)
}

#' curve
#' 
#' compute the time from end of dive to surface
#' 
#' @param depth in meter. Max is 65m see Detail !
#' @param time in minute. Max is 180' see Detail !
#' @param palier a object palier computed with the palier function. 
#' Is a list of depth and time for every deco stage.
#' @param vup 10 m/min max speed up to the deco. 
#' Speed between deco is fixed to 6
#' 
#' @details 
#' This function will stop if the depth > 65 or time > 180 because the table are limited to this extent.
#' However for lower values the table can return NA values. 
#' 
#' @return a list, with a vector of depths, a vector of time and the dtr value.
#' 
#' @export
curve <- function(time, depth, palier, vup = 10){
  # time of deco
  tpal <- sum(palier$time)
  
  # compute dtr
  maxpal <- sum(3* (palier$time > 0))
  # time to deco
  up <- (depth - maxpal ) /vup
  # time between deco
  vpal <- maxpal/6
  dtr <- up + tpal + vpal
  
  depths <- c(0,rep(depth,2),
              rev(sort(rep(palier$depth[palier$time > 0], 2 ))), 0)
  
  times <- c(0, 0,
             time,
             time+up,
             time+up+palier$time["m9"],
             time+up+palier$time["m9"]+0.5 * (palier$time[1] > 0),
             time+up+sum(palier$time[c("m9","m6")])+0.5 * (palier$time[1] > 0),
             time+up+sum(palier$time[c("m9","m6")])+sum(0.5 * (palier$time[c(1,2)] > 0)),
             time+up+sum(palier$time)+sum(0.5 * (palier$time[c(1,2)] > 0)),
             time + dtr)
  
  times <- c(0,times[!duplicated(times)])
  
  # plot(times, -depths, type = "l")
  # abline(h = 0, col = "darkblue", lty = 3)
  names(times) <- NULL
  
  curve <- list(depths = depths, times = times, dtr = dtr)
  # end
  return(curve)
}
