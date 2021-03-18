#' palier
#'
#' Compute the palier depth and time from mn90 table.
#' 
#' @param depth depth in meter. Must be a single positive value 
#' with a maximum is 65m.
#' @param time time in minute. Must be a single positive value 
#' with a maximum is 180 min
#' @param secu true by default, add a secu deco 3 min at 3 meter
#' 
#' @details 
#' This function will stop if the depth > 65 or time > 180 because the table 
#' are limited to this extent.
#' However for lower values the table can return NA values. 
#' 
#' @return palier, a list with a vector of depth and a vector of time. 
#' Vectors are ordered from deepest deco stage (9m) to the higher (3m).
#' 
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @export
palier <- function(depth, time, secu = TRUE) {
  #### LOAD DATA
  table <- DiveR::table
  grp <- DiveR::grp
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
  # checks for max
  tablecheck(depth, time)
  # round to upper depths and times !
  rdepth <- min(depths[depths >= depth])
  rtime <- min(times[times >= time])
  # get the times
  pal <- rev(table[depths == rdepth, times == rtime, ])

  grup <- grp[depths == rdepth, times == rtime, ]

  # if table return NA
  if (sum(is.na(pal))) {
    # return empty dive ?
    stop("out of the table")
  }

  # secu palier
  if (secu) pal[3] <- pal[3] + 3
  palier <- list(
    depth = c(9, 6, 3),
    time = pal,
    group = grup
  )
  class(palier) <- "palier"
  # end
  return(palier)
}

#' dtcurve
#' 
#' Trace a curve of depth and time for given parameters and palier information
#' 
#' @param depth in meter. Max is 65m see Detail !
#' @param time in minute. Max is 180' see Detail !
#' @param palier a object palier computed with the palier function. 
#' Is a list of depth and time for every deco stage.
#' @param ascent_speed 10 m/min max speed up to the deco. 
#' Speed between deco is fixed to 6
#' @param dist a distance vector
#' @param speed speed of the diver
#' @param way if the dive is one way or the diver return by the same depth
#' 
#' @details 
#' This function will stop if the depth > 65 or time > 180 because the table 
#' are limited to this extent.
#' However for lower values the table can return NA values. 
#' 
#' @return a list, with a vector of depths, a vector of time and the dtr value.
#' 
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @export
dtcurve <- function(time, depth, palier, ascent_speed = 10, 
                    dist = NULL, speed = NULL, way = c('AR','AS')) {
  # checks
  if (time <= 0 | !is.numeric(time)) {
    stop("time must be a single positive numeric value.")
  }
  
  if (length(depth) > 1){
    if (is.null(speed)){stop('A speed must be provided')}
    
    vdepth <- depth
    depth <- max(vdepth)
    if (depth <= 0 | !is.numeric(depth)) {
      stop("depth must be a single positive numeric value.")
    }
    
    vtimes <- cumsum(dist/speed)
    if (way == 'AR'){
      vtimes <- vtimes[-length(dist)]
    }
  } else {
    vdepth <- rep(depth, 2)
    if (depth <= 0 | !is.numeric(depth)) {
      stop("depth must be a single positive numeric value.")
    }
    
    vtimes <- c(0, 0, time)
  }
  
  if (class(palier) != "palier") stop("palier must be of class palier")
  # time of deco
  tpal <- sum(palier$time)
  # maxpal
  maxpal <- sum(3 * (palier$time > 0))
  if(any(!vdepth > maxpal)){
    stop('this probleme is not yet fully implemented')
  }
  # time to deco
  up <- (depth - maxpal) / ascent_speed
  # time between deco
  vpal <- maxpal / 6
  dend <- up + tpal + vpal

  depths <- c(
    0, vdepth,
    rev(sort(rep(palier$depth[palier$time > 0], 2))), 0
  )

  times <- c(
    vtimes,
    time + up,
    time + up + palier$time["m9"],
    time + up + palier$time["m9"] + 0.5 * (palier$time[1] > 0),
    time + up + sum(palier$time[c("m9", "m6")]) + 0.5 * (palier$time[1] > 0),
    time + up + sum(palier$time[c("m9", "m6")]) + # line
      sum(0.5 * (palier$time[c(1, 2)] > 0)),
    time + up + sum(palier$time) + sum(0.5 * (palier$time[c(1, 2)] > 0)),
    time + dend
  )

  times <- unname(c(0, times[!duplicated(times)]))
  
  dtr = max(times) - times[max(which(depths == max(depths)))]

  # plot(times, -depths, type = "l")
  # abline(h = 0, col = "darkblue", lty = 3)

  curve <- list(depths = depths, times = times, dtr = dtr)
  # end
  return(curve)
}
