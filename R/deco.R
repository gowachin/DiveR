#' tablecheck
#' 
#' @param depth in meter. Max is 65m see Detail !
#' @param time in minute. Max is 180' see Detail !
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
  table <- mn90::table
  # checks
  check_val(depth)
  check_val(time)
  # get table values
  depths <- as.numeric(rownames(table))
  times <- as.numeric(colnames(table))

  res <- TRUE
  # checks for max
  if (depth > max(depths) | time > max(times)) {
    if (force) {
      res <- FALSE
    } else {
      stop("Time or depth values are outside the mn90 table 
         depth must be not exceed 65 and time 3h (180 minutes)
         please read doc with ?tablecheck or help(tablecheck)", call. = F)
    }
  }
  return(res)
}

#' max_depth_t
#' 
#' Max time present in the table for a given depth.
#' 
#' @param depth in meter. Max is 65m see Detail !
#' 
#' @return a single numeric value, the max time possible to dive at the given 
#' depth is the MN90 table.
#'  
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @export
max_depth_t <- function(depth) {
  table <- mn90::table[, , 1]
  # chekcs
  check_val(depth)

  depths <- as.numeric(rownames(table))
  # round to upper depths and times !
  rdepth <- min(depths[depths >= depth])

  d <- as.character(rdepth)
  t <- names(which(!is.na(table[d, ])))
  m <- max(as.numeric(t))
  return(m)
}

#' palier
#'
#' compute the palier depth and time from mn90 table.
#' @param depth in meter. Max is 65m see Detail !
#' @param time in minute. Max is 180' see Detail !
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
  table <- mn90::table
  grp <- mn90::grp
  # checks
  check_val(depth)
  check_val(time)
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

#' majoration
#'
#' compute the time majoration to a second dive at a specific depth.
#' 
#' @param depth in meter. Max is 65m see Detail !
#' @param group byt default "Z", the deco group indicated by a letter. 
#' This value is indicated in a palier object computed with the palier function.
#' @param inter 16 by default, interval between dives in minutes 
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
majoration <- function(depth, group = "Z", inter = 16) {
  n2 <- mn90::azote
  tmaj <- mn90::maj
  # checks
  check_val(depth)
  if(depth > 60) stop('Depth must be inferior or equal to 60.')
  check_val(inter)
  if (!group %in% c(rownames(n2), "Z")) {
    stop("Group must be a capital letter between A and P or Z")
  }
  # get n2 values
  grps <- rownames(n2)
  times <- as.numeric(colnames(n2))
  # get tmaj values
  azotes <- as.numeric(rownames(tmaj))
  depths <- as.numeric(colnames(tmaj))
  # roud the interval to lower interval given in tables and get azote value
  rinter <- max(times[times <= inter])
  azote <- n2[grps == group, times == rinter]
  # round depth and get maj
  rdepth <- min(depths[depths >= depth])
  razote <- min(azotes[azotes >= azote])
  maj <- tmaj[azotes == razote, depths == rdepth]

  class(maj) <- "maj"
  return(maj)
}

#' dtcurve
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
#' This function will stop if the depth > 65 or time > 180 because the table 
#' are limited to this extent.
#' However for lower values the table can return NA values. 
#' 
#' @return a list, with a vector of depths, a vector of time and the dtr value.
#' 
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @export
dtcurve <- function(time, depth, palier, vup = 10, 
                    dist = NULL, speed = NULL, way = c('AR','AS')) {
  # checks
  check_val(time)
  
  if (length(depth) > 1){
    if (is.null(speed)){stop('A speed must be provided')}
    
    vdepth <- depth
    depth <- max(vdepth)
    check_val(depth)
    
    vtimes <- cumsum(dist/speed)
    if (way == 'AR'){
      vtimes <- vtimes[-length(dist)]
    }
  } else {
    vdepth <- rep(depth, 2)
    check_val(depth)
    
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
  up <- (depth - maxpal) / vup
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
