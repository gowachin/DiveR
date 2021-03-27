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
  #### IDIOT PROOF
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

#' @rdname dtr
#' 
#' @export
dtr.palier <- function(object, ..., ascent_speed = 10) {
  palier <- object
  
  call_par <- list(...)
  
  if (is.null(call_par$depth)) {
    stop("dtr computed with a palier object require a depth 
         to start the ascent")
  } else {
    depth <- call_par$depth
  }
  
  # time of deco
  tpal <- sum(palier$time)
  
  maxpal <- sum(3 * (palier$time > 0))
  # time to deco
  up <- (depth - maxpal) / ascent_speed
  # time between deco
  vpal <- maxpal / 6
  
  dtr <- up + tpal + vpal
  # end
  return(dtr)
}
