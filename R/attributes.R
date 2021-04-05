#' dist2time
#' 
#' @param dist Distance vector in meters from points to points.
#' @param speed A diver speed value in meter/minute. 
#' 
dist2time <- function(dist, speed){ # TODO : need to find default value
  time = c(0, cumsum(dist / speed))
}


#' time at depth
#'
#' Find the time for a given depth and dive.
#'
#' @param dive \code{\link[DiveR]{dive}} object.
#' @param depth positive numeric value in meter 
#' 
#' @examples 
#' time_at_depth(dive(20,40), 3)
#' time_at_depth(dive(20,40), 10)
#' time_at_depth(dive(20,40), 20)
#'
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#'
#' @export
time_at_depth <- function(dive, depth){
  times <- dive$dtcurve$times #+ dive$hour[1]
  depths <- dive$dtcurve$depths
  res <- c()
  
  if(depth > max(depths) | depth < 0){
    warning('Depth must be set between 0 and the maximum depth of the dive.')
    return(res)
  }
  # TODO : refacto this code with vector !
  for(i in 2:length(times)){
    if(depth %in% depths[i-1]:depths[i] & depths[i-1] != depths[i]){
      if(times[i-1] == times[i]){
        res <- c(res,times[i])
        next
      }
      reg <- lm(c(depths[i-1], depths[i])~ c(times[i-1], times[i]))
      tmp <- unname( ( depth - reg$coefficients[1] ) / reg$coefficients[2])
      res <- c(res, tmp )
    }
  }

  return(res)
}


#'simpl
#'
#' Remove points of curve when isodepth around
#'
#' @param dtcurve a curve dive list
#' 
#' @examples 
#' cons <- conso(dive = dive(20,40), tank(12, 200), cons = 20)
#' cons$dtcurve
#' simpl(cons$dtcurve)
#'
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#'
#' @export
simpl <- function(dtcurve){
  to_rm <- c()
  for(i in c(2: (length(dtcurve$depths)-1))){
    if (dtcurve$depths[i] == dtcurve$depths[i-1] & dtcurve$depths[i] == dtcurve$depths[i+1]){
      to_rm <- append(to_rm, i)
    }
  }
  if(is.null(to_rm)) return(dtcurve)
  if(class(dtcurve) == "data.frame"){
    dtcurve <- dtcurve[-to_rm,]
  } else {
    dtcurve$depths <- dtcurve$depths[-to_rm]
    dtcurve$times <- dtcurve$times[-to_rm]
  }
  return(dtcurve)
}