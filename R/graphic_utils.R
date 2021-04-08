#' depths_inf
#' 
#' Create information about depth and time to use with text function 
#' in plot.dive. 
#' Find the places and labels to show depending on the dive curve.
#' 
#' @param x a \code{\link[DiveR]{dive}} object.
#' @param col a color value
#' @param only_desat set to \code{FALSE} by default,
#' 
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @export
depths_inf <- function(x, col = "black", only_desat = FALSE){
  # if (!is.dive(x)) stop("x must be a dive object",call. = interactive())
  if( !is.logical(only_desat) | is.na(only_desat) ){
    stop('only_desat must be TRUE or FALSE',call. = interactive())
  }
  
  x$dtcurve <- simpl(x$dtcurve)
  
  if(only_desat){
    depths <- x$desat$desat_stop$depth
    times <- x$desat$desat_stop$time + x$desat$desat_stop$hour
    pos <- rep(4, nrow(x$desat$desat_stop))
  } else {
    x$dtcurve <- rbind(x$dtcurve, c(0, 720))
    tmp <- x$dtcurve[head(x$dtcurve$depths, -1) > tail(x$dtcurve$depths, -1),]
    pos <- rep(4, nrow(tmp))
    depths <- tmp$depths
    times <- tmp$times
    pos[depths == max(x$dtcurve$depths)] <- 1
  }
  
  depth_inf <- list(x = times, y = -depths, labels = paste(-depths, "m"), 
                    pos = pos, col = col)
  
  depth_inf <- as.data.frame(depth_inf, stringsAsFactors = FALSE)
  return(depth_inf)
}


#' times_inf
#'  
#' @param x a \code{\link[DiveR]{dive}} object.
#' @param col a color value
#' 
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' @rdname depths_inf
#' @export
times_inf <- function(x, col = "black"){
  # if (!is.dive(x)) stop("x must be a dive object",call. = interactive())
  
  x$dtcurve <- simpl(x$dtcurve)
  x$dtcurve$times <- x$dtcurve$times - x$hour[1]
  
  dtimes <- diff(c(0,tail(x$dtcurve$times, -1)))
  depths <- (tail(x$dtcurve$depths, -1) +  
               head(x$dtcurve$depths, -1))/2
  times <- (tail(x$dtcurve$times, -1) +  
              head(x$dtcurve$times, -1))/2
  
  times <- times[dtimes >= 3]
  depths <- depths[dtimes >= 3]
  dtimes <- dtimes[dtimes >= 3]
  
  desat <- x$desat$desat_stop
  
  times <- c(times, (desat$time + 2 *desat$hour ) /2 )
  depths <- c(depths, desat$depth)
  dtimes <- c(dtimes, desat$time)
  
  times <- times[!duplicated(dtimes)] + x$hour[1]
  depths <- depths[!duplicated(dtimes)]
  dtimes <- dtimes[!duplicated(dtimes)]
  
  pos <- rep(3,length(dtimes))
  
  time_inf <- list(x = times, y = -depths, 
                   labels = paste0(round(dtimes,1), "'"), 
                   pos = pos, col = col)
  time_inf <- as.data.frame(time_inf, stringsAsFactors = FALSE)
  return(time_inf)
}
