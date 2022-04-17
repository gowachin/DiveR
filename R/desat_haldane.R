#' half_life
#' 
#' compute the load of nitrogen at a given time for a compartment period.
#' 
#' @param period period in minute for the compartment.
#' @param time time at which user want the load of nitrogen
#' 
#' @examples 
#' half_life(50, 50)
#' cpp_half_life(period = 1, time = c(1,2))
#' 
#' 
#' @return percentage load.
#' 
#' @export
half_life <- function(period, time){
  return((1 - 2^(-time/period)) * 100)
  # # old version
  # return((1 - exp(-(log(2)/period) *time)) * 100)
}


#' cut_dtcurve
#' 
#' Adding points in a dtcurve to compute more precise desaturation.
#' 
#' @param dtcurve a depth time curve in a data.frame with 2 columns depth and 
#' time. Depths values are meters (positive values) and time is in minute.
#' @param delta time between 2 points. Default value is set to 1 minute, 
#' minor value will add computation but larger value can be dangerous and 
#' approximate desaturation.
#' @param cut_h is the algorithme going to cut horizontal part of the dtcurve. 
#' FALSE by default.
#' 
#' @return discretised dtcurve table.
#' 
#' @export
cut.dtcurve <- function(dtcurve, delta = 1, cut_h = FALSE){
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
  if (any(delta < 0) | !is.numeric(delta) | length(delta) > 1) {
    stop("delta must be a single positive numeric value.", call. = interactive())
  }
  if( !is.logical(cut_h) | is.na(cut_h) ){
    stop('cut_h must be TRUE or FALSE',
         call. = interactive())
  }
  
  # false dive object for method.
  d <- list(dtcurve = dtcurve)
  colnames(d$dtcurve) <- c('depths', 'times')
  class(d) <- 'dive'
  
  times <- c(); depths <- c()
  for(i in 2:nrow(dtcurve)){
    if(cut_h | dtcurve$depth[i] != dtcurve$depth[i-1]){
      tmp <- seq(dtcurve$time[i-1], dtcurve$time[i], by = delta)
      times <- c(times, tmp)
      depths <- c(depths, rep(NA, length(tmp)))
    } else {
      times <- c(times, dtcurve$time[i])
      depths <- c(depths, dtcurve$depth[i])
    }
  }
  # duplicate last point in case of round value in seq
  times <- c(times, dtcurve$time[i])
  depths <- c(depths, dtcurve$depth[i])
  res <- data.frame(depth = depths, time = times)
  # completing depths
  for(i in 1:nrow(res)){
    if(is.na(res$depth[i])){
      res$depth[i] <- round(depth_at_time(d, res$time[i]), 2)
    }
  }
  tmp <- unique(merge(res, dtcurve, all = TRUE))
  res <- tmp[order(tmp$time),]
  return(res)
}