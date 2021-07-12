# HALDANE
library(DiveR)
## parameters
depth <- 40
time <- 15
secu <- TRUE
ascent_speed <- 10
desat_model <- 'Haldane'
hour <- 0
gas <- 'AIR'
way <- 'OW'

# https://en.wikipedia.org/wiki/B%C3%BChlmann_decompression_algorithm#Tables

raw_dtcurve <- init_dtcurve(depth, time, ascent_speed, way)

desat_stop <- list(desat_stop = data.frame(depth = 0, time = 0, hour = NA,
                                           row.names = "m0"),
                   group = 'Z', model = "other")
class(desat_stop) <- "desat"

## modify desat_stop

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

raw_dtcurve <- cut.dtcurve(dtcurve = raw_dtcurve, delta = .5, cut_h = TRUE)
raw_dtcurve

# dv <- c(0)
# for(i in 1:7){
#   if(i == 1){
#     dv <- c(dv, 50)
#   } else {
#     dv <- c(dv, dv[i] + (100 - dv[i])/2)
#   }
# }
# dv <- round(dv, 3)
# dv
# a = 10
# plot(seq(0,7*a, by = a), dv, type = 'b')
# t <- 0:100
# p <-  (1 - exp(-(log(2)/10) *t)) * 100
# lines(t, p, col = 'red')
# plot(t, p)

#' half_life
#' 
#' compute the load of nitrogen at a given time for a compartment period.
#' 
#' @param period period in minute for the compartment.
#' @param time time at which user want the load of nitrogen
#' 
#' @return percentage load.
#' 
#' @export
half_life <- function(period, time){
  return((1 - exp(-(log(2)/period) *time)) * 100)
}


#' desat_haldane
#' 
#' @param dtcurve
#' @param maj
#' @param altitude
#' @param ppn2
#' 
#' @return a desat object (list)
#' 
#' @export
desat_haldane <- function(dtcurve, maj = 0, altitude = 0, ppn2 = 0.791){
  #### IDIOT PROOF ####
  # TODO : copy from desat_table
  #
  
  dtcurve = raw_dtcurve #
  maj = 0               #
  altitude = 0          #
  ppn2 = 0.791          #
  
  # adding cols for haldane
  dtcurve <- cbind(dtcurve, dt = c(NA, diff(dtcurve$time)),
                   ppn2 = round((dtcurve$depth/10 + 1) * ppn2,3), 
                   c5 = NA, c10 = NA, c20 = NA, c40 = NA, c75 = NA, 
                   Sc5 = NA, Sc10 = NA, Sc20 = NA, Sc40 = NA, Sc75 = NA
                   # Sc5 = 2.72, Sc10 = 2.38, Sc20 = 2.04, Sc40 = 1.68, Sc75 = NA
                   )
  dtcurve[1, 5:9] <- 0.791
  
  plot(0,xlim = c(-2, 24), ylim = c(0.7, 4), type = 'n')
  
  comp <- c(5, 10, 20, 40, 75)
  
  for(i in 2:nrow(dtcurve)){
    dtcurve[i, 5:9] <- (dtcurve$ppn2[i] - dtcurve[i-1, 5:9]) *
      half_life(comp, dtcurve$dt[i])/100  + dtcurve[i-1, 5:9]
    points(rep(dtcurve$time[i], 5), dtcurve[i, 5:9], col = c(1:5), pch = 20)
  }
  abline(h = c(0.791,5*0.791), col = 'red', lty = 3)
  # dtcurve
  
}

desat_dtcurve <- add_desat(raw_dtcurve, desat_stop, ascent_speed, secu)
dtr <- max(desat_dtcurve$time) - tail(raw_dtcurve$time, 2)[1]