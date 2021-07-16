# HALDANE
library(DiveR)
## parameters
depth <- 40
time <- 15
secu <- FALSE
ascent_speed <- 10
desat_model <- 'Haldane'
hour <- 0
gas <- 'AIR'
way <- 'OW'

# https://en.wikipedia.org/wiki/B%C3%BChlmann_decompression_algorithm#Tables

raw_dtcurve <- init_dtcurve(depth, time, ascent_speed, way)

desat_stop <- desat_table(dtcurve = raw_dtcurve, maj = maj, ppn2 = 0.791)
desat_stop$desat_stop$time[3] <- 3
raw_dtcurve <- add_desat(raw_dtcurve, desat_stop, ascent_speed, secu)

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

raw_dtcurve <- cut.dtcurve(dtcurve = raw_dtcurve, delta = .05, cut_h = TRUE)
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
#' @details There is a modification of the last compartement, with an half-life
#' of 60 instead of 75 min. This is because the Sc value was not found for such
#' time.
#' 
#' @return a desat object (list)
#' 
#' @export
# desat_haldane <- function(dtcurve, maj = 0, altitude = 0, ppn2 = 0.791){
  #### IDIOT PROOF ####
  # TODO : copy from desat_table
  #
  
  dtcurve = raw_dtcurve #
  maj = 0               #
  altitude = 0          #
  ppn2 = 0.791          #
  
  comp <- c(5, 10, 20, 40, 60)
  Scomp <- c(Sc5 = 2.72, Sc10 = 2.38, Sc20 = 2.04, Sc40 = 1.68, Sc60 = 1.58)
  depths <- -c(3, 6, 9)
  
  
  # pal = 7
  # dtcurve <- rbind(dtcurve, dtcurve[41,])
  # dtcurve[41,1:2] <- c(3, 19 + pal)
  # dtcurve[42, 2] <- dtcurve[42, 2] + pal
  
  # adding cols for haldane
  dtcurve <- cbind(dtcurve, dt = c(NA, diff(dtcurve$time)),
                   ppn2 = round((dtcurve$depth/10 + 1) * ppn2,3), 
                   c5 = NA, c10 = NA, c20 = NA, c40 = NA, c60 = NA,
                   S5 = 1, S10 = 1, S20 = 1, S40 = 1, S60 = 1, 
                   anarchy = FALSE, drive = 0
                   )
  dtcurve[1, 5:9] <- 0.791
  

  
  # Graphic
  plot(0,xlim = c(-2, 30), ylim = c(0, 4), type = 'n')
  
  i = 2
  while(i <= nrow(dtcurve)){
    # compute 
    dtcurve[i, 5:9] <- (dtcurve$ppn2[i] - dtcurve[i-1, 5:9]) *
      half_life(comp, dtcurve$dt[i])/100  + dtcurve[i-1, 5:9]
    dtcurve[i, 10:14] <- dtcurve[i, 5:9] / (dtcurve$ppn2[i]/dtcurve$ppn2[1]) # S = Tn2 / Pabs
    anar <- dtcurve[i, 10:14] >= Scomp
    dtcurve$anarchy[i] <- any(anar)
    # if(dtcurve$anarchy[i]){
    #   depth <- (max(dtcurve[i, 10:14][anar] / Scomp[anar]) - 1) * 10
    #   depth <- - max(depths[(depth + depths) < 0 ])
    #   
    #   1.3 * log(1 + ((2.647 - 2.38)/ (0.791 - 2.647))) / log(.5)
    #   
    # }
    
    # Graphic
    points(rep(dtcurve$time[i], 5), dtcurve[i, 5:9], col = c(1:5), pch = 20)
    points(rep(dtcurve$time[i], 5), dtcurve[i, 10:14], col = c(1:5), pch = 10, 
           cex = .5 + (dtcurve[i, 10:14] >= Scomp))
    
    i <- i + 1
  }
  # Graphic
  abline(h = c(0.791,5*0.791), col = 'red', lty = 3)
  abline(h = Scomp, col = c(1:5), lty = 5)
  
  dtcurve
  
# } 

desat_dtcurve <- add_desat(raw_dtcurve, desat_stop, ascent_speed, secu)
dtr <- max(desat_dtcurve$time) - tail(raw_dtcurve$time, 2)[1]