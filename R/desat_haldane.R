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
#' @rdname half_life
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
#' @param mandatory is depths that are important to add in the dtcurve. 
#' This is essential to add deco stop classical depths.
#' 
#' @return discretised dtcurve table.
#' 
#' @export
cut_dtcurve <- function(dtcurve, delta = 1, cut_h = FALSE, 
                        mandatory = c(9, 6, 3)){
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
  # browser()
  # add mandatory depths
  mand <- list()
  for(i in mandatory){
    tmp <- time_at_depth(d, i)
    mand[[paste0("m",i)]] <- data.frame(
      depth = rep(i, length(tmp)),
      time = tmp
    )
  }
  mand <- do.call(rbind, mand)
  res <- rbind(res, mand)

  
  
  tmp <- unique(merge(res, dtcurve, all = TRUE))
  res <- tmp[order(tmp$time),]
  return(res)
}

# TODO : add special cut for deco stops !

#' desat_haldane
#' 
#' @param dtcurve a depth time curve in a data.frame with 2 columns depth and 
#' time. Depths values are meters (positive values) and time is in minute.
#' @param maj TODO : ignored parameter for now
#' @param altitude Altitude of the dive in meter. Default is 0 m (sea level).
#' @param ppn2 Partial pressure of nitrogen in bar. Default is 0.791 bar
#' @param ncomp Number of compartment for the model. Choice between 5 and 12.
#' 
#' @details There is a modification of the last compartment, with an half-life
#' of 60 instead of 75 min. This is because the Sc value was not found for such
#' time.
#' 
#' Limitation is imposed because we use Haldane model with constant depths,
#' and not the Schreiner equation using depth evolution.
#' 
#' @return a desat object, which is a list with a data.frame containing 
#' desaturation stops at 9, 6 and 3 m depth. Next element is the dive group
#' for possible second dive and lastly the times at which the desaturation
#' stops occur during the dive. The last element is NULL because it's made with 
#' tables.
#' 
#' @export
desat_haldane <- function(dtcurve, maj = 0, altitude = 0, ppn2 = 0.791, ncomp = 5){
  #### IDIOT PROOF ####
  # TODO : copy from desat_table
  
  # # done before
  # ## parameters
  # depth <- 60
  # time <- 25
  # secu <- FALSE
  # ascent_speed <- 10
  # desat_model <- 'Haldane'
  # hour <- 0
  # gas <- 'AIR'
  # way <- 'OW'
  # raw_dtcurve <- init_dtcurve(depth, time, ascent_speed, way)
  # # EOF done before
  # dtcurve <- init_dtcurve(62, 15, 15, "OW")
  dtcurve <- cut_dtcurve(dtcurve, delta = .1, cut_h = FALSE)
  # dtcurve[-c(1:38),]
  
  bpal_speed <- 6 # speed between deco stops
  
  # adding cols for haldane
  dtcurve <- cbind(dtcurve, dt = c(NA, diff(dtcurve$time)),
                   ppn2 = round((dtcurve$depth/10 + 1) * ppn2,3), 
                   anarchy = FALSE, drive = 0, Pabs_pal = 0, max_depth = 0,
                   nex_pal = 0, need_pal = FALSE, time_pal = 0
  )
  
  # Choose nomber of compartement
  Haldane <- switch(as.character(ncomp),
                    "5" = DiveR::Haldane5,
                    "12" = DiveR::Haldane12,
                    DiveR::Haldane5
  )
  comp <- Haldane$comp
  Scomp <- Haldane$Scomp
  depths <- c(0, 3, 6, 9)
  
  dtcurve <- cpp_haldane_desat(dtcurve, comp, Scomp, depths, ppn2, bpal_speed)
  
  # extract the pal time to compare mn90
  pal <- depths
  names(pal) <- paste0("m",pal)
  for(d in seq_along(depths)){
    pal[d] <- sum(dtcurve$time_pal[dtcurve$depth == depths[d]])
  }
  
  desat <- list(
    desat_stop = data.frame(
      depth = rev(depths)[-4],
      time = rev(pal)[-4],
      hour = rep(NA, 3)
    ),
    group = "Z", model = paste0("Hal",ncomp)
    # TODO : add last saturation values and ncomp in ?
  )
  
  class(desat) <- "desat"
  # end
  return(desat)
}
