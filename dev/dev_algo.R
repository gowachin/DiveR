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

desat_stop <- desat_table(dtcurve = raw_dtcurve, maj = 0, ppn2 = 0.791)
desat_stop$desat_stop$time[3] <- 0
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
  return((1 - 2^(-time/period)) * 100)
}

# half_lifeb <- function(period, time){
#   return((1 - exp(-(log(2)/period) *time)) * 100)
# }


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
#' Limitation is imposed because we use Haldane model with constant depths,
#' and not the Schreiner equation using depth evolution.
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
  depths <- c(0, 3, 6, 9)
  
  
  # pal = 7
  # dtcurve <- rbind(dtcurve, dtcurve[41,])
  # dtcurve[41,1:2] <- c(3, 19 + pal)
  # dtcurve[42, 2] <- dtcurve[42, 2] + pal
  
  # adding cols for haldane
  dtcurve <- cbind(dtcurve, dt = c(NA, diff(dtcurve$time)),
                   ppn2 = round((dtcurve$depth/10 + 1) * ppn2,3), 
                   c5 = NA, c10 = NA, c20 = NA, c40 = NA, c60 = NA,
                   S5 = 1, S10 = 1, S20 = 1, S40 = 1, S60 = 1, 
                   anarchy = FALSE, drive = 0, Pabs_pal = 0, max_depth = 0,
                   nex_pal = 0, need_pal = FALSE, time_pal = 0
                   )
  dtcurve[1, 5:9] <- 0.791
  
  # pre-insert stop part
  rows <- head(which(dtcurve$depth %in% depths)[-1], -1)
  #' copy from SO 
  #' https://stackoverflow.com/questions/11561856/add-new-row-to-dataframe-at-specific-row-index-not-appended
  insertRow <- function(existingDF, newrow, r) {
    existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
    existingDF[r,] <- newrow
    existingDF
  }
  
  for(r in 1:length(rows)){
    dtcurve <- insertRow(dtcurve, dtcurve[rows[r],], rows[r])
    dtcurve$dt[rows[r]+1] <- 0
    rows <- rows+ 1
  }
  dtcurve[360: 380, 1:4]
  
  # Graphic
  plot(0,xlim = c(-2, 30), ylim = c(0, 4), type = 'n')
  
  i = 2
  while(i <= nrow(dtcurve)){
    # compute 
    dtcurve[i, 5:9] <- (dtcurve$ppn2[i] - dtcurve[i-1, 5:9]) *
      half_life(comp, dtcurve$dt[i])/100  + dtcurve[i-1, 5:9]
    dtcurve[i, 10:14] <- dtcurve[i, 5:9] / (dtcurve$ppn2[i]/dtcurve$ppn2[1]) # S = Tn2 / Pabs
    anar <- dtcurve[i, 10:14] >= Scomp
    dtcurve$anarchy[i] <- any(anar) # is any compartement anar
    
    Pabs_pal <- dtcurve[i, 5:9] / Scomp # Pabs = Tn2 / Sc compart
    dtcurve$drive[i] <- which.max(Pabs_pal)
    dtcurve$Pabs_pal[i] <- max(Pabs_pal)
    dtcurve$max_depth[i] <- (dtcurve$Pabs_pal[i] - 1) * 10
    if( dtcurve$max_depth[i] < 0 | round(dtcurve$max_depth[i], 3) == 0){
      dtcurve$max_depth[i] <- 0
    } 
    # okay on c'est quand il faut un palier et sa profondeur !
    dtcurve$nex_pal[i] <- min(depths[(dtcurve$max_depth[i] - depths) <= 0])
    
    
    # Graphic
    points(rep(dtcurve$time[i], 5), dtcurve[i, 5:9], col = c(1:5), pch = 20)
    points(rep(dtcurve$time[i], 5), dtcurve[i, 10:14], col = c(1:5), pch = 10, 
           cex = .5 + (dtcurve[i, 10:14] >= Scomp))
    
     # on est a la profondeur d'un palier requis
    dtcurve$need_pal[i] <- dtcurve$depth[i] <= dtcurve$nex_pal[i]
    if(dtcurve$need_pal[i] & dtcurve$depth[i] != 0){
      cat('shit...')
      next_pal <- depths[which(depths == dtcurve$nex_pal[i]) -1]
      pal_time <- -comp[dtcurve$drive[i]] * (log(1-(((Scomp[dtcurve$drive[i]] * (next_pal / 10 +1) )-
                                                       dtcurve[i,5:9][dtcurve$drive[i]])/
                                                      (dtcurve$ppn2[i] - dtcurve[i,5:9][dtcurve$drive[i]])))/
                                               log(2))
      # print(str(pal_time))
      cat(" - t: ", unlist(pal_time), "\n")
      dtcurve$time_pal[i] <- unname(unlist(pal_time))
      dtcurve$dt[i+1] <- unname(unlist(pal_time))
    } 
    
    # TODO : probleme d'arrondis dans les diffs
    if(dtcurve$depth[i] != 0 & round(dtcurve$dt[i], 3) > round(dtcurve$time[i+1] - dtcurve$time[i], 3)){
      dtcurve$time[(i+1):nrow(dtcurve)] <- dtcurve$time[(i+1):nrow(dtcurve)] + dtcurve$dt[i]
    }
    
    i <- i + 1
  }
  # Graphic
  abline(h = c(0.791,5*0.791), col = 'red', lty = 3) # pression atmo et a profondeur (saturation max)
  abline(h = Scomp, col = c(1:5), lty = 5) # S limit a ne pas depasser
  dtcurve[375:nrow(dtcurve),-c(5:14) ]
  
# } 

# desat_dtcurve <- add_desat(raw_dtcurve, desat_stop, ascent_speed, secu)
# dtr <- max(desat_dtcurve$time) - tail(raw_dtcurve$time, 2)[1]


# plot de palier obligatoire, 
# TODO : use this idea !!!
plot(dtcurve$max_depth, xlim = c(300, 400), ylim = c(0,40),
     cex = .5 + dtcurve$anarchy, pch = 3)
lines(dtcurve$depth , lty = 3)
lines(dtcurve$nex_pal, lty = 2)
abline(h = 3)


# Explorer le temps qu'il faut pour desat !
# tmp <- dtcurve[dtcurve$need_pal,][rep(1, 70),]
# i = 2
# while(i <= nrow(tmp)){
#   # compute 
#   tmp[i, 5:9] <- (tmp$ppn2[i] - tmp[i-1, 5:9]) *
#     half_life(comp, tmp$dt[i])/100  + tmp[i-1, 5:9]
#   tmp[i, 10:14] <- tmp[i, 5:9] / (tmp$ppn2[i]/0.791) # S = Tn2 / Pabs
#   anar <- tmp[i, 10:14] >= Scomp
#   tmp$anarchy[i] <- any(anar) # is any compartement anar
#   tmp$time[i] <- tmp$time[i-1] + tmp$dt[i]
#   
#   Pabs_pal <- tmp[i, 5:9] / Scomp # Pabs = Tn2 / Sc compart
#   tmp$drive[i] <- which.max(Pabs_pal)
#   tmp$Pabs_pal[i] <- max(Pabs_pal)
#   tmp$max_depth[i] <- (tmp$Pabs_pal[i] - 1) * 10
#   if( tmp$max_depth[i] < 0 | round(tmp$max_depth[i], 2) == 0) tmp$max_depth[i] <- 0
#   # okay on c'est quand il faut un palier et sa profondeur !
#   tmp$nex_pal[i] <- min(depths[(tmp$max_depth[i] - depths) <= 0])
#   
#   # on est a la profondeur d'un palier requis
#   tmp$need_pal[i] <- tmp$depth[i] <= tmp$nex_pal[i]
#   i <- i + 1
# }
# tmp[,-c(5:14)]
# 
# tmp$time[min(which(!tmp$need_pal))]-tmp$time[1]
# # la formule marche bien, on arrive a la Scomp voulue
# 2.73 + (1.028-2.73) * (1- 2^(-3.4/10))
# 
# j = 1

# -10 * (log(1-((2.38-2.73)/(1.028 - 2.73)))/log(2))
# next_stop <- 0
# -comp[tmp$drive[j]] * (log(1-(((Scomp[tmp$drive[j]] * (next_stop / 10 +1) )-
#                                  tmp[j,5:9][tmp$drive[j]])/
#                                 (tmp$ppn2[j] - tmp[j,5:9][tmp$drive[j]])))/
#                          log(2))
