# HALDANE
devtools::load_all()
rm(list = ls())

# # testing the demi-life formula
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


# compute_desat <- function(dtcurve, Ccurve, Scurve, comp, Scomp){
#   return(dtcurve)
# }


#' desat_haldane
#' 
#' @param dtcurve
#' @param maj
#' @param altitude
#' @param ppn2
#' @param ncomp
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
# desat_haldane <- function(dtcurve, maj = 0, altitude = 0, ppn2 = 0.791, ncomp = 5){
  #### IDIOT PROOF ####
  # TODO : copy from desat_table

# done before
## parameters
depth <- 60
time <- 25
secu <- FALSE
ascent_speed <- 10
desat_model <- 'Haldane'
hour <- 0
gas <- 'AIR'
way <- 'OW'
raw_dtcurve <- init_dtcurve(depth, time, ascent_speed, way)
# EOF done before

  dtcurve <- cut.dtcurve(dtcurve = raw_dtcurve, delta = .1, cut_h = FALSE)
  dtcurve
  maj = 0               #
  altitude = 0          #
  ppn2 = 0.791          #
  ncomp = 12             #
  bpal_speed <- 6 # speed between deco stops
  
  # adding cols for haldane
  dtcurve <- cbind(dtcurve, dt = c(NA, diff(dtcurve$time)),
                   ppn2 = round((dtcurve$depth/10 + 1) * ppn2,3), 
                   anarchy = FALSE, drive = 0, Pabs_pal = 0, max_depth = 0,
                   nex_pal = 0, need_pal = FALSE, time_pal = 0
  )
  
  Haldane <- switch(as.character(ncomp),
                    "5" = DiveR::Haldane5,
                    "12" = DiveR::Haldane12,
                    DiveR::Haldane5
  )
  comp <- Haldane$comp
  Scomp <- Haldane$Scomp
  depths <- c(0, 3, 6, 9)
  
  dtcurve <- cpp_haldane_desat(dtcurve, comp, Scomp, depths)
  save(dtcurve, file = "dev/Cdtcurve.RData")
  
  # plot(dtcurve$time, -dtcurve$depth, type = "l", ylim = c(-15, 0))
  # lines(dtcurve$time, -dtcurve$max_depth, col = "red")
  # abline(h= -c(9,6,3), lty = 3)
  
  # extract the pal time to compare mn90
  # pal <- depths
  # names(pal) <- paste0("m",pal)
  # for(d in seq_along(depths)){
  #   pal[d] <- sum(dtcurve$time_pal[dtcurve$depth ==depths[d]])
  # }
  # print(round(pal[-1], 2))
  # print(DiveR::table[as.character(depth),as.character(time),])
  # 
  # desat <- list(
  #   desat_stop = data.frame(
  #     depth = depths[-1], 
  #     time = pal[-1], 
  #     hour = rep(NA, 3)
  #   ),
  #   group = "Z", model = paste0("Hal",ncomp)
  #   # TODO : add last saturation values and ncomp in ?
  # )
  # 
  
  
  # desat_dtcurve <- add_desat(raw_dtcurve, desat_stop, ascent_speed, secu)
  # dtr <- max(desat_dtcurve$time) - tail(raw_dtcurve$time, 2)[1]
  
  
  # plot de palier obligatoire, 
  # TODO : use this idea !!!
  # plot(dtcurve$max_depth, xlim = c(300, 400), ylim = c(0,40),
  #      cex = .5 + dtcurve$anarchy, pch = 3)
  # lines(dtcurve$depth , lty = 3)
  # lines(dtcurve$nex_pal, lty = 2)
  # abline(h = 3)
  
  
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
  