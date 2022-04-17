# HALDANE
devtools::load_all()
devtools::document()
rm(list = ls())


# compute_desat <- function(dtcurve, Ccurve, Scurve, comp, Scomp){
#   return(dtcurve)
# }


# desat_dtcurve <- add_desat(raw_dtcurve, desat_stop, ascent_speed, secu)
# dtr <- max(desat_dtcurve$time) - tail(raw_dtcurve$time, 2)[1]


# plot de palier obligatoire, 
# TODO : use this idea !!!
# plot(dtcurve$max_depth, xlim = c(300, 400), ylim = c(0,40),
#      cex = .5 + dtcurve$anarchy, pch = 3)
# lines(dtcurve$depth , lty = 3)
# lines(dtcurve$nex_pal, lty = 2)
# abline(h = 3)
