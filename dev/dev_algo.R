# HALDANE
devtools::load_all()
devtools::document()
rm(list = ls())

maj = 0;
altitude = 0;
ppn2 = 0.791;
ncomp = 5
ascent_speed = 10
secu = FALSE

raw_dtcurve <- init_dtcurve(62, 15, 15, "OW")
desat_stop <- desat_haldane(dtcurve = raw_dtcurve, maj = maj,
                            ppn2 = ppn2, ncomp = 5)
dtcurve <- add_desat(raw_dtcurve, desat_stop, ascent_speed, secu)


haldane_Scurve <- function(dtcurve, ncomp, ppn2,
                          cut = NULL){
  # compute Scomp value during the dive
  if(is.null(cut)){
    delta <- 0.1
    cut_h <- FALSE
  }
  dtcurve <- cut_dtcurve(dtcurve, delta = delta, cut_h = cut_h)

  res <- rep(0, ncomp)
  return(res)
}

haldane_Ccurve <- function(dtcurve, ncomp, cut = TRUE){
  # compute Ccomp value during the dive
  res <- rep(0, ncomp)
  return(res)
}

# plot de palier obligatoire, 
# TODO : use this idea !!!
# plot(dtcurve$max_depth, xlim = c(300, 400), ylim = c(0,40),
#      cex = .5 + dtcurve$anarchy, pch = 3)
# lines(dtcurve$depth , lty = 3)
# lines(dtcurve$nex_pal, lty = 2)
# abline(h = 3)
