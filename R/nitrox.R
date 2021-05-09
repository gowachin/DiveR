#' nitrox_maxdepth
#'
#' Maximum depth for partial pressure of nitrogen.
#' 
#' @param ppn2 Partial pressure of nitrogen in bar. Default is 0.791 bar
#' 
#' @return maximum depth in meter
#' 
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @export
nitrox_maxdepth <- function(ppn2 = 0.791){
  if (any(ppn2 < 0) | !is.numeric(ppn2) | any(ppn2 >= 1) ) {
    stop("ppn2 must be positive numeric value between 0 and 1.", 
         call. = interactive())
  }
  MOD =  floor((10 * 1.6 / (1 - ppn2)) - 10)
  
  return(MOD)
}

#' MODcheck
#'
#' Test if the depth is possible with such partial pressure of nitrogen.
#' 
#' @param depth Depth of the dive in meter. Need to be positive values.
#' @param ppn2 Partial pressure of nitrogen in bar. Default is 0.791 bar
#' @param force FALSE by default, if TRUE don't stop the function but 
#' return a TRUE/FALSE value
#' 
#' @return Nothing expect when \code{force} parameter is TRUE. 
#' Is designed to stop code and returns errors
#' 
#'  
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @export
MODcheck <- function(depth, ppn2 = 0.791, force = FALSE){
  if (any(depth < 0) | !is.numeric(depth)) {
    stop("depth must be positive numeric value(s).", call. = interactive())
  }
  if (any(ppn2 < 0) | !is.numeric(ppn2) | any(ppn2 > 1) ) {
    stop("ppn2 must be positive numeric value between 0 and 1.", 
         call. = interactive())
  }
  if( !is.logical(force) | is.na(force) ){
    stop('force must be TRUE or FALSE',
         call. = interactive())
  }
  
  MOD =  nitrox_maxdepth(ppn2)
  
  res <- TRUE
  if (depth >= MOD) {
    if (force) {
      res <- FALSE
    } else {
      stop(paste0("This depth is highter than the maximum depth possible (",
                  MOD, "m)
while using this gas melange."), call. = interactive())
    }
  }
  return(res)
}


#' nitrox_depth
#'
#' Compute equivalent depth when using this nitrox partial pressure
#' 
#' @param depth Depth of the dive in meter. Need to be positive values.
#' @param ppn2 Partial pressure of nitrogen in bar. Default is 0.791 bar
#' 
#' @return The equivalent depth in meter when using this nitrox partial pressure
#' in a tank.
#' 
#'  
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @export
nitrox_depth <- function(depth, ppn2 = 0.791){
  if (any(depth < 0) | !is.numeric(depth)) {
    stop("depth must be positive numeric value(s).", call. = interactive())
  }
  if (any(ppn2 < 0) | !is.numeric(ppn2) | any(ppn2 > 1) ) {
    stop("ppn2 must be positive numeric value(s) between 0 and 1.", 
         call. = interactive())
  }
  # check maxdepth for ppn2
  MODcheck(depth, ppn2)
  
  PMU =  ((depth+10) * ppn2/ 0.791) -10
  return(PMU)
}
