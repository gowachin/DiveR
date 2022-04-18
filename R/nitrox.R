#' nitrox_maxdepth
#'
#' Maximum depth for partial pressure of dioxygen.
#' 
#' @param ppo2 Partial pressure of dioxygen in bar. Default is 0.791 bar
#' @param MOD maximum operating depth. This is the maximal ppo2 value in bar
#' default is 1.6
#' 
#' @examples 
#' nitrox_maxdepth()
#' nitrox_maxdepth(0.3)
#' nitrox_maxdepth(1)
#' 
#' @return maximum depth in meter
#' 
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @import checkmate
#' 
#' @export
nitrox_maxdepth <- function(ppo2 = 0.209, MOD = 1.6){
  assertNumber(ppo2, lower = 0, upper = 1)
  assertNumber(MOD, lower = 0, upper = 1.6)
  
  MOD =  floor((10 * MOD / ppo2) - 10)
  
  return(MOD)
}

# TODO nitrox_bestmix <- function(depth, MOD = 1.6){
  # res <- MOD / (depth / 10 + 1)
# }

#' MODcheck
#'
#' Test if the depth is possible with such partial pressure of dioxygen.
#' 
#' @param depth Depth of the dive in meter. Need to be positive values.
#' @param ppo2 Partial pressure of dioxygen in bar. Default is 0.791 bar
#' @param force FALSE by default, if TRUE don't stop the function but 
#' return a TRUE/FALSE value
#' @param MOD maximum operating depth. This is the maximal ppo2 value in bar
#' default is 1.6
#' 
#' @return Nothing expect when \code{force} parameter is TRUE. 
#' Is designed to stop code and returns errors
#' 
#' @examples 
#' MODcheck(70, ppo2 = 0.209, force = TRUE)
#' MODcheck(65, ppo2 = 0.209, force = TRUE)
#' MODcheck(30, ppo2 = 0.3, MOD = 1.4)
#'  
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @import checkmate
#' 
#' @export
MODcheck <- function(depth, ppo2 = 0.209, force = FALSE, MOD = 1.6){

  assertNumber(depth, lower = 0)
  assertNumber(ppo2, lower = 0, upper = 1)
  assertLogical(force, any.missing = FALSE)
  assertNumber(MOD, lower = 0, upper = 1.6)
  
  MOD =  nitrox_maxdepth(ppo2, MOD)
  
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
#' @param ppo2 Partial pressure of dioxygen in bar. Default is 0.209 bar
#' @param MOD maximum operating depth. This is the maximal ppo2 value in bar
#' default is 1.6
#' 
#' @return The equivalent depth in meter when using this nitrox partial pressure
#' in a tank.
#' 
#'  
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#' 
#' @export
nitrox_depth <- function(depth, ppo2 = 0.209, MOD = 1.6){
  assertNumber(depth, lower = 0)
  assertNumber(ppo2, lower = 0, upper = 1)
  assertNumber(MOD, lower = 0, upper = 1.6)
  
  # check maxdepth for ppo2
  MODcheck(depth, ppo2, force = FALSE,MOD)
  
  PMU =  ((depth+10) * (1 - ppo2)/ 0.791) -10
  return(PMU)
}
