#' @import plot.matrix
#' @import graphics
NULL

#' mn90
#'
#' A package to code some basic games on R
#'
#' @docType package
#' @name mn90
NULL

#' dive
#' 
#' @param depth in meter
#' @param time in minute
#' @param secu true by default, secu deco 3 min at 3 meter
#' @param vup 10 m/min by default
#' @param maj 0 by default
#' @param hour NULL not implemented yet
#' 
#' @examples 
#' dive = dive(depth = 39, time = 22, secu = TRUE, vup = 10)
#' 
#' @return dive, a dive class object.
#' 
#' @export
dive <- function(depth = 20, time = 40, secu = TRUE, vup = 10, maj = 0, hour = NULL){
  # depth = 39; time = 22; secu = TRUE; vup = 10
  if(maj > 0) time = time + maj
  if(!is.null(hour)){
    print("not implemented yet")
  }
  # check for values
  tablecheck(depth,time)
  
  # get the palier from the table
  palier <- palier(depth, time, secu)
  
  # compute the dtr from palier and depth in square profile
  dtr <- dtr(palier, depth = depth, vup = vup)
  
  # dive curve
  curve <- curve(time = time, depth = depth, palier = palier, vup = vup)[-3]
  
  # hour <- c()
  
  dive <- list(curve = curve, dtr = dtr, palier = palier)
  class(dive) = "dive"
  return(dive)
}
