#' ceiling_dec
#' 
#' Ceiling at a specific decimal level
#' 
#' @param x values to round. Must be numeric
#' @param level decimal to round to. Default is 1
#' 
#' @return data.frame
#' 
#' @examples 
#' ceiling_dec(1.259, 2)
#' ceiling_dec(1.9, 0)
#' ceiling_dec(29, -1)
#' ceiling_dec(1.251, 2)
#' ceiling_dec(1.2, 0)
#' ceiling_dec(23, -1)
#' 
#' @details 
#' copied from SO
#' \url{https://stackoverflow.com/questions/35807523/r-decimal-ceiling}
#' 
#' @import checkmate
#' 
#' @export
ceiling_dec <- function(x, level=1) {
  assertNumeric(x)
  assertNumber(level)
  round(x + 5*10^(-level-1), level)
  }

